import os
from typing import Dict, List, Optional

import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from scipy.optimize import curve_fit

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test


# ==== Costanti ====
INTERVAL = 10
LOOP_SPACING = 1000
MAGNITUDE = -3


# ==== Modelli ====

class HysteresisLoop(BaseModel):
    n_cycles: List[float]
    strain: List[float]
    stress: List[float]


class FatigueTest(BaseModel):
    specimen_id: int
    specimen_name: str
    total_dissipated_energy: Optional[float] = None
    run_out: bool
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]
    stress_at_failure: Optional[float] = None
    strain_at_failure: Optional[float] = None
    n_fail: int
    warning_messages: bool
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    crack_n_cycles: List[float]
    da_dN: Optional[List[float]] = None
    G_MBT: Optional[List[float]] = None


# ==== Funzioni utili ====

def get_dataframe(data_in: str, exp: Dict[str, str], specimen_id: int) -> DataFrame:
    researcher_name = exp["researcher"].split(" ")[-1]
    folder_name = (
        f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}"
        if data_in == "HYS"
        else f"{data_in}_{researcher_name}_{exp['date']}_{exp['experiment_type']}"
    )
    filename = (
        f"{data_in}_measure_{specimen_id:03d}.csv"
        if data_in == "HYS"
        else f"measure_{specimen_id:03d}.csv"
    )
    filepath = os.path.join(DATA_DIRECTORY, folder_name, filename)
    return pd.read_csv(os.path.abspath(filepath))


def get_loops_dataframe(experiment: Dict[str, str], specimen_id: int) -> Optional[DataFrame]:
    researcher_name = experiment["researcher"].split(" ")[-1]
    folder_name = f"TST_{researcher_name}_{experiment['date']}_{experiment['experiment_type']}"
    file_path = os.path.join(DATA_DIRECTORY, folder_name, f"Loops_measure_{specimen_id:03d}.csv")
    return pd.read_csv(file_path) if os.path.exists(file_path) else None


def extract_hysteresis_loops_from_file(df: pd.DataFrame) -> List[HysteresisLoop]:
    loops = []
    for n_cycle, group in df.groupby("n_cycles"):
        sorted_group = group.sort_values("point_index")
        loops.append(HysteresisLoop(
            n_cycles=[n_cycle] * len(sorted_group),
            strain=sorted_group["fit_x"].tolist(),
            stress=sorted_group["fit_y"].tolist()
        ))
    return loops


def compute_sub_indexes(df: DataFrame) -> List[int]:
    unique_n_cycles = np.unique(df["n_cycles"])
    indexes = np.linspace(0, len(unique_n_cycles) - 1, 10).astype(int)
    return unique_n_cycles[indexes]


def fatigue_processing(df: DataFrame, sub_indexes: List[int], test_meta) -> Dict:
    sub_hystloops = []
    df = df.assign(
        stress=df["Load"] / (test_meta["width"] * test_meta["thickness"]),
        strain=df["exx"]
    )
    n_fail = int(df["N_cycles"].max())

    for sub_index in sub_indexes:
        mask = df["N_cycles"] == sub_index
        if not mask.any():
            continue

        sub_df = df[mask]
        stress = np.append(sub_df["stress"].to_numpy(), sub_df["stress"].iloc[0])
        strain = np.append(sub_df["strain"].to_numpy(), sub_df["strain"].iloc[0])
        cycles = np.append(sub_df["N_cycles"].to_numpy(), sub_df["N_cycles"].iloc[0])

        sub_hystloops.append(HysteresisLoop(
            n_cycles=cycles.tolist(),
            stress=stress.tolist(),
            strain=strain.tolist()
        ))

    return {
        "sub_hystloops": sub_hystloops,
        "n_fail": n_fail
    }


def smooth_spikes(data: List[float], threshold: float = 0.05) -> List[float]:
    smoothed = data.copy()
    for i in range(2, len(data) - 2):
        v0, v1, v2, v3, v4 = data[i - 2:i + 3]
        m1, m2 = (v0 + v1) / 2, (v3 + v4) / 2
        avg = (m1 + m2) / 2
        if avg == 0:
            continue
        if abs(m1 - m2) / avg < threshold and abs(v2 - avg) / avg > threshold:
            smoothed[i] = avg
    return smoothed


def apply_spike_filter(df: DataFrame, field: str, specimen_name: str) -> (List[float], bool):
    original = df[field].tolist()
    filtered = smooth_spikes(original)
    changed = original != filtered
    if changed:
        print(f"[Spike Filter] '{specimen_name}' – '{field}' modified in {sum(o != f for o, f in zip(original, filtered))} points")
    return filtered, changed


# ==== Funzione principale ====

async def fatigue_test(session: AsyncSession, experiment_id: int, test_id: int) -> FatigueTest:
    experiment = (await session.execute(
        select(
            Experiment.laboratory,
            Experiment.researcher,
            Experiment.experiment_type,
            Experiment.date,
            Experiment.fa_experiment_type,
        ).where(Experiment.id == experiment_id))
    ).one()._asdict()

    test_meta = await get_test_fields(session, experiment_id, test_id, (
        Test.sequential_number,
        Test.specimen_name,
        Test.run_out,
        Test.number_of_cycles,
        Test.width,
        Test.thickness,
        Test.length,
        Test.maximum_load,
        Test.t, 
        Test.l_prime,
    ))

    specimen_id = test_meta["sequential_number"]
    specimen_name = test_meta.get("specimen_name") or specimen_id
    is_fracture = experiment.get("fa_experiment_type") == "fracture"
    warning_triggered = False

    std_df = get_dataframe("TST", experiment, specimen_id)

    if is_fracture:
        crack_load = std_df["Load"].tolist() if "Load" in std_df else []
        crack_load = (np.array(crack_load) / 1000).tolist()  # Convert to kN if needed
        crack_displacement = std_df["u"].tolist() if "u" in std_df else []
        crack_length = std_df["Crack_length"].tolist() if "Crack_length" in std_df else []
        crack_n_cycles = std_df["N_cycles"].tolist() if "N_cycles" in std_df else []

        w = test_meta["width"]
        h = test_meta["thickness"]
        t = test_meta["t"] or 0.0
        l_prime = test_meta["l_prime"] or 0.0

        # Funzioni di utilità
        def fit_func(x, a, b):
            return a * x + b

        def fit_crack_length_from_compliance(
            crack_length: List[float],
            crack_displacement: List[float],
            crack_load: List[float]
        ) -> List[float]:
            from scipy.optimize import curve_fit
            import numpy as np

            compliance = np.array(crack_displacement) / np.array(crack_load)
            crack_length_array = np.array(crack_length)

            def power_law(x, a, m):
                return a * x ** m

            popt, _ = curve_fit(power_law, crack_length_array, compliance)
            a_fit, m_fit = popt

            crack_length_fitted = (compliance / a_fit) ** (1 / m_fit)
            return crack_length_fitted.tolist()

        def compute_compliance_and_crack_length_fitted(
            crack_displacement: List[float],
            crack_load: List[float],
            crack_length: List[float]
        ) -> (np.ndarray, np.ndarray):
            compliance = np.array(crack_displacement) / np.array(crack_load)
            crack_length_fitted = np.array(
                fit_crack_length_from_compliance(crack_length, crack_displacement, crack_load)
            )
            return compliance, crack_length_fitted

        def compute_factors(
            crack_displacement: List[float],
            crack_length_fitted: np.ndarray,
            t: float,
            l_prime: float
        ):
            F = (
                1
                - 3/10 * (np.array(crack_displacement) / crack_length_fitted)**2
                - 3/2 * ((np.array(crack_displacement) * t) / crack_length_fitted**2)
            )
            N = (
                1
                - (l_prime / crack_length_fitted)**3
                - 9/8 * (1 - (l_prime / crack_length_fitted)**2) * (np.array(crack_displacement) * t) / crack_length_fitted**2
                - 9/35 * (np.array(crack_displacement) / crack_length_fitted)**2
            )
            return F, N

        def compute_g_mbt(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w
        ):
            C_N_1_3 = (compliance / N)**(1/3)
            params, _ = curve_fit(fit_func, crack_length_fitted, C_N_1_3)
            a, b = params
            triangle = abs(b / a)

            G = (
                (3 * np.array(crack_load) * np.array(crack_displacement))
                / (2 * w * (crack_length_fitted + triangle))
                * F / N
                * 1e6
            )
            return G.tolist()

        def compute_da_dn(crack_length_fitted, crack_n_cycles):
            da_dn_new = []
            a_estimated = []

            # Punto iniziale
            a_0 = 0.5 * (crack_length_fitted[1] + crack_length_fitted[0])
            a_estimated.append(a_0)
            da_dn_0 = (
                (crack_length_fitted[1] - crack_length_fitted[0])
                / (crack_n_cycles[1] - crack_n_cycles[0])
            )
            da_dn_new.append(da_dn_0)

            def poly_func(x, b0, b1, b2):
                return b0 + b1 * x + b2 * x**2

            for i in range(3, len(crack_length_fitted) - 3):
                C1 = 0.5 * (crack_n_cycles[i - 3] + crack_n_cycles[i + 3])
                C2 = 0.5 * (crack_n_cycles[i + 3] - crack_n_cycles[i - 3])
                x_vals = (np.array(crack_n_cycles[i - 3:i + 4]) - C1) / C2
                y_vals = crack_length_fitted[i - 3:i + 4]
                popt, _ = curve_fit(poly_func, x_vals, y_vals)
                a_est = poly_func(x_vals[3], *popt)
                a_estimated.append(a_est)
                da_dn = popt[1] / C2 + (2 * popt[2] * (crack_n_cycles[i] - C1)) / C2**2
                da_dn_new.append(da_dn)

            for i in range(len(crack_length_fitted) - 3, len(crack_length_fitted)):
                if i == len(crack_length_fitted) - 1:
                    a_est = 0.5 * (crack_length_fitted[-2] + crack_length_fitted[-1])
                    da_dn = (
                        (crack_length_fitted[-1] - crack_length_fitted[-2])
                        / (crack_n_cycles[-1] - crack_n_cycles[-2])
                    )
                else:
                    a_est = crack_length_fitted[i]
                    da_dn = (
                        (crack_length_fitted[i + 1] - crack_length_fitted[i - 1])
                        / (crack_n_cycles[i + 1] - crack_n_cycles[i - 1])
                    )
                a_estimated.append(a_est)
                da_dn_new.append(da_dn)

            return da_dn_new

        # Calcolo dei valori comuni
        compliance, crack_length_fitted = compute_compliance_and_crack_length_fitted(
            crack_displacement, crack_load, crack_length
        )
        F, N = compute_factors(crack_displacement, crack_length_fitted, t, l_prime)
        crack_load_kN = np.array(crack_load)  # già in kN

        G_MBT = compute_g_mbt(
            compliance, crack_displacement, crack_load_kN, crack_length_fitted, F, N, w
        )
        da_dn_new = compute_da_dn(crack_length_fitted, crack_n_cycles)

        # Costruzione del FatigueTest
        return FatigueTest(
            specimen_id=specimen_id,
            specimen_name=specimen_name,
            run_out=test_meta["run_out"],
            hysteresis_loops=[],
            n_cycles=crack_n_cycles,
            creep=[],
            hysteresis_area=[],
            stiffness=[],
            total_dissipated_energy=None,
            stress_at_failure=None,
            strain_at_failure=None,
            n_fail=test_meta["number_of_cycles"],
            warning_messages=False,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length_fitted.tolist(),
            crack_n_cycles=crack_n_cycles,
            da_dN=da_dn_new,
            G_MBT=G_MBT,
        )


    hyst_df = get_dataframe("HYS", experiment, specimen_id).fillna(0)

    for field in ["creep", "hysteresis_area", "stiffness"]:
        filtered, changed = apply_spike_filter(hyst_df, field, specimen_name)
        hyst_df[field] = filtered
        warning_triggered = warning_triggered or changed

    loops_df = get_loops_dataframe(experiment, specimen_id)

    if loops_df is not None:
        loops = extract_hysteresis_loops_from_file(loops_df)
        n_fail = test_meta["number_of_cycles"]
    else:
        result = fatigue_processing(std_df, compute_sub_indexes(hyst_df), test_meta)
        loops = result["sub_hystloops"]
        n_fail = result["n_fail"]

    max_stress = test_meta["maximum_load"] / (test_meta["width"] * test_meta["thickness"])

    return FatigueTest(
        specimen_id=specimen_id,
        specimen_name=specimen_name,
        run_out=test_meta["run_out"],
        hysteresis_loops=loops,
        n_cycles=hyst_df["n_cycles"].tolist(),
        creep=hyst_df["creep"].tolist(),
        hysteresis_area=hyst_df["hysteresis_area"].tolist(),
        stiffness=hyst_df["stiffness"].tolist(),
        total_dissipated_energy=np.sum(hyst_df["hysteresis_area"]),
        stress_at_failure=max_stress,
        strain_at_failure=hyst_df["creep"].iloc[-1],
        n_fail=n_fail,
        warning_messages=warning_triggered,
        crack_displacement=[],
        crack_load=[],
        crack_length=[],
        crack_n_cycles=[], 
        da_dN=[],
        G_MBT=[],
    )

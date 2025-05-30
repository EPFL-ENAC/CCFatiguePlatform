import os
from typing import Dict, List, Optional

import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

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
    ))

    specimen_id = test_meta["sequential_number"]
    specimen_name = test_meta.get("specimen_name") or specimen_id
    is_fracture = experiment.get("fa_experiment_type") == "fracture"
    warning_triggered = False

    std_df = get_dataframe("TST", experiment, specimen_id)

    if is_fracture:
        crack_load = std_df["Load"].tolist() if "Load" in std_df else []
        crack_displacement = std_df["u"].tolist() if "u" in std_df else []
        #crack_displacement = [u - crack_displacement[0] for u in crack_displacement] if crack_displacement else []
        crack_length = std_df["Crack_length"].tolist() if "Crack_length" in std_df else []
        crack_n_cycles = std_df["N_cycles"].tolist() if "N_cycles" in std_df else []

        # Parametri geometrici
        w = test_meta["width"]
        h = test_meta["thickness"]
        t = h / 4 + 22
        l_prime = 0

        # Calcoli preliminari
        std_df["Force_kN"] = std_df["Load"] * 1e-3
        std_df["compliance"] = std_df["u"] / std_df["Force_kN"]

        # F factor
        std_df["F"] = (
            1
            - 3/10 * (std_df["u"] / std_df["Crack_length"])**2
            - 3/2 * ((std_df["u"] * t) / (std_df["Crack_length"])**2)
        )

        # N factor
        std_df["N"] = (
            1
            - (l_prime / std_df["Crack_length"])**3
            - 9/8 * (1 - (l_prime / std_df["Crack_length"])**2) * (std_df["u"] * t) / (std_df["Crack_length"]**2)
            - 9/35 * (std_df["u"] / std_df["Crack_length"])**2
        )

        # MBT triangle fitting
        from scipy.optimize import curve_fit

        def fit_func(x, a, b):
            return a * x + b

        x_fit = std_df["Crack_length"]
        y_fit = (std_df["compliance"] / std_df["N"])**(1/3)
        # Filtra eventuali NaN o Inf
        x_fit = np.array(x_fit)
        y_fit = np.array(y_fit)
        mask = np.isfinite(x_fit) & np.isfinite(y_fit)
        x_fit = x_fit[mask]
        y_fit = y_fit[mask]

        # Verifica se ci sono dati sufficienti
        if len(y_fit) == 0 or len(x_fit) == 0:
            # Lancia un errore o logga un warning
            raise ValueError("Nessun dato valido per curve_fit.")

        # Se OK, procedi al fit
        params, _ = curve_fit(fit_func, x_fit, y_fit)
        a_param, b_param = params
        triangle = abs(b_param / a_param)

        # G_MBT
        std_df["G_MBT"] = (
            (3 * std_df["Force_kN"] * std_df["u"])
            / (2 * w * (std_df["Crack_length"] + triangle))
            * std_df["F"] / std_df["N"]
            * 1e6
        )

        # Calcolo da/dN (metodo polinomiale)
        def func(x, b0, b1, b2):
            return b0 + b1 * x + b2 * x**2

        da_dn_new = []
        a_estimated = []

        # Punto iniziale
        a_0 = 0.5 * (std_df["Crack_length"].iloc[1] + std_df["Crack_length"].iloc[0])
        a_estimated.append(a_0)
        da_dn_0 = (
            (std_df["Crack_length"].iloc[1] - std_df["Crack_length"].iloc[0])
            / (std_df["N_cycles"].iloc[1] - std_df["N_cycles"].iloc[0])
        )
        da_dn_new.append(da_dn_0)

        # Punti intermedi con finestra mobile di 7 dati
        for i in range(3, len(std_df) - 3):
            C1 = 0.5 * (std_df["N_cycles"].iloc[i - 3] + std_df["N_cycles"].iloc[i + 3])
            C2 = 0.5 * (std_df["N_cycles"].iloc[i + 3] - std_df["N_cycles"].iloc[i - 3])
            x_vals = (std_df["N_cycles"].iloc[i - 3:i + 4] - C1) / C2
            y_vals = std_df["Crack_length"].iloc[i - 3:i + 4]
            popt, _ = curve_fit(func, x_vals, y_vals)
            a_est = func(x_vals.iloc[3], *popt)
            a_estimated.append(a_est)
            da_dn = popt[1] / C2 + (2 * popt[2] * (std_df["N_cycles"].iloc[i] - C1)) / C2**2
            da_dn_new.append(da_dn)

        # Ultimi punti (usa differenze finite semplici)
        for i in range(len(std_df) - 3, len(std_df)):
            if i == len(std_df) - 1:
                a_est = 0.5 * (std_df["Crack_length"].iloc[-2] + std_df["Crack_length"].iloc[-1])
                da_dn = (
                    (std_df["Crack_length"].iloc[-1] - std_df["Crack_length"].iloc[-2])
                    / (std_df["N_cycles"].iloc[-1] - std_df["N_cycles"].iloc[-2])
                )
            else:
                a_est = std_df["Crack_length"].iloc[i]
                da_dn = (
                    (std_df["Crack_length"].iloc[i + 1] - std_df["Crack_length"].iloc[i - 1])
                    / (std_df["N_cycles"].iloc[i + 1] - std_df["N_cycles"].iloc[i - 1])
                )
            a_estimated.append(a_est)
            da_dn_new.append(da_dn)

        # Restituzione del FatigueTest (senza da_dn_new o G_MBT inclusi direttamente)
        return FatigueTest(
            specimen_id=specimen_id,
            specimen_name=specimen_name,
            run_out=test_meta["run_out"],
            hysteresis_loops=[],
            n_cycles=std_df["N_cycles"].tolist(),
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
            crack_length=crack_length,
            crack_n_cycles=crack_n_cycles,
            da_dN = da_dn_new,
            G_MBT=std_df["G_MBT"].tolist(),
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

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
from sklearn.linear_model import LinearRegression
import math


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
    n_fail: int | None
    warning_messages: bool
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    crack_n_cycles: List[float]
    da_dN: Optional[List[float]] = None
    G_MBT: Optional[List[float]] = None
    G_MCC: Optional[List[float]] = None
    G_ECM: Optional[List[float]] = None
    c_value_paris: float | None 
    m_value_paris: float | None


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

        # Utility functions
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

        def compute_g_mcc(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width, thickness):
            a_over_h = crack_length_fitted / thickness
            C_N_1_3 = (compliance / N)**(1/3)
            A1, _ = np.polyfit(C_N_1_3, a_over_h, 1)
            G = (3 * np.array(crack_load)**2 * (compliance / N)**(2/3)) / (2 * A1 * width * thickness) * F * 1e6
            return G.tolist()

        def compute_g_ecm(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width):
            crack_array = crack_length_fitted
            c_over_n = compliance / N

            valid_mask = (crack_array > 0) & (c_over_n > 0) & np.isfinite(c_over_n)
            if np.sum(valid_mask) < 2:
                return [float("nan")] * len(crack_array)

            a_log = np.log10(crack_array[valid_mask])
            log_C_N = np.log10(c_over_n[valid_mask])

            m, _ = np.polyfit(a_log, log_C_N, 1)

            G = (m * np.array(crack_load) * np.array(crack_displacement)) / (2 * width * crack_array) * F / N * 1e6
            return G.tolist()

        def compute_da_dn(crack_length_fitted, crack_n_cycles):
            """
            Computes da/dN exactly following the sequence:
            i=0, i=1, i=2, central loop (i=3..N-4), i=N-3, i=N-2, i=N-1
            as in the "manual" block, but using two arrays:
            - crack_length_fitted: list or array of fitted crack lengths
            - crack_n_cycles:      list or array of corresponding cycle numbers

            Returns:
            - da_dn_new: list of da/dN values calculated point by point
            """
            def poly_func(x, b0, b1, b2):
                return b0 + b1 * x + b2 * x**2

            da_dn_new = []
            a_estimated = []

            
            a_arr = np.array(crack_length_fitted)
            N_arr = np.array(crack_n_cycles)
            N = len(a_arr)

            
            a_0 = 0.5 * (a_arr[1] + a_arr[0])
            a_estimated.append(a_0)
            da_dn_0 = (a_arr[1] - a_arr[0]) / (N_arr[1] - N_arr[0])
            da_dn_new.append(da_dn_0)

            
            C1 = 0.5 * (N_arr[0] + N_arr[2])
            C2 = 0.5 * (N_arr[2] - N_arr[0])
            # x_vals and y_vals for i=1
            x_vals = (N_arr[0:3] - C1) / C2
            y_vals = a_arr[0:3]
            popt, _ = curve_fit(poly_func, x_vals, y_vals)
            # estimate a_1 at the central point (index 1 of block 0:3)
            xi = x_vals[1]
            a_1_estimated = popt[0] + popt[1] * xi + popt[2] * xi**2
            a_estimated.append(a_1_estimated)
            da_dn_1 = popt[1] / C2 + (2 * popt[2] * (N_arr[1] - C1)) / (C2**2)
            da_dn_new.append(da_dn_1)

            # --- Point i = 2 (window on indices 0..4) ---
            C1 = 0.5 * (N_arr[0] + N_arr[4])
            C2 = 0.5 * (N_arr[4] - N_arr[0])
            x_vals = (N_arr[0:5] - C1) / C2
            y_vals = a_arr[0:5]
            popt2, _ = curve_fit(poly_func, x_vals, y_vals)
            xi = x_vals[2]
            a_2_estimated = popt2[0] + popt2[1] * xi + popt2[2] * xi**2
            a_estimated.append(a_2_estimated)
            da_dn_2 = popt2[1] / C2 + (2 * popt2[2] * (N_arr[2] - C1)) / (C2**2)
            da_dn_new.append(da_dn_2)

            # --- Main loop i = 3 .. N-4 (window on 7 points) ---
            for i in range(3, N - 3):
                C1 = 0.5 * (N_arr[i - 3] + N_arr[i + 3])
                C2 = 0.5 * (N_arr[i + 3] - N_arr[i - 3])
                x_vals = (N_arr[i - 3 : i + 4] - C1) / C2
                y_vals = a_arr[i - 3 : i + 4]
                popt3, _ = curve_fit(poly_func, x_vals, y_vals)
                # estimate a_i at the central point (index 3 of block i-3:i+4)
                xi = x_vals[3]
                a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
                a_estimated.append(a_3_estimated)
                da_dn_3 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
                da_dn_new.append(da_dn_3)

            # --- Point i = N-3 (window on indices N-5..N-1) ---
            i = N - 3
            C1 = 0.5 * (N_arr[i - 2] + N_arr[i + 2])
            C2 = 0.5 * (N_arr[i + 2] - N_arr[i - 2])
            x_vals = (N_arr[i - 2 : i + 3] - C1) / C2
            y_vals = a_arr[i - 2 : i + 3]
            popt3, _ = curve_fit(poly_func, x_vals, y_vals)
            xi = x_vals[2]  # corresponds to the central index i
            a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
            a_estimated.append(a_3_estimated)
            da_dn_4 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
            da_dn_new.append(da_dn_4)

            # --- Point i = N-2 (window on indices N-3..N-1) ---
            i = N - 2
            C1 = 0.5 * (N_arr[i - 1] + N_arr[i + 1])
            C2 = 0.5 * (N_arr[i + 1] - N_arr[i - 1])
            x_vals = (N_arr[i - 1 : i + 2] - C1) / C2
            y_vals = a_arr[i - 1 : i + 2]
            popt3, _ = curve_fit(poly_func, x_vals, y_vals)
            xi = x_vals[1]  # central index of the 3-point block
            a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
            a_estimated.append(a_3_estimated)
            da_dn_5 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
            da_dn_new.append(da_dn_5)

            # --- Point i = N-1 (last point, simple derivative on two values) ---
            i = N - 1
            a_last = 0.5 * (a_arr[N - 2] + a_arr[N - 1])
            a_estimated.append(a_last)
            da_dn_last = (a_arr[N - 1] - a_arr[N - 2]) / (N_arr[N - 1] - N_arr[N - 2])
            da_dn_new.append(da_dn_last)

            return da_dn_new


        def find_best_paris_fit(
            G_MBT,
            da_dn,
            min_window_size=3,
            r2_threshold=0.98,
            da_dn_min=None,
            da_dn_max=None
        ):

            da_dn = np.array(da_dn)


            print(f"Len G_MBT: {len(G_MBT)}, len da_dn: {len(da_dn)}")
            log_G = np.log(G_MBT)
            log_da_dn = np.log(da_dn)

            if da_dn_min is not None and da_dn_max is not None:
                print("USING SELECTED WINDOW")
                # Filter the window based on da/dN values
                mask = (da_dn >= da_dn_min) & (da_dn <= da_dn_max)
                filtered_log_G = log_G[mask]
                filtered_log_da_dn = log_da_dn[mask]
                
                if len(filtered_log_G) < min_window_size:
                    raise ValueError(f"The selected window ({len(filtered_log_G)} points) is too small for fitting.")
                
                x = filtered_log_G.reshape(-1, 1)
                y = filtered_log_da_dn
                reg = LinearRegression().fit(x, y)
                r2 = reg.score(x, y)
                m = reg.coef_[0]
                logC = reg.intercept_
                C = math.exp(logC)
                return m, C, r2

            else:
                # Use automatic logic as fallback
                best_r2 = -np.inf
                best_start = None
                best_end = None
                best_params = None

                for start in range(len(log_G) - min_window_size + 1):
                    for end in range(start + min_window_size - 1, len(log_G)):
                        x = log_G[start:end+1].reshape(-1, 1)
                        y = log_da_dn[start:end+1]
                        reg = LinearRegression().fit(x, y)
                        r2 = reg.score(x, y)
                        if r2 > best_r2:
                            best_r2 = r2
                            best_start = start
                            best_end = end
                            best_params = (reg.coef_[0], reg.intercept_)

                # Expand the window as long as R² remains high
                expand = True
                while expand:
                    expanded = False
                    if best_start > 0:
                        new_start = best_start - 1
                        x = log_G[new_start:best_end+1].reshape(-1, 1)
                        y = log_da_dn[new_start:best_end+1]
                        reg = LinearRegression().fit(x, y)
                        r2 = reg.score(x, y)
                        if r2 >= r2_threshold:
                            best_start = new_start
                            best_params = (reg.coef_[0], reg.intercept_)
                            best_r2 = r2
                            expanded = True
                    if best_end < len(log_G) - 1:
                        new_end = best_end + 1
                        x = log_G[best_start:new_end+1].reshape(-1, 1)
                        y = log_da_dn[best_start:new_end+1]
                        reg = LinearRegression().fit(x, y)
                        r2 = reg.score(x, y)
                        if r2 >= r2_threshold:
                            best_end = new_end
                            best_params = (reg.coef_[0], reg.intercept_)
                            best_r2 = r2
                            expanded = True
                    if not expanded:
                        expand = False

                m, logC = best_params
                C = math.exp(logC)
                print(f"Paris Law fit: m = {m:.4f}, C = {C:.4e}, R² = {best_r2:.4f}")
                da_dn_window = da_dn[best_start:best_end+1]
                print(f"da/dN window: {da_dn_window}")

                return m, C, best_r2

        # Compute common values
        compliance, crack_length_fitted = compute_compliance_and_crack_length_fitted(
            crack_displacement, crack_load, crack_length
        )

        F, N = compute_factors(crack_displacement, crack_length_fitted, t, l_prime)

        G_MBT = compute_g_mbt(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w
        )

        G_MCC = compute_g_mcc(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w, h
        )

        G_ECM = compute_g_ecm(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w
        )

        da_dn_new = compute_da_dn(crack_length_fitted, crack_n_cycles)

        m, C, r2_best = find_best_paris_fit(G_MBT, da_dn_new, da_dn_min=0.00001, da_dn_max=0.001)

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
            G_MCC=G_MCC,
            G_ECM=G_ECM,
            m_value_paris=m,
            c_value_paris=C,
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
        G_MCC=[],
        G_ECM=[],
        c_value_paris=None,
        m_value_paris=None
    )

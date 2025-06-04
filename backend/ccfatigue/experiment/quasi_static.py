import os
from re import Pattern, search
from typing import Callable, Dict, List, Any
import numpy as np
from scipy.optimize import curve_fit

import pandas as pd
from pandas import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test


class QuasiStaticTest(BaseModel):
    specimen_name: str
    specimen_id: int
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    displacement: Dict[str, List[float]]
    load: Dict[str, List[float]]
    strain: Dict[str, List[float]]
    stress: Dict[str, List[float]]
    toughness: float | None  # new feature
    initial_crack_length: float | None # new feature
    young_modulus: float | None # new feature
    poisson_ratio: float | None # new feature
    crack_fractureenergy_mbt: List[float]
    crack_fractureenergy_mcc: List[float]
    crack_fractureenergy_ecm: List[float]
    g_init_mbt: float | None 
    bridginglength_mbt: float | None 
    g_plateau_mbt: float | None 


def get_dataframe(
    exp: Dict[str, str],
    specimen_id: int,
) -> DataFrame:
    researcher_name = exp["researcher"].split(" ")[-1]
    filepath = os.path.join(
        DATA_DIRECTORY,
        f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}",
        f"measure_{specimen_id:03d}.csv",
    )
    abspath = os.path.abspath(filepath)
    return pd.read_csv(abspath)


def get_test_metadata(
    exp: Dict[str, str],
    specimen_id: int,
) -> Dict:
    researcher_name = exp["researcher"].split(" ")[-1]
    filepath = os.path.join(
        DATA_DIRECTORY,
        f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}",
        "tests.csv",
    )
    abspath = os.path.abspath(filepath)
    df = pd.read_csv(abspath)
    return df[df["sequential number"] == specimen_id].to_dict("records")[0]


def filter_regex(values: List[str], pattern: str | Pattern[str]) -> List[str]:
    return list(filter(lambda value: search(pattern, value), values))


def filter_columns(
    df: DataFrame,
    column_list: List[str],
    pattern: str | Pattern[str],
    fn: Callable[[float], float] = lambda value: value,
) -> Dict[str, List[float]]:
    columns = filter_regex(column_list, pattern)
    selected_df = df[columns].dropna()
    mapped_df = selected_df.apply(fn)
    return {column: mapped_df[column].to_list() for column in columns}


async def quasi_static_test(
    session: AsyncSession,
    experiment_id: int,
    test_id: int,
) -> Dict:
    experiment = (
        await session.execute(
            select(
                Experiment.laboratory,
                Experiment.researcher,
                Experiment.date,
                Experiment.experiment_type,
                Experiment.qs_experiment_type,
                Experiment.fa_experiment_type,
                Experiment.material_tested,
            ).where(Experiment.id == experiment_id)
        )
    ).one()._asdict()

    is_fracture = experiment.get("qs_experiment_type") == "fracture"

    test_meta = await get_test_fields(
        session, experiment_id, test_id,
        (Test.sequential_number, Test.specimen_name, Test.initial_crack_length)
    )
    specimen_id = test_meta["sequential_number"]
    df = get_dataframe(experiment, specimen_id)

    test_info = get_test_metadata(experiment, specimen_id)
    width = test_info.get("width")
    thickness = test_info.get("thickness")
    l_prime = test_info.get("l_prime") or 0
    t = test_info.get("t") or 0

    if is_fracture:
        crack_displacement = df["u"].dropna().tolist() if "u" in df.columns else []
        crack_load = df["Load"].dropna().tolist() if "Load" in df.columns else []
        crack_length = df["Crack_length"].dropna().tolist() if "Crack_length" in df.columns else []
        crack_load = (np.array(crack_load) / 1000).tolist()  # Convert to kN if needed

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
            crack_load: List[float],
            crack_length_fitted: np.ndarray,
            thickness: float,
            t: float,
            l_prime: float
        ):
            print(f"t value read: {t} mm")
            print(f"l prime value read: {l_prime} mm")

            displacement = np.array(crack_displacement)
            load = np.array(crack_load)
            crack = crack_length_fitted

            F = 1 - (3/10)*(displacement/crack)**2 - (3/2)*((displacement * t)/(crack**2))
            N = 1 - ((l_prime/crack)**3) - (9/8)*(1 - ((l_prime/crack)**2))*((displacement * t)/(crack**2)) - (9/35)*(displacement/crack)**2

            return F, N

        def compute_g_mbt(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width):
            C_N_1_3 = (compliance / N)**(1/3)
            params, _ = curve_fit(fit_func, crack_length_fitted, C_N_1_3)
            a, b = params
            print(f"a: {a}, b: {b}")
            triangle = abs(b / a)
            print(f"Triangle value: {triangle} mm")

            linear_compliance = np.mean(compliance[:5])
            threshold_compliance = linear_compliance * 1.01

            # Find the index where compliance exceeds the threshold
            threshold_index = np.where(compliance >= threshold_compliance)[0]
            
            G = (3 * np.array(crack_load) * np.array(crack_displacement)) / (2 * width * (crack_length_fitted + triangle)) * F / N * 1e6
            
            # G_init is the value of G at the threshold compliance. use the index of the first occurrence
            if len(threshold_index) > 0:
                G_init_mbt = G[threshold_index[0]]
                bridginglength_mbt = crack_length_fitted[threshold_index[0]]
            else:
                G_init_mbt = float("nan")
                bridginglength_mbt = float("nan")

            # calculate G plateau as the average f the values of G after the threshold compliance
            if len(threshold_index) > 0:
                G_plateau_mbt = np.mean(G[threshold_index[0]:])
            else:
                G_plateau_mbt = float("nan")

            return G.tolist(), G_init_mbt, bridginglength_mbt, G_plateau_mbt

        def compute_g_mcc(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width, thickness):
            a_over_h = crack_length_fitted / thickness
            C_N_1_3 = (compliance / N)**(1/3)
            A1, _ = np.polyfit(C_N_1_3, a_over_h, 1)
            G = (3 * np.array(crack_load)**2 * (compliance / N)**(2/3)) / (2 * A1 * width * thickness) * F * 1e6
            print(f"A1: {A1}")
            print(f"compliance min/max: {np.min(compliance)}, {np.max(compliance)}")
            print(f"N min/max: {np.min(N)}, {np.max(N)}")
            print(f"F min/max: {np.min(F)}, {np.max(F)}")
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

        # Calcolo dei valori comuni
        compliance, crack_length_fitted = compute_compliance_and_crack_length_fitted(
            crack_displacement, crack_load, crack_length
        )

        F, N = compute_factors(
            crack_displacement, crack_load, crack_length_fitted, thickness, t, l_prime
        )

        crack_fractureenergy_mbt, G_init_mbt, bridginglength_mbt, G_plateau_mbt = compute_g_mbt(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width
        )
        crack_fractureenergy_mcc = compute_g_mcc(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width, thickness
        )
        crack_fractureenergy_ecm = compute_g_ecm(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width
        )


        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length_fitted.tolist(),
            crack_fractureenergy_mbt=crack_fractureenergy_mbt,
            crack_fractureenergy_mcc=crack_fractureenergy_mcc,
            crack_fractureenergy_ecm=crack_fractureenergy_ecm,
            displacement={},
            load={},
            strain={},
            stress={},
            toughness=None,
            initial_crack_length=test_meta["initial_crack_length"],
            young_modulus=None,
            poisson_ratio=None,
            g_init_mbt=G_init_mbt,
            bridginglength_mbt=bridginglength_mbt,
            g_plateau_mbt=G_plateau_mbt,
        )

    else:
        poisson_ratio = None
        displacement = {"u": df["u"].dropna().tolist()} if "u" in df.columns else {}
        load = {"Load": df["Load"].dropna().tolist()} if "Load" in df.columns else {}

        strain = {}
        for col in ["exx", "eyy", "exy"]:
            if col in df.columns:
                label = "Engineering strain" if col == "exx" else col
                strain[label] = df[col].dropna().tolist()

        stress = {}
        if "Load" in df.columns and width and thickness:
            area = width * thickness
            stress["Engineering stress"] = (df["Load"] / area).dropna().tolist()

        toughness = None
        if "Engineering stress" in stress and "Engineering strain" in strain:
            stress_values = stress["Engineering stress"]
            strain_values = strain["Engineering strain"]
            min_len = min(len(stress_values), len(strain_values))
            if min_len > 1:
                toughness = float(np.trapz(stress_values[:min_len], strain_values[:min_len]))

        young_modulus = None
        if experiment.get("material_tested", "").lower() == "bulk adhesives":
            if "Engineering stress" in stress and "Engineering strain" in strain:
                stress_values = np.array(stress["Engineering stress"])
                strain_values = np.array(strain["Engineering strain"])
                mask = (strain_values >= 0.0015) & (strain_values <= 0.0035)
                if np.sum(mask) >= 2:
                    x = strain_values[mask]
                    y = stress_values[mask]
                    coeffs = np.polyfit(x, y, 1)
                    young_modulus = float(coeffs[0]) / 1000  # MPa → GPa

                
                if "eyy" in df.columns:
                    eyy_values = np.array(df["eyy"].dropna())
                    if len(eyy_values) == len(strain_values):  # Make sure the lengths match
                        eyy_range = eyy_values[mask]
                        exx_range = strain_values[mask]
                        if len(exx_range) >= 2:
                            # Linear fit: y = m * x + q → m = poisson_ratio
                            coeffs = np.polyfit(exx_range, -eyy_range, 1)
                            poisson_ratio = float(coeffs[0])

        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=[],
            crack_load=[],
            crack_length=[],
            crack_fractureenergy_mbt=[],
            crack_fractureenergy_mcc=[],
            crack_fractureenergy_ecm=[],
            displacement=displacement,
            load=load,
            strain=strain,
            stress=stress,
            toughness=toughness,
            initial_crack_length=None,
            young_modulus=young_modulus,
            poisson_ratio=poisson_ratio,
            g_init_mbt=None,
            bridginglength_mbt=None,
            g_plateau_mbt=None,
        )

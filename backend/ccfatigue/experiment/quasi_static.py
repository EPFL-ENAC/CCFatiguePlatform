import os
from re import Pattern, search
from typing import Callable, Dict, List, Any
import numpy as np

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
    crack_fractureenergy_eccm: List[float]


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

    if is_fracture:
        crack_displacement = df["u"].dropna().tolist() if "u" in df.columns else []
        if crack_displacement:
            crack_displacement = [v - crack_displacement[0] for v in crack_displacement]
        crack_load = df["Load"].dropna().tolist() if "Load" in df.columns else []
        crack_length = df["Crack_length"].dropna().tolist() if "Crack_length" in df.columns else []

        def compute_factors(crack_displacement, crack_load, crack_length, thickness):
            t = thickness / 4 + 22
            compliance = np.array(crack_displacement) / np.array(crack_load)
            crack = np.array(crack_length)
            displacement = np.array(crack_displacement)
            load = np.array(crack_load)

            F = 1 - (3/10)*(displacement/crack)**2 - (3/2)*((displacement * t)/(crack**2))
            N = 1 - (9/8)*((displacement * t)/(crack**2)) - (9/35)*(displacement/crack)**2
            return compliance, F, N

        def compute_g_mbt(crack_displacement, crack_load, crack_length, width, thickness):
            compliance, F, N = compute_factors(crack_displacement, crack_load, crack_length, thickness)
            C_N_1_3 = (compliance / N)**(1/3)
            a, b = np.polyfit(crack_length, C_N_1_3, 1)
            triangle = abs(b / a)
            G = (3 * np.array(crack_load) * np.array(crack_displacement)) / (2 * width * (np.array(crack_length) + triangle)) * F / N * 1e6
            return G.tolist()

        def compute_g_mcc(crack_displacement, crack_load, crack_length, width, thickness):
            compliance, F, N = compute_factors(crack_displacement, crack_load, crack_length, thickness)
            a_over_h = np.array(crack_length) / thickness
            C_N_1_3 = (compliance / N)**(1/3)
            A1, _ = np.polyfit(C_N_1_3, a_over_h, 1)
            G = (3 * np.array(crack_load)**2 * (compliance / N)**(2/3)) / (2 * A1 * width * thickness) * F * 1e6
            return G.tolist()

        def compute_g_eccm(crack_displacement, crack_load, crack_length, width, thickness):
            compliance, F, N = compute_factors(crack_displacement, crack_load, crack_length, thickness)

            # Convert to NumPy array for masking
            crack_array = np.array(crack_length)
            c_over_n = compliance / N

            # Crea maschera per evitare problemi con log10
            valid_mask = (crack_array > 0) & (c_over_n > 0) & np.isfinite(c_over_n)

            if np.sum(valid_mask) < 2:
                return [float("nan")] * len(crack_array)

            a_log = np.log10(crack_array[valid_mask])
            log_C_N = np.log10(c_over_n[valid_mask])

            m, _ = np.polyfit(a_log, log_C_N, 1)

            G = (m * np.array(crack_load) * np.array(crack_displacement)) / (2 * width * crack_array) * F / N * 1e6
            return G.tolist()
    

        # Placeholder per crack_fractureenergy
        crack_fractureenergy_mbt = compute_g_mbt(crack_displacement, crack_load, crack_length, width, thickness)
        crack_fractureenergy_mcc = compute_g_mcc(crack_displacement, crack_load, crack_length, width, thickness)
        crack_fractureenergy_eccm = compute_g_eccm(crack_displacement, crack_load, crack_length, width, thickness)


        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length,
            crack_fractureenergy_mbt=crack_fractureenergy_mbt,
            crack_fractureenergy_mcc=crack_fractureenergy_mcc,
            crack_fractureenergy_eccm=crack_fractureenergy_eccm,
            displacement={},
            load={},
            strain={},
            stress={},
            toughness=None,
            initial_crack_length=test_meta["initial_crack_length"],
            young_modulus=None,
            poisson_ratio=None,
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
            crack_fractureenergy_eccm=[],
            displacement=displacement,
            load=load,
            strain=strain,
            stress=stress,
            toughness=toughness,
            initial_crack_length=None,
            young_modulus=young_modulus,
            poisson_ratio=poisson_ratio,
        )

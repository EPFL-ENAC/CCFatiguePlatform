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
    crack_fractureenergy: List[float]
    displacement: Dict[str, List[float]]
    load: Dict[str, List[float]]
    strain: Dict[str, List[float]]
    stress: Dict[str, List[float]]
    toughness: float | None  # new feature
    initial_crack_length: float | None # new feature
    young_modulus: float | None # new feature
    poisson_ratio: float | None # new feature


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

        # Placeholder per crack_fractureenergy
        crack_fractureenergy = crack_length.copy()

        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length,
            crack_fractureenergy=crack_fractureenergy,
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
                    if len(eyy_values) == len(strain_values):  # assicurati che le lunghezze corrispondano
                        eyy_range = eyy_values[mask]
                        exx_range = strain_values[mask]
                        if len(exx_range) >= 2:
                            # Fit lineare: y = m * x + q → m = poisson_ratio
                            coeffs = np.polyfit(exx_range, -eyy_range, 1)
                            poisson_ratio = float(coeffs[0])

        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=[],
            crack_load=[],
            crack_length=[],
            crack_fractureenergy=[],
            displacement=displacement,
            load=load,
            strain=strain,
            stress=stress,
            toughness=toughness,
            initial_crack_length=None,
            young_modulus=young_modulus,
            poisson_ratio=poisson_ratio,
        )

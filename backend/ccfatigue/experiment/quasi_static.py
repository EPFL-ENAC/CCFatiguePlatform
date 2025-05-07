import os
from re import Pattern, search
from typing import Callable, Dict, List, Any
import numpy as np

import pandas as pd
from pandas import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from ccfatigue.experiment.common import extract_experiment_metadata

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.experiment.common import extract_experiment_metadata, flatten_metadata
from ccfatigue.models.database_v2 import Experiment, Test


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
    experiment_metadata: Dict[str, Any]  # new term
    toughness: float | None  # new feature


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
                Experiment.fracture_mode_fm,
                Experiment.loading_rate,
                Experiment.material_tested,
                Experiment.material_type_sample_type,
                Experiment.material_type_fiber_form,
                Experiment.material_type_resin,
                Experiment.laminates_and_assemblies_stacking_sequence,
                Experiment.curing_time,
                Experiment.curing_temperature,
                Experiment.curing_pressure,
                Experiment.postcuring_time,
                Experiment.postcuring_temperature,
                Experiment.postcuring_pressure,
                Experiment.publication_doi,
                Experiment.control_mode,
                Experiment.fatigue_r_ratio,
                Experiment.fatigue_frequency,
                Experiment.fatigue_loading_type_flt,
                Experiment.measuring_equipment,
            ).where(Experiment.id == experiment_id)
        )
    ).one()._asdict()

    is_fracture = (
        experiment.get("fa_experiment_type") == "fracture"
        or experiment.get("qs_experiment_type") == "fracture"
    )

    test_meta = await get_test_fields(
        session, experiment_id, test_id, (Test.sequential_number, Test.specimen_name)
    )
    specimen_id = test_meta["sequential_number"]
    df = get_dataframe(experiment, specimen_id)

    test_info = get_test_metadata(experiment, specimen_id)
    width = test_info.get("width")
    thickness = test_info.get("thickness")

    crack_displacement = df["u"].dropna().tolist() if is_fracture and "u" in df.columns else []
    crack_load = df["Load"].dropna().tolist() if is_fracture and "Load" in df.columns else []
    crack_length = df["Crack length"].dropna().tolist() if is_fracture and "Crack length" in df.columns else []

    displacement = {"u": df["u"].dropna().tolist()} if not is_fracture and "u" in df.columns else {}
    load = {"Load": df["Load"].dropna().tolist()} if not is_fracture and "Load" in df.columns else {}

    strain = {}
    for col in ["exx", "eyy", "exy"]:
        if col in df.columns:
            strain[col] = df[col].dropna().tolist()

    stress = {}
    if "Load" in df.columns and width and thickness:
        area = width * thickness
        stress["nominal"] = (df["Load"] / area).dropna().tolist()

    # Calcolo della toughness (area sotto la curva stress-strain)
    toughness = None
    if "nominal" in stress and "exx" in strain:
        stress_values = stress["nominal"]
        strain_values = strain["exx"]
        min_len = min(len(stress_values), len(strain_values))
        if min_len > 1:
            # Allineiamo i dati e calcoliamo l'area
            toughness = float(np.trapz(stress_values[:min_len], strain_values[:min_len]))


    metadata_flat = flatten_metadata(extract_experiment_metadata(experiment))
    import json
    #print("✅ experiment_metadata FLAT (to be sent to frontend):")
    #print(json.dumps(metadata_flat, indent=2))

    print("✅ SPECIMEN ID:", specimen_id)


    return QuasiStaticTest(
        specimen_name=test_meta["specimen_name"],
        specimen_id=test_meta["sequential_number"],
        crack_displacement=crack_displacement,
        crack_load=crack_load,
        crack_length=crack_length,
        displacement=displacement,
        load=load,
        strain=strain,
        stress=stress,
        experiment_metadata=metadata_flat,
        toughness=toughness  # 👈 nuova proprietà
    )
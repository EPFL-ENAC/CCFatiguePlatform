import os
from typing import Dict, List

import pandas as pd
from pandas import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.common import DATA_DIRECTORY, get_specimen_id
from ccfatigue.models.database import Experiment


class QuasiStaticTest(BaseModel):
    machine_displacement: List[float]
    machine_load: List[float]
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    strain: Dict[str, List[float]]
    stress: List[float]


def get_dataframe(
    exp: Dict[str, str],
    specimen_id: int,
) -> DataFrame:
    """
    return extracted DataFrame related to that test from CSV
    """
    # FIXME researcher_name from a column value
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
    """
    return extracted metadata related to the test from CSV
    """
    # FIXME researcher_name from a column value
    researcher_name = exp["researcher"].split(" ")[-1]
    filepath = os.path.join(
        DATA_DIRECTORY,
        f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}",
        "tests.csv",
    )
    abspath = os.path.abspath(filepath)
    df = pd.read_csv(abspath)
    return df[df["specimen number"] == specimen_id].to_dict("records")[0]


async def quasi_static_test(
    session: AsyncSession,
    experiment_id: int,
    test_id: int,
) -> QuasiStaticTest:
    experiment: Dict[str, str] = (
        (
            await session.execute(
                select(
                    Experiment.researcher,
                    Experiment.experiment_type,
                    Experiment.date,
                    Experiment.fracture,
                ).where(Experiment.id == experiment_id)
            )
        )
        .one()  # type: ignore
        ._asdict()
    )
    specimen_id = await get_specimen_id(session, experiment_id, test_id)
    df = get_dataframe(experiment, specimen_id)
    machine_df = (
        df[["Machine_Displacement", "Machine_Load"]].dropna()
        if {"Machine_Displacement", "Machine_Load"}.issubset(df.columns)
        else pd.DataFrame(columns=["Machine_Displacement", "Machine_Load"])
    )

    fracture = experiment["fracture"]
    crack_df = (
        df[["Crack_Displacement", "Crack_Load", "Crack_length"]].dropna()
        if fracture
        else pd.DataFrame(columns=["Crack_Displacement", "Crack_Load", "Crack_length"])
    )
    strain_stress_df = pd.DataFrame(columns=["Stress"])
    strain: Dict[str, List[float]] = {}
    if not fracture:
        test = get_test_metadata(experiment, specimen_id)
        area = test["width"] * test["thickness"]

        if "exx--1" in df.columns:
            strain["exx--1"] = df["exx--1"].to_list()
        if "eyy--1" in df.columns:
            strain["eyy--1"] = df["eyy--1"].to_list()
        if "exy--1" in df.columns:
            strain["exy--1"] = df["exy--1"].to_list()

        strain_stress_df["Stress"] = df["MD_Load--1"] / area
    return QuasiStaticTest(
        machine_displacement=machine_df["Machine_Displacement"].to_list(),
        machine_load=machine_df["Machine_Load"].to_list(),
        crack_displacement=crack_df["Crack_Displacement"].to_list(),
        crack_load=crack_df["Crack_Load"].to_list(),
        crack_length=crack_df["Crack_length"].to_list(),
        strain=strain,
        stress=strain_stress_df["Stress"].to_list(),
    )

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
                ).where(Experiment.id == experiment_id)
            )
        )
        .one()  # type: ignore
        ._asdict()
    )
    specimen_id = await get_specimen_id(session, experiment_id, test_id)
    df = get_dataframe(experiment, specimen_id)
    machine_df = df[["Machine_Displacement", "Machine_Load"]].dropna()
    crack_df = df[["Crack_Displacement", "Crack_Load", "Crack_length"]].dropna()
    return QuasiStaticTest(
        machine_displacement=machine_df["Machine_Displacement"].to_list(),
        machine_load=machine_df["Machine_Load"].to_list(),
        crack_displacement=crack_df["Crack_Displacement"].to_list(),
        crack_load=crack_df["Crack_Load"].to_list(),
        crack_length=crack_df["Crack_length"].to_list(),
    )

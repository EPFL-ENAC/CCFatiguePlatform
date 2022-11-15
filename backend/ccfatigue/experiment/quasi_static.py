import os
from re import Pattern, search
from typing import Callable, Dict, List

import pandas as pd
from pandas import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test


class QuasiStaticTest(BaseModel):
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    displacement: Dict[str, List[float]]
    load: Dict[str, List[float]]
    strain: Dict[str, List[float]]
    stress: Dict[str, List[float]]


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

    test_meta = await get_test_fields(
        session, experiment_id, test_id, (Test.specimen_number,)
    )
    df = get_dataframe(experiment, test_meta["specimen_number"])
    column_list = df.columns.to_list()

    displacement = filter_columns(
        df, column_list, r"^(Machine_Displacement|MD_Displacement--\d+|u--\d+|v--\d+)$"
    )
    load = filter_columns(df, column_list, r"^(Machine_Load|MD_Load--\d+)$")

    fracture = experiment["fracture"]
    crack_df = (
        df[["Crack_Displacement", "Crack_Load", "Crack_length"]].dropna()
        if fracture
        and {"Crack_Displacement", "Crack_Load", "Crack_length"}.issubset(df.columns)
        else pd.DataFrame(columns=["Crack_Displacement", "Crack_Load", "Crack_length"])
    )
    strain: Dict[str, List[float]] = {}
    stress: Dict[str, List[float]] = {}
    if not fracture:
        test = get_test_metadata(experiment, test_meta["specimen_number"])
        if "width" in test and "thickness" in test:
            strain = filter_columns(df, column_list, r"^(exx--\d+|eyy--\d+|exy--\d+)$")
            area = test["width"] * test["thickness"]
            stress = filter_columns(
                df,
                column_list,
                r"^(MD_Load--\d+|Machine_Load)$",
                lambda value: value / area,
            )
    return QuasiStaticTest(
        crack_displacement=crack_df["Crack_Displacement"].to_list(),
        crack_load=crack_df["Crack_Load"].to_list(),
        crack_length=crack_df["Crack_length"].to_list(),
        displacement=displacement,
        load=load,
        strain=strain,
        stress=stress,
    )

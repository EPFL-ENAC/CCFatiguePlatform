import os
from typing import Dict, List

import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test

INTERVAL: int = 10
LOOP_SPACING: int = 1000
MAGNITUDE: int = -3


class HysteresisLoop(BaseModel):
    n_cycles: List[float]
    strain: List[float]
    stress: List[float]


class FatigueTest(BaseModel):
    specimen_id: int
    total_dissipated_energy: int
    run_out: bool
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]


def get_dataframe(
    data_in: str,
    exp: Dict[str, str],
    specimen_id: int,
) -> DataFrame:
    """
    return extracted DataFrame related to that test from CSV
    """
    # FIXME researcher_name from a column value
    researcher_name = exp["researcher"].split(" ")[-1]
    if data_in == "HYS":
        filepath = os.path.join(
            DATA_DIRECTORY,
            f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}",
            f"{data_in}_measure_{specimen_id:03d}.csv",
        )
    else:
        filepath = os.path.join(
            DATA_DIRECTORY,
            f"{data_in}_{researcher_name}_{exp['date']}_{exp['experiment_type']}",
            f"measure_{specimen_id:03d}.csv",
        )
    abspath = os.path.abspath(filepath)
    return pd.read_csv(abspath)


def get_total_dissipated_energy(hyst_df: DataFrame) -> int:
    """
    return calculated Total Dissipated Energy (TDE)
    """
    return np.sum(hyst_df["hysteresis_area"])


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """
    Compute subset of index used for plotting curves
    """
    unique_n_cycles = np.unique(df.n_cycles)
    indexes = np.linspace(0, unique_n_cycles.size - 1, 10).astype(int)
    return unique_n_cycles[indexes]


def create_sub_hystloops(df: DataFrame, sub_indexes: List[int]) -> List[HysteresisLoop]:
    """
    Conditional plotting of hysteresis loops
    we only plot the loops specified by sub_index
    """
    sub_hystloops: List[HysteresisLoop] = []
    for sub_index in sub_indexes:
        sub_hystloops_strain = []
        sub_hystloops_stress = []
        sub_hystloops_ncycles = []
        for i in range(len(df)):
            if df.Machine_N_cycles[i] == sub_index:
                sub_hystloops_strain.append(df.Machine_Displacement[i])
                sub_hystloops_stress.append(df.Machine_Load[i])
                sub_hystloops_ncycles.append(df.Machine_N_cycles[i])
        # make curve closed // Optional
        if len(sub_hystloops_ncycles) != 0:
            sub_hystloops_strain.append(sub_hystloops_strain[0])
            sub_hystloops_stress.append(sub_hystloops_stress[0])
            sub_hystloops_ncycles.append(sub_hystloops_ncycles[0])
        if (
            len(sub_hystloops_ncycles) > 0
            and len(sub_hystloops_strain) > 0
            and len(sub_hystloops_stress) > 0
        ):
            sub_hystloops.append(
                HysteresisLoop(
                    n_cycles=sub_hystloops_ncycles,
                    strain=sub_hystloops_strain,
                    stress=sub_hystloops_stress,
                )
            )
    return sub_hystloops


async def fatigue_test(
    session: AsyncSession,
    experiment_id: int,
    test_id: int,
) -> FatigueTest:
    experiment: Dict[str, str] = (
        (
            await session.execute(
                select(
                    Experiment.laboratory,
                    Experiment.researcher,
                    Experiment.experiment_type,
                    Experiment.date,
                ).where(Experiment.id == experiment_id)
            )
        )
        .one()  # type: ignore
        ._asdict()
    )
    test_meta = await get_test_fields(
        session, experiment_id, test_id, (Test.specimen_number, Test.run_out)
    )
    std_df = get_dataframe("TST", experiment, test_meta["specimen_number"])
    hyst_df = get_dataframe("HYS", experiment, test_meta["specimen_number"]).fillna(
        value=0
    )
    hysteresis_loops = create_sub_hystloops(std_df, compute_sub_indexes(hyst_df))
    return FatigueTest(
        specimen_id=test_meta["specimen_number"],
        run_out=test_meta["run_out"],
        total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        hysteresis_loops=hysteresis_loops,
        n_cycles=hyst_df["n_cycles"].to_list(),
        creep=hyst_df["creep"].to_list(),
        hysteresis_area=hyst_df["hysteresis_area"].to_list(),
        stiffness=hyst_df["stiffness"].to_list(),
    )

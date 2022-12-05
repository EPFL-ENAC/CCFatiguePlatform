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
    stress_ratio: float
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]
    stress_at_failure: float
    strain_at_failure: float
    n_fail: int


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


def fatigue_processing(df: DataFrame, sub_indexes: List[int], test_meta) -> Dict:
    """
    return {
        "sub_hystloops": list of hysteresis loops
            only for the loops specified by sub_index
        "n_fail"
        "stress_at_failure"
        "strain_at_failure"

    }
    """
    sub_hystloops: List[HysteresisLoop] = []

    if "Machine_N_cycles" in df.columns and df.count().Machine_N_cycles > 0:
        n_cycles = np.sort(df["Machine_N_cycles"].unique())
        cycle_field = "Machine_N_cycles"
        load_field = "Machine_Load"
        displacement_field = "Machine_Displacement"
    else:
        n_cycles = np.sort(df["MD_N_cycles--1"].unique())
        cycle_field = "MD_N_cycles--1"
        load_field = "MD_Load--1"
        displacement_field = "MD_Displacement--1"

    # calculate Stress and Strain
    df = df.assign(
        stress=df[load_field] / (test_meta["width"] * test_meta["thickness"]),
        strain=df[displacement_field] / test_meta["length"],
    )

    n_fail = np.max(n_cycles)
    last_cycle = df[cycle_field].unique()[-1]
    stress_at_failure = np.max(df[df[cycle_field] == last_cycle].stress)
    strain_at_failure = np.max(df[df[cycle_field] == last_cycle].strain)

    for sub_index in sub_indexes:
        mask = df.Machine_N_cycles == sub_index
        nb_entries = mask.value_counts().loc[True]

        sub_hystloops_strain = np.full(nb_entries + 1, np.nan)
        sub_hystloops_stress = np.full(nb_entries + 1, np.nan)
        sub_hystloops_ncycles = np.full(nb_entries + 1, np.nan)

        sub_hystloops_stress[0:nb_entries] = df[mask].stress
        sub_hystloops_strain[0:nb_entries] = df[mask].strain
        sub_hystloops_ncycles[0:nb_entries] = df[mask][cycle_field]
        sub_hystloops_stress[nb_entries] = sub_hystloops_stress[0]
        sub_hystloops_strain[nb_entries] = sub_hystloops_strain[0]
        sub_hystloops_ncycles[nb_entries] = sub_hystloops_ncycles[0]
        if (
            sub_hystloops_ncycles.size > 0
            and sub_hystloops_stress.size > 0
            and sub_hystloops_strain.size > 0
        ):
            sub_hystloops.append(
                HysteresisLoop(
                    n_cycles=sub_hystloops_ncycles.tolist(),
                    stress=sub_hystloops_stress.tolist(),
                    strain=sub_hystloops_strain.tolist(),
                )
            )
    return {
        "sub_hystloops": sub_hystloops,
        "n_fail": int(n_fail),
        "stress_at_failure": stress_at_failure,
        "strain_at_failure": strain_at_failure,
    }


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
        session,
        experiment_id,
        test_id,
        (
            Test.specimen_number,
            Test.run_out,
            Test.stress_ratio,
            Test.width,
            Test.thickness,
            Test.length,
        ),
    )
    std_df = get_dataframe("TST", experiment, test_meta["specimen_number"])
    hyst_df = get_dataframe("HYS", experiment, test_meta["specimen_number"]).fillna(
        value=0
    )
    fatigue_processed = fatigue_processing(
        std_df, compute_sub_indexes(hyst_df), test_meta
    )

    return FatigueTest(
        specimen_id=test_meta["specimen_number"],
        run_out=test_meta["run_out"],
        stress_ratio=test_meta["stress_ratio"],
        total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        hysteresis_loops=fatigue_processed["sub_hystloops"],
        n_cycles=hyst_df["n_cycles"].to_list(),
        creep=hyst_df["creep"].to_list(),
        hysteresis_area=hyst_df["hysteresis_area"].to_list(),
        stiffness=hyst_df["stiffness"].to_list(),
        stress_at_failure=fatigue_processed["stress_at_failure"],
        strain_at_failure=fatigue_processed["strain_at_failure"],
        n_fail=fatigue_processed["n_fail"],
    )

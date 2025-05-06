import os
from typing import Dict, List

import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database_v2 import Experiment, Test

INTERVAL: int = 10
LOOP_SPACING: int = 1000
MAGNITUDE: int = -3


class HysteresisLoop(BaseModel):
    n_cycles: List[float]
    strain: List[float]
    stress: List[float]


class FatigueTest(BaseModel):
    specimen_id: int
    specimen_name: str
    total_dissipated_energy: int
    run_out: bool
    #stress_ratio: float
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]
    stress_at_failure: float  # actually max_stress now
    strain_at_failure: float  # last value of creep
    n_fail: int


def get_dataframe(data_in: str, exp: Dict[str, str], specimen_id: int) -> DataFrame:
    """
    Return extracted DataFrame related to that test from CSV
    """
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
    return np.sum(hyst_df["hysteresis_area"])


def compute_sub_indexes(df: DataFrame) -> List[int]:
    unique_n_cycles = np.unique(df["n_cycles"])
    indexes = np.linspace(0, unique_n_cycles.size - 1, 10).astype(int)
    return unique_n_cycles[indexes]


def fatigue_processing(df: DataFrame, sub_indexes: List[int], test_meta) -> Dict:
    sub_hystloops: List[HysteresisLoop] = []

    cycle_field = "N_cycles"
    load_field = "Load"
    strain_field = "exx"  # or "exx" depending on setup

    df = df.assign(
        stress=(df[load_field] * 1000) / (test_meta["width"] * test_meta["thickness"]),
        strain=df[strain_field],
    )

    n_cycles = np.sort(df[cycle_field].unique())
    n_fail = int(np.max(n_cycles))

    for sub_index in sub_indexes:
        mask = df[cycle_field] == sub_index
        nb_entries = mask.sum()

        if nb_entries == 0:
            continue

        sub_hystloops_stress = df.loc[mask, "stress"].to_numpy()
        sub_hystloops_strain = df.loc[mask, "strain"].to_numpy()
        sub_hystloops_ncycles = df.loc[mask, cycle_field].to_numpy()

        # Close the loop
        sub_hystloops_stress = np.append(sub_hystloops_stress, sub_hystloops_stress[0])
        sub_hystloops_strain = np.append(sub_hystloops_strain, sub_hystloops_strain[0])
        sub_hystloops_ncycles = np.append(sub_hystloops_ncycles, sub_hystloops_ncycles[0])

        sub_hystloops.append(
            HysteresisLoop(
                n_cycles=sub_hystloops_ncycles.tolist(),
                stress=sub_hystloops_stress.tolist(),
                strain=sub_hystloops_strain.tolist(),
            )
        )

    return {
        "sub_hystloops": sub_hystloops,
        "n_fail": n_fail,
    }


async def fatigue_test(session: AsyncSession, experiment_id: int, test_id: int) -> FatigueTest:
    experiment: Dict[str, str] = (
        (await session.execute(
            select(
                Experiment.laboratory,
                Experiment.researcher,
                Experiment.experiment_type,
                Experiment.date,
            ).where(Experiment.id == experiment_id))
        )
        .one()  # type: ignore
        ._asdict()
    )

    test_meta = await get_test_fields(
        session,
        experiment_id,
        test_id,
        (
            Test.sequential_number,
            Test.specimen_name,
            Test.run_out,
            #Test.stress_ratio,
            Test.width,
            Test.thickness,
            Test.length,
            Test.maximum_load,  # <- nuovo campo per max_stress
        ),
    )

    std_df = get_dataframe("TST", experiment, test_meta["sequential_number"])
    hyst_df = get_dataframe("HYS", experiment, test_meta["sequential_number"]).fillna(0)

    fatigue_processed = fatigue_processing(std_df, compute_sub_indexes(hyst_df), test_meta)

    max_stress = test_meta["maximum_load"] / (test_meta["width"] * test_meta["thickness"])
    strain_at_failure = hyst_df["creep"].iloc[-1]

    specimen_name = test_meta.get("specimen_name") or test_meta["sequential_number"]

    return FatigueTest(
        specimen_id=test_meta["sequential_number"],
        specimen_name=specimen_name,
        run_out=test_meta["run_out"],
        # stress_ratio=test_meta["stress_ratio"],
        total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        hysteresis_loops=fatigue_processed["sub_hystloops"],
        n_cycles=hyst_df["n_cycles"].to_list(),
        creep=hyst_df["creep"].to_list(),
        hysteresis_area=hyst_df["hysteresis_area"].to_list(),
        stiffness=hyst_df["stiffness"].to_list(),
        stress_at_failure=max_stress,
        strain_at_failure=strain_at_failure,
        n_fail=fatigue_processed["n_fail"],
    )

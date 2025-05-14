import os
from typing import Dict, List, Optional

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
    total_dissipated_energy: Optional[float] = None
    run_out: bool
    #stress_ratio: float
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]
    stress_at_failure: Optional[float] = None  # actually max_stress now
    strain_at_failure: Optional[float] = None  # last value of creep
    n_fail: int
    warning_messages: bool
    
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    crack_n_cycles: List[float]


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


def get_total_dissipated_energy(hyst_df: DataFrame) -> float:
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
        stress=(df[load_field]) / (test_meta["width"] * test_meta["thickness"]),
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
                Experiment.fa_experiment_type,
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
            Test.number_of_cycles,
            #Test.stress_ratio,
            Test.width,
            Test.thickness,
            Test.length,
            Test.maximum_load,  # <- nuovo campo per max_stress
        ),
    )
    warning_triggered = False
    is_fracture = experiment.get("fa_experiment_type") == "fracture"
    std_df = get_dataframe("TST", experiment, test_meta["sequential_number"])
    specimen_name = test_meta.get("specimen_name") or test_meta["sequential_number"]
        # Defaults
    warning_triggered = False
    hysteresis_loops = []
    n_cycles = []
    creep = []
    hysteresis_area = []
    stiffness = []
    total_dissipated_energy = None
    stress_at_failure = None
    strain_at_failure = None
    crack_displacement = []
    crack_load = []
    crack_length = []
    crack_n_cycles = []
    if is_fracture:
        crack_load = std_df["Load"].tolist() if "Load" in std_df.columns else []
        crack_displacement = std_df["u"].tolist() if "u" in std_df.columns else []
        crack_length = std_df["Crack_length"].tolist() if "Crack_length" in std_df.columns else []
        crack_n_cycles = std_df["N_cycles"].tolist() if "N_cycles" in std_df.columns else []
    else:
        hyst_df = get_dataframe("HYS", experiment, test_meta["sequential_number"]).fillna(0)
        '''
        def smooth_outliers(data: List[float], threshold: float = 10) -> List[float]:
            """
            Rimuove outlier con filtro a finestra mobile di 5 elementi.
            L'elemento centrale viene sostituito con la media degli altri 4 se è un outlier.
            """
            smoothed = data.copy()
            for i in range(2, len(data) - 2):
                window = data[i-2:i+3]
                center = window[2]
                others = window[:2] + window[3:]
                mean_others = np.mean(others)
                std_others = np.std(others)
                if abs(center - mean_others) > threshold * std_others:
                    smoothed[i] = mean_others
            return smoothed

        def log_if_modified(original: List[float], filtered: List[float], field: str, specimen_name: str):
            diffs = [i for i, (o, f) in enumerate(zip(original, filtered)) if o != f]
            if diffs:
                print(
                    f"[Filtro outlier] Provino '{specimen_name}' – colonna '{field}' modificata in {len(diffs)} cicli "
                    f"(posizioni: {diffs})"
                )


        original_creep = hyst_df["creep"].tolist()
        filtered_creep = smooth_outliers(original_creep)
        log_if_modified(original_creep, filtered_creep, "creep", specimen_name)
        hyst_df["creep"] = filtered_creep
        original_area = hyst_df["hysteresis_area"].tolist()
        filtered_area = smooth_outliers(original_area)
        log_if_modified(original_area, filtered_area, "hysteresis_area", specimen_name)
        hyst_df["hysteresis_area"] = filtered_area
        original_stiffness = hyst_df["stiffness"].tolist()
        filtered_stiffness = smooth_outliers(original_stiffness)
        log_if_modified(original_stiffness, filtered_stiffness, "stiffness", specimen_name)
        hyst_df["stiffness"] = filtered_stiffness
        '''
        def smooth_spikes(data: List[float], threshold: float = 0.1) -> List[float]:
            """
            Rimuove spike/drop locali in una finestra mobile di 5 elementi.
            Il valore centrale viene sostituito se è molto diverso dalla media delle due mezze finestre,
            ma le mezze finestre sono tra loro coerenti.
            """
            smoothed = data.copy()
            for i in range(2, len(data) - 2):
                v0, v1, v2, v3, v4 = data[i-2:i+3]
                m1 = (v0 + v1) / 2
                m2 = (v3 + v4) / 2
                avg_context = (m1 + m2) / 2

                if avg_context == 0:
                    continue

                diff_between_halves = abs(m1 - m2) / avg_context
                center_diff = abs(v2 - avg_context) / avg_context

                if diff_between_halves < threshold and center_diff > threshold:
                    smoothed[i] = avg_context
            return smoothed


        def log_if_modified(original: List[float], filtered: List[float], field: str, specimen_name: str):
            nonlocal warning_triggered
            diffs = [i for i, (o, f) in enumerate(zip(original, filtered)) if o != f]
            if diffs:
                warning_triggered = True
                print(
                    f"[Filtro spike] Provino '{specimen_name}' – colonna '{field}' modificata in {len(diffs)} cicli "
                    f"(posizioni: {diffs})"
                )


        original_creep = hyst_df["creep"].tolist()
        filtered_creep = smooth_spikes(original_creep)
        log_if_modified(original_creep, filtered_creep, "creep", specimen_name)
        hyst_df["creep"] = filtered_creep

        original_area = hyst_df["hysteresis_area"].tolist()
        filtered_area = smooth_spikes(original_area)
        log_if_modified(original_area, filtered_area, "hysteresis_area", specimen_name)
        hyst_df["hysteresis_area"] = filtered_area

        original_stiffness = hyst_df["stiffness"].tolist()
        filtered_stiffness = smooth_spikes(original_stiffness)
        log_if_modified(original_stiffness, filtered_stiffness, "stiffness", specimen_name)
        hyst_df["stiffness"] = filtered_stiffness
        fatigue_processed = fatigue_processing(std_df, compute_sub_indexes(hyst_df), test_meta)

        max_stress = test_meta["maximum_load"] / (test_meta["width"] * test_meta["thickness"])
        strain_at_failure = hyst_df["creep"].iloc[-1]
        total_dissipated_energy = get_total_dissipated_energy(hyst_df)

    '''
    return FatigueTest(
        specimen_id=test_meta["sequential_number"],
        specimen_name=specimen_name,
        run_out=test_meta["run_out"],
        # stress_ratio=test_meta["stress_ratio"],
        total_dissipated_energy=total_dissipated_energy,
        hysteresis_loops=fatigue_processed["sub_hystloops"],
        n_cycles=hyst_df["n_cycles"].to_list(),
        creep=hyst_df["creep"].to_list(),
        hysteresis_area=hyst_df["hysteresis_area"].to_list(),
        stiffness=hyst_df["stiffness"].to_list(),
        stress_at_failure=max_stress,
        strain_at_failure=strain_at_failure,
        # n_fail=fatigue_processed["n_fail"],
        n_fail=test_meta["number_of_cycles"],
        warning_messages = warning_triggered,
        crack_displacement=crack_displacement,
        crack_load=crack_load,
        crack_length=crack_length,
        crack_n_cycles=crack_n_cycles,
    )
    '''
    if is_fracture:
        return FatigueTest(
            specimen_id=test_meta["sequential_number"],
            specimen_name=specimen_name,
            run_out=test_meta["run_out"],
            total_dissipated_energy=None,
            hysteresis_loops=[],
            n_cycles=[],
            creep=[],
            hysteresis_area=[],
            stiffness=[],
            stress_at_failure=None,
            strain_at_failure=None,
            n_fail=test_meta["number_of_cycles"],
            warning_messages=False,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length,
            crack_n_cycles=crack_n_cycles,
        )
    else:
        return FatigueTest(
            specimen_id=test_meta["sequential_number"],
            specimen_name=specimen_name,
            run_out=test_meta["run_out"],
            total_dissipated_energy=get_total_dissipated_energy(hyst_df),
            hysteresis_loops=fatigue_processed["sub_hystloops"],
            n_cycles=hyst_df["n_cycles"].to_list(),
            creep=hyst_df["creep"].to_list(),
            hysteresis_area=hyst_df["hysteresis_area"].to_list(),
            stiffness=hyst_df["stiffness"].to_list(),
            stress_at_failure=max_stress,
            strain_at_failure=strain_at_failure,
            n_fail=test_meta["number_of_cycles"],
            warning_messages=warning_triggered,
            crack_displacement=[],
            crack_load=[],
            crack_length=[],
            crack_n_cycles=[],
        )

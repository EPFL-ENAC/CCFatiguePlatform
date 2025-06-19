import os
from typing import Dict, List, Optional, Sequence, Tuple
from statistics import median

import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from scipy.optimize import curve_fit

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test
from sklearn.linear_model import LinearRegression
import math
from ccfatigue.experiment.fatigue_with_fracture_utils import (
    compute_compliance_and_crack_length_fitted,
    compute_factors,
    compute_g_mbt,
    compute_g_mcc,
    compute_g_ecm,
    find_fit_limit,
    find_best_paris_fit,  
    compute_da_dn
)
from ccfatigue.experiment.fatigue_utils import (
    get_loops_dataframe,
    extract_hysteresis_loops_from_file,
    compute_sub_indexes,
    fatigue_processing,
    apply_spike_filter,
    apply_iqr_filter,
)


# ==== Costants ====
INTERVAL = 10
LOOP_SPACING = 1000
MAGNITUDE = -3


# ==== Models ====

class HysteresisLoop(BaseModel):
    n_cycles: List[float]
    strain: List[float]
    stress: List[float]


class FatigueTest(BaseModel):
    specimen_id: int
    specimen_name: str
    total_dissipated_energy: Optional[float] = None
    run_out: bool
    hysteresis_loops: List[HysteresisLoop]
    n_cycles: List[float]
    creep: List[float]
    hysteresis_area: List[float]
    stiffness: List[float]
    stress_at_failure: Optional[float] = None
    strain_at_failure: Optional[float] = None
    n_fail: int | None
    warning_messages: bool
    crack_displacement: List[float]
    crack_load: List[float]
    crack_length: List[float]
    crack_n_cycles: List[float]
    da_dN: Optional[List[float]] = None
    G_MBT: Optional[List[float]] = None
    G_MCC: Optional[List[float]] = None
    G_ECM: Optional[List[float]] = None
    c_paris_mbt: float | None 
    m_paris_mbt: float | None
    G_th_mbt: float | None 
    c_paris_mcc: float | None 
    m_paris_mcc: float | None
    G_th_mcc: float | None 
    c_paris_ecm: float | None 
    m_paris_ecm: float | None
    G_th_ecm: float | None 


def get_dataframe(data_in: str, exp: Dict[str, str], specimen_id: int) -> DataFrame:
    researcher_name = exp["researcher"].split(" ")[-1]
    folder_name = (
        f"TST_{researcher_name}_{exp['date']}_{exp['experiment_type']}"
        if data_in == "HYS"
        else f"{data_in}_{researcher_name}_{exp['date']}_{exp['experiment_type']}"
    )
    filename = (
        f"{data_in}_measure_{specimen_id:03d}.csv"
        if data_in == "HYS"
        else f"measure_{specimen_id:03d}.csv"
    )
    filepath = os.path.join(DATA_DIRECTORY, folder_name, filename)
    return pd.read_csv(os.path.abspath(filepath))

async def fatigue_test(session: AsyncSession, experiment_id: int, test_id: int) -> FatigueTest:
    experiment = (await session.execute(
        select(
            Experiment.laboratory,
            Experiment.researcher,
            Experiment.experiment_type,
            Experiment.date,
            Experiment.fa_experiment_type,
        ).where(Experiment.id == experiment_id))
    ).one()._asdict()

    test_meta = await get_test_fields(session, experiment_id, test_id, (
        Test.sequential_number,
        Test.specimen_name,
        Test.run_out,
        Test.number_of_cycles,
        Test.width,
        Test.thickness,
        Test.length,
        Test.maximum_load,
        Test.t, 
        Test.l_prime,
    ))

    specimen_id = test_meta["sequential_number"]
    specimen_name = test_meta.get("specimen_name") or specimen_id
    is_fracture = experiment.get("fa_experiment_type") == "fracture"
    warning_triggered = False

    std_df = get_dataframe("TST", experiment, specimen_id)

    if is_fracture:
        crack_load = std_df["Load"].tolist() if "Load" in std_df else []
        crack_load = (np.array(crack_load) / 1000).tolist()  # Convert to kN if needed
        crack_displacement = std_df["u"].tolist() if "u" in std_df else []
        crack_length = std_df["Crack_length"].tolist() if "Crack_length" in std_df else []
        crack_n_cycles = std_df["N_cycles"].tolist() if "N_cycles" in std_df else []

        w = test_meta["width"]
        h = test_meta["thickness"]
        t = test_meta["t"] or 0.0
        l_prime = test_meta["l_prime"] or 0.0

        # Compute common values
        compliance, crack_length_fitted = compute_compliance_and_crack_length_fitted(
            crack_displacement, crack_load, crack_length
        )

        F, N = compute_factors(crack_displacement, crack_length_fitted, t, l_prime)

        G_MBT = compute_g_mbt(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w
        )

        G_MCC = compute_g_mcc(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w, h
        )

        G_ECM = compute_g_ecm(
            compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w
        )

        da_dn_new = compute_da_dn(crack_length_fitted, crack_n_cycles)

        G_th_mbt, slope_mbt = find_fit_limit(G_MBT, da_dn_new)

        G_th_mcc, slope_mbt = find_fit_limit(G_MCC, da_dn_new)

        G_th_ecm, slope_mbt = find_fit_limit(G_ECM, da_dn_new)

        m_mbt, C_mbt, r2_best_mbt = find_best_paris_fit(G_MBT, da_dn_new, da_dn_min=0.00001, da_dn_max=0.001)

        m_mcc, C_mcc, r2_best_mcc = find_best_paris_fit(G_MCC, da_dn_new, da_dn_min=0.00001, da_dn_max=0.001)

        m_ecm, C_ecm, r2_best_ecm = find_best_paris_fit(G_ECM, da_dn_new, da_dn_min=0.00001, da_dn_max=0.001)

        return FatigueTest(
            specimen_id=specimen_id,
            specimen_name=specimen_name,
            run_out=test_meta["run_out"],
            hysteresis_loops=[],
            n_cycles=crack_n_cycles,
            creep=[],
            hysteresis_area=[],
            stiffness=[],
            total_dissipated_energy=None,
            stress_at_failure=None,
            strain_at_failure=None,
            n_fail=test_meta["number_of_cycles"],
            warning_messages=False,
            crack_displacement=crack_displacement,
            crack_load=crack_load,
            crack_length=crack_length_fitted.tolist(),
            crack_n_cycles=crack_n_cycles,
            da_dN=da_dn_new,
            G_MBT=G_MBT,
            G_MCC=G_MCC,
            G_ECM=G_ECM,
            m_paris_mbt=m_mbt,
            c_paris_mbt=C_mbt,
            G_th_mbt=G_th_mbt,
            m_paris_mcc=m_mcc,
            c_paris_mcc=C_mcc,
            G_th_mcc=G_th_mcc,
            m_paris_ecm=m_ecm,
            c_paris_ecm=C_ecm,
            G_th_ecm=G_th_ecm,
        )


    hyst_df = get_dataframe("HYS", experiment, specimen_id).fillna(0)

    for field in ["creep", "hysteresis_area", "stiffness"]:
        # 1️⃣ Global-scale cleaning: IQR filter
        values, _ = apply_iqr_filter(hyst_df, field, specimen_name)
        hyst_df[field] = values                # ← reviewer can comment here

        # 2️⃣ Local-scale smoothing: spike filter
        values, changed = apply_spike_filter(hyst_df, field, specimen_name)
        hyst_df[field] = values                # ← …or here
        warning_triggered = warning_triggered or changed

    loops_df = get_loops_dataframe(experiment, specimen_id)

    if loops_df is not None:
        loops = extract_hysteresis_loops_from_file(loops_df)
        n_fail = test_meta["number_of_cycles"]
    else:
        result = fatigue_processing(std_df, compute_sub_indexes(hyst_df), test_meta)
        loops = result["sub_hystloops"]
        n_fail = result["n_fail"]

    max_stress = test_meta["maximum_load"] / (test_meta["width"] * test_meta["thickness"])

    return FatigueTest(
        specimen_id=specimen_id,
        specimen_name=specimen_name,
        run_out=test_meta["run_out"],
        hysteresis_loops=loops,
        n_cycles=hyst_df["n_cycles"].tolist(),
        creep=hyst_df["creep"].tolist(),
        hysteresis_area=hyst_df["hysteresis_area"].tolist(),
        stiffness=hyst_df["stiffness"].tolist(),
        total_dissipated_energy=np.sum(hyst_df["hysteresis_area"]),
        stress_at_failure=max_stress,
        strain_at_failure=hyst_df["creep"].iloc[-1],
        n_fail=n_fail,
        warning_messages=warning_triggered,
        crack_displacement=[],
        crack_load=[],
        crack_length=[],
        crack_n_cycles=[], 
        da_dN=[],
        G_MBT=[],
        G_MCC=[],
        G_ECM=[],
        c_paris_mbt=None,
        m_paris_mbt=None,
        G_th_mbt=None,
        c_paris_mcc=None,
        m_paris_mcc=None,
        G_th_mcc=None,
        c_paris_ecm=None,
        m_paris_ecm=None,
        G_th_ecm=None,
    )

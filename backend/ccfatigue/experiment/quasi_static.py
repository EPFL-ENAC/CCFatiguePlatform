"""
quasi_static.py
Pipeline for a single quasi-static test (with or without fracture energy calculation).
Local dependencies:

• quasistatic_with_fracture_utils → compliance-fit & G-methods
• quasistatic_utils               → toughness, Young’s modulus E, Poisson’s ratio ν
"""

from __future__ import annotations
from typing import Callable, Dict, List, Optional, Pattern

import os
import numpy as np
import pandas as pd
from pandas import DataFrame
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from pydantic import BaseModel

from ccfatigue.experiment.common import DATA_DIRECTORY, get_test_fields
from ccfatigue.models.database import Experiment, Test

# ───── helper modules ────────────────────────────────────────────────
from ccfatigue.experiment.quasistatic_with_fracture_utils import (
    compute_compliance_and_crack_length_fitted,
    compute_factors,
    compute_g_mbt,
    compute_g_mcc,
    compute_g_ecm,
)
from ccfatigue.experiment.quasistatic_utils import (
    calc_toughness,
    calc_young_modulus,
    calc_poisson_ratio,
)

# ───── response dataclass ───────────────────────────────────────────
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
    toughness: Optional[float]
    initial_crack_length: Optional[float]
    young_modulus: Optional[float]
    poisson_ratio: Optional[float]
    crack_fractureenergy_mbt: List[float]
    crack_fractureenergy_mcc: List[float]
    crack_fractureenergy_ecm: List[float]
    g_init_mbt: Optional[float]
    bridginglength_mbt: Optional[float]
    g_plateau_mbt: Optional[float]
    g_init_mcc: Optional[float]
    bridginglength_mcc: Optional[float]
    g_plateau_mcc: Optional[float]
    g_init_ecm: Optional[float]
    bridginglength_ecm: Optional[float]
    g_plateau_ecm: Optional[float]

# ───── local I/O helpers ────────────────────────────────────────────
def _csv_path(exp: Dict[str, str], fname: str) -> str:
    researcher = exp["researcher"].split(" ")[-1]
    folder     = f"TST_{researcher}_{exp['date']}_{exp['experiment_type']}"
    return os.path.abspath(os.path.join(DATA_DIRECTORY, folder, fname))


def get_dataframe(exp: Dict[str, str], specimen_id: int) -> DataFrame:
    return pd.read_csv(_csv_path(exp, f"measure_{specimen_id:03d}.csv"))


def get_test_metadata(exp: Dict[str, str], specimen_id: int) -> Dict:
    tests = pd.read_csv(_csv_path(exp, "tests.csv"))
    return tests[tests["sequential number"] == specimen_id].to_dict("records")[0]


# ───── regex helpers (immutate logic pre-esistente) ─────────────────
from re import search as _re_search
def _filter_regex(vals: List[str], pattern: str | Pattern[str]) -> List[str]:
    return [v for v in vals if _re_search(pattern, v)]


def _filter_columns(
    df: DataFrame,
    columns: List[str],
    pattern: str | Pattern[str],
    fn: Callable[[float], float] = lambda x: x,
) -> Dict[str, List[float]]:
    cols = _filter_regex(columns, pattern)
    return {c: df[c].dropna().map(fn).tolist() for c in cols}


# ════════════════════════════════════════════════════════════════════
#  Main async entry
# ════════════════════════════════════════════════════════════════════
async def quasi_static_test(
    session: AsyncSession,
    experiment_id: int,
    test_id: int,
) -> Dict:

    # ── experiment meta ────────────────────────────────────────────
    exp_row = await session.execute(
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
    exp = exp_row.one()._asdict()
    is_fracture = exp.get("qs_experiment_type") == "fracture"

    # ── test meta ──────────────────────────────────────────────────
    test_meta = await get_test_fields(
        session, experiment_id, test_id,
        (Test.sequential_number, Test.specimen_name, Test.initial_crack_length),
    )
    specimen_id = test_meta["sequential_number"]

    df   = get_dataframe(exp, specimen_id)
    meta = get_test_metadata(exp, specimen_id)

    width     = meta.get("width")
    thickness = meta.get("thickness")
    l_prime   = meta.get("l_prime") or 0.0
    t_param   = meta.get("t")       or 0.0  # offset for MBT formula

    # =================================================================
    #  FRACTURE branch
    # =================================================================
    if is_fracture:

        u   = df.get("u",    pd.Series(dtype=float)).dropna().tolist()
        P   = df.get("Load", pd.Series(dtype=float)).dropna().tolist()
        a   = df.get("Crack_length", pd.Series(dtype=float)).dropna().tolist()
        P   = (np.asarray(P) / 1000.0).tolist()  # N → kN

        C, a_fit = compute_compliance_and_crack_length_fitted(u, P, a)
        F, N     = compute_factors(u, P, a_fit, thickness, t_param, l_prime)

        G_mbt, g0_mbt, Lb_mbt, Gpl_mbt = compute_g_mbt(C, u, P, a_fit, F, N, width)
        G_mcc, g0_mcc, Lb_mcc, Gpl_mcc = compute_g_mcc(C, u, P, a_fit, F, N, width, thickness)
        G_ecm, g0_ecm, Lb_ecm, Gpl_ecm = compute_g_ecm(C, u, P, a_fit, F, N, width)

        return QuasiStaticTest(
            specimen_name=test_meta["specimen_name"],
            specimen_id=specimen_id,
            crack_displacement=u,
            crack_load=P,
            crack_length=a_fit.tolist(),
            displacement={}, load={}, strain={}, stress={},
            toughness=None,
            initial_crack_length=test_meta["initial_crack_length"],
            young_modulus=None, poisson_ratio=None,
            crack_fractureenergy_mbt=G_mbt,
            crack_fractureenergy_mcc=G_mcc,
            crack_fractureenergy_ecm=G_ecm,
            g_init_mbt=g0_mbt,  bridginglength_mbt=Lb_mbt, g_plateau_mbt=Gpl_mbt,
            g_init_mcc=g0_mcc,  bridginglength_mcc=Lb_mcc, g_plateau_mcc=Gpl_mcc,
            g_init_ecm=g0_ecm,  bridginglength_ecm=Lb_ecm, g_plateau_ecm=Gpl_ecm,
        )

    # =================================================================
    #  MECHANICAL branch (nessun calcolo G)
    # =================================================================
    disp = {"u": df.get("u", pd.Series(dtype=float)).dropna().tolist()}
    load = {"Load": df.get("Load", pd.Series(dtype=float)).dropna().tolist()}

    strain = {}
    for col in ("exx", "eyy", "exy"):
        if col in df.columns:
            label = "Engineering strain" if col == "exx" else col
            strain[label] = df[col].dropna().tolist()

    stress = {}
    if "Load" in df.columns and width and thickness:
        area = width * thickness
        stress["Engineering stress"] = (df["Load"] / area).dropna().tolist()

    # ── proprietà meccaniche ─────────────────────────────────────────
    toughness = calc_toughness(
        stress.get("Engineering stress"),
        strain.get("Engineering strain"),
    )

    young_modulus = None
    poisson_ratio = None
    if exp.get("material_tested", "").lower() == "bulk adhesives":
        young_modulus = calc_young_modulus(
            stress.get("Engineering stress"),
            strain.get("Engineering strain"),
        )
        eyy_list = df["eyy"].dropna().tolist() if "eyy" in df.columns else None
        poisson_ratio = calc_poisson_ratio(
            strain.get("Engineering strain"),
            eyy_list,
        )

    return QuasiStaticTest(
        specimen_name=test_meta["specimen_name"],
        specimen_id=specimen_id,
        crack_displacement=[], crack_load=[], crack_length=[],
        displacement=disp, load=load, strain=strain, stress=stress,
        toughness=toughness,
        initial_crack_length=None,
        young_modulus=young_modulus,
        poisson_ratio=poisson_ratio,
        crack_fractureenergy_mbt=[], crack_fractureenergy_mcc=[], crack_fractureenergy_ecm=[],
        g_init_mbt=None, bridginglength_mbt=None, g_plateau_mbt=None,
        g_init_mcc=None, bridginglength_mcc=None, g_plateau_mcc=None,
        g_init_ecm=None, bridginglength_ecm=None, g_plateau_ecm=None,
    )

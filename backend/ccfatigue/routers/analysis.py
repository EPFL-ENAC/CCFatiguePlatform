"""
Handle /analysis requests
"""

from typing import List

from fastapi import APIRouter, File, Query, UploadFile

from ccfatigue.analyzer import (
    SnModel,
    run_cld,
    run_cycle_counting,
    run_damage_summation,
    run_fatigue_failure,
    run_sn_curve,
)
from ccfatigue.model import (
    CldMethod,
    CycleCountingMethod,
    DamageSummationMethod,
    FatigueFailureMethod,
    SnCurveMethod,
    SnCurveResult,
)

router = APIRouter(
    prefix="/analysis",
    tags=["analysis"],
)


@router.post("/cycleCounting/file", response_model=bytes)
async def run_cycle_counting_file(
    file: UploadFile = File(...),
    method: CycleCountingMethod = Query(...),
) -> bytes:
    return run_cycle_counting(file.file, method)


@router.post("/snCurve/file", response_model=SnCurveResult)
async def run_sn_curve_file(
    file: UploadFile = File(...),
    methods: List[SnCurveMethod] = Query(...),
    r_ratios: List[float] = Query(..., alias="rRatios"),
) -> SnCurveResult:
    return run_sn_curve(file.file, methods, r_ratios)


@router.post("/cld/file", response_model=bytes)
async def run_cld_file(
    file: UploadFile = File(...),
    method: CldMethod = Query(...),
) -> bytes:
    return run_cld(file.file, method)


@router.post("/fatigueFailure/file", response_model=bytes)
async def run_fatigue_failure_file(
    x_file: UploadFile = File(..., alias="xFile"),
    y_file: UploadFile = File(..., alias="yFile"),
    f_file: UploadFile = File(..., alias="fFile"),
    method: FatigueFailureMethod = Query(...),
    sn_model: SnModel = Query(..., alias="snModel"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    off_axis_angle: float = Query(..., alias="offAxisAngle"),
) -> bytes:
    return run_fatigue_failure(
        x_file.file,
        y_file.file,
        f_file.file,
        method,
        sn_model,
        desirable_angle,
        off_axis_angle,
    )


@router.post("/damageSummation/file", response_model=bytes)
async def run_damage_summation_file(
    snc_file: UploadFile = File(..., alias="sncFile"),
    cyc_file: UploadFile = File(..., alias="cycFile"),
    method: DamageSummationMethod = Query(...),
) -> bytes:
    return run_damage_summation(snc_file.file, cyc_file.file, method)

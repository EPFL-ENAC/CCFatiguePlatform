"""
Handle /analysis requests
"""
from fastapi import APIRouter, File, Query, UploadFile

from ccfatigue.analyzer import (
    FatigueModel,
    run_cld,
    run_cycle_counting,
    run_damage_summation,
    run_fatigue_failure,
    run_sn_curve,
)
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationMethod,
    FatigueFailureMethod,
    SnCurveMethod,
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


@router.post("/snCurve/file", response_model=AnalysisResult)
async def run_sn_curve_file(
    file: UploadFile = File(...),
    method: SnCurveMethod = Query(...),
) -> AnalysisResult:
    return run_sn_curve(file.file, method)


@router.post("/cld/file", response_model=bytes)
async def run_cld_file(
    file: UploadFile = File(...),
    method: CldMethod = Query(...),
    ucs: float = Query(...),
    uts: float = Query(...),
) -> bytes:
    return run_cld(file.file, method, ucs, uts)


@router.post("/fatigueFailure/file", response_model=AnalysisResult)
async def run_fatigue_failure_file(
    x_file: UploadFile = File(..., alias="xFile"),
    y_file: UploadFile = File(..., alias="yFile"),
    f_file: UploadFile = File(..., alias="fFile"),
    method: FatigueFailureMethod = Query(...),
    sn_model: FatigueModel = Query(..., alias="snModel"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    off_axis_angle: float = Query(..., alias="offAxisAngle"),
) -> AnalysisResult:
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

"""
Handle /analysis requests
"""

from fastapi import APIRouter, File, HTTPException, Query, UploadFile

from ccfatigue.analyzer import (
    FatigueModel,
    run_cld,
    run_cld_piecewiselinear_from_sn,
    run_cld_piecewisenonlinear_from_sn,
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
    confidence_interval: float | None = Query(None, alias="confidenceInterval"),
) -> AnalysisResult:
    try:
        return run_sn_curve(file.file, method, confidence_interval)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"{method} failed: {str(e)}")


@router.post("/cld/file", response_model=AnalysisResult)
async def run_cld_file(
    file: UploadFile = File(...),
    method: CldMethod = Query(...),
    ucs: float = Query(...),
    uts: float = Query(...),
    np_reference: float | None = Query(default=None),
    m0_init: float | None = Query(default=None),
    d_init: float | None = Query(default=None),
    alpha_t_init: float | None = Query(default=None),
    alpha_c_init: float | None = Query(default=None),
) -> AnalysisResult:
    return run_cld(
        file.file,
        method,
        ucs,
        uts,
        np_reference,
        m0_init,
        d_init,
        alpha_t_init,
        alpha_c_init,
    )


@router.post("/cld/piecewiselinear-from-sn/file", response_model=AnalysisResult)
async def run_cld_piecewiselinear_from_sn_file(
    file: UploadFile = File(...),
    sn_method: SnCurveMethod = Query(..., alias="snMethod"),
    ucs: float = Query(...),
    uts: float = Query(...),
    confidence_interval: float | None = Query(None, alias="confidenceInterval"),
) -> AnalysisResult:
    try:
        return run_cld_piecewiselinear_from_sn(
            file.file, sn_method, ucs, uts, confidence_interval
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"PiecewiseLinear from SN failed: {str(e)}",
        )


@router.post("/cld/piecewisenonlinear-from-sn/file", response_model=AnalysisResult)
async def run_cld_piecewisenonlinear_from_sn_file(
    file: UploadFile = File(...),
    sn_method: SnCurveMethod = Query(..., alias="snMethod"),
    ucs: float = Query(...),
    uts: float = Query(...),
    confidence_interval: float | None = Query(None, alias="confidenceInterval"),
) -> AnalysisResult:
    try:
        return run_cld_piecewisenonlinear_from_sn(
            file.file, sn_method, ucs, uts, confidence_interval
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"PiecewiseNonLinear from SN failed: {str(e)}",
        )


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

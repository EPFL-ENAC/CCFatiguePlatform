"""
Handle /analysis requests
"""

from fastapi import APIRouter, File, HTTPException, Query, UploadFile

from ccfatigue.analyzer import (
    run_cld,
    run_cld_piecewiselinear_from_sn,
    run_cld_piecewisenonlinear_from_sn,
    run_cycle_counting,
    run_damage_summation,
    run_fatigue_failure,
    run_fatigue_failure_fawazellyin,
    run_fatigue_failure_ftpf,
    run_fatigue_failure_hashinrotem,
    run_fatigue_failure_kawai,
    run_fatigue_failure_shokriehtaheri,
    run_fatigue_failure_simsbrogdon,
    run_sn_curve,
)
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationInputType,
    FatigueFailureMethod,
    SnCurveLoadType,
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
    load_type: SnCurveLoadType = Query(
        SnCurveLoadType.TENSILE, alias="loadType"
    ),
    angle: float | None = Query(0),
) -> AnalysisResult:
    try:
        return run_sn_curve(
            file.file,
            method,
            confidence_interval,
            load_type,
            angle,
        )
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
    try:
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
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"{method} failed: {str(e)}")


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


@router.post("/fatigueFailure/material/file", response_model=AnalysisResult)
async def run_fatigue_failure_material_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    method: FatigueFailureMethod = Query(...),
    target_angle: float = Query(..., alias="targetAngle"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure(
            material_file.file,
            method,
            target_angle,
            target_stress_ratio,
            target_static_strength,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"{method} failed: {str(e)}")


@router.post("/fatigueFailure/ftpf/file", response_model=AnalysisResult)
async def run_fatigue_failure_ftpf_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_ftpf(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"FTPF failed: {str(e)}")


@router.post("/fatigueFailure/hashinRotem/file", response_model=AnalysisResult)
async def run_fatigue_failure_hashinrotem_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_hashinrotem(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"HashinRotem failed: {str(e)}")


@router.post("/fatigueFailure/shokriehTaheri/file", response_model=AnalysisResult)
async def run_fatigue_failure_shokriehtaheri_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_shokriehtaheri(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"ShokriehTaheri failed: {str(e)}")


@router.post("/fatigueFailure/fawazEllyin/file", response_model=AnalysisResult)
async def run_fatigue_failure_fawazellyin_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_fawazellyin(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"FawazEllyin failed: {str(e)}")


@router.post("/fatigueFailure/kawai/file", response_model=AnalysisResult)
async def run_fatigue_failure_kawai_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_kawai(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Kawai failed: {str(e)}")


@router.post("/fatigueFailure/simsBrogdon/file", response_model=AnalysisResult)
async def run_fatigue_failure_simsbrogdon_file(
    material_file: UploadFile = File(..., alias="materialFile"),
    target_angle: float = Query(..., alias="targetAngle"),
    target_static_strength: float | None = Query(None, alias="targetStaticStrength"),
    target_stress_ratio: float | None = Query(None, alias="targetStressRatio"),
    agg_file: UploadFile | None = File(None, alias="aggFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_simsbrogdon(
            material_file.file,
            target_angle,
            target_static_strength,
            target_stress_ratio,
            agg_file.file if agg_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"SimsBrogdon failed: {str(e)}")


@router.post("/damageSummation/file", response_model=AnalysisResult)
async def run_damage_summation_file(
    load_history_file: UploadFile = File(..., alias="loadHistoryFile"),
    fatigue_data_file: UploadFile = File(..., alias="fatigueDataFile"),
    fatigue_data_type: DamageSummationInputType = Query(..., alias="fatigueDataType"),
    cycle_counting_method: CycleCountingMethod = Query(
        ..., alias="cycleCountingMethod"
    ),
    cld_method: CldMethod = Query(..., alias="cldMethod"),
    ucs: float = Query(..., gt=0),
    uts: float = Query(..., gt=0),
    sn_curve_method: SnCurveMethod | None = Query(None, alias="snCurveMethod"),
    confidence_interval: float | None = Query(None, alias="confidenceInterval"),
    load_type: SnCurveLoadType = Query(
        SnCurveLoadType.TENSILE, alias="loadType"
    ),
    angle: float | None = Query(0),
    factor_min: float = Query(0.4, alias="factorMin", gt=0),
    factor_max: float = Query(1.4, alias="factorMax", gt=0),
    factor_count: int = Query(11, alias="factorCount", ge=1, le=100),
) -> AnalysisResult:
    try:
        return run_damage_summation(
            load_history_file.file,
            fatigue_data_file.file,
            fatigue_data_type,
            cycle_counting_method,
            cld_method,
            ucs,
            uts,
            sn_curve_method,
            confidence_interval,
            load_type,
            angle,
            factor_min,
            factor_max,
            factor_count,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Damage summation failed: {e}")

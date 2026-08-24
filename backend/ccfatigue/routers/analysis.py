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
    run_fatigue_failure_fawazellyin,
    run_fatigue_failure_ftpf,
    run_fatigue_failure_hashinrotem,
    run_fatigue_failure_kawai,
    run_fatigue_failure_shokriehtaheri,
    run_sn_curve,
)
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationMethod,
    FatigueFailureMethod,
    HashinRotemPanelType,
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


@router.post("/fatigueFailure/file", response_model=AnalysisResult)
async def run_fatigue_failure_file(
    x_file: UploadFile = File(..., alias="xFile"),
    y_file: UploadFile = File(..., alias="yFile"),
    f_file: UploadFile = File(..., alias="fFile"),
    method: FatigueFailureMethod = Query(...),
    sn_model: FatigueModel = Query(..., alias="snModel"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    off_axis_angle: float = Query(..., alias="offAxisAngle"),
    off_axis_angle2: float | None = Query(default=None, alias="offAxisAngle2"),
    tensile_transverse_strength: float | None = Query(default=None),
    compressive_transverse_strength: float | None = Query(default=None),
    shear_strength: float | None = Query(default=None),
    tensile_strength1: float | None = Query(default=None),
    compressive_strength1: float | None = Query(default=None),
    tensile_strength2: float | None = Query(default=None),
    compressive_strength2: float | None = Query(default=None),
    tensile_strength_at_desirable_angle: float | None = Query(default=None),
    compressive_strength_at_desirable_angle: float | None = Query(default=None),
) -> AnalysisResult:
    try:
        return run_fatigue_failure(
            x_file.file,
            y_file.file,
            f_file.file,
            method,
            sn_model,
            desirable_angle,
            off_axis_angle,
            off_axis_angle2,
            tensile_transverse_strength,
            compressive_transverse_strength,
            shear_strength,
            tensile_strength1,
            compressive_strength1,
            tensile_strength2,
            compressive_strength2,
            tensile_strength_at_desirable_angle,
            compressive_strength_at_desirable_angle,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"{method} failed: {str(e)}")


@router.post("/fatigueFailure/ftpf/file", response_model=AnalysisResult)
async def run_fatigue_failure_ftpf_file(
    x_file: UploadFile = File(..., alias="xFile"),
    y_file: UploadFile = File(..., alias="yFile"),
    f_file: UploadFile = File(..., alias="fFile"),
    sn_model: FatigueModel = Query(..., alias="snModel"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    off_axis_angle: float = Query(..., alias="offAxisAngle"),
    xc_file: UploadFile | None = File(default=None, alias="xcFile"),
    yc_file: UploadFile | None = File(default=None, alias="ycFile"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_ftpf(
            x_file.file,
            y_file.file,
            f_file.file,
            sn_model,
            desirable_angle,
            off_axis_angle,
            xc_file.file if xc_file is not None else None,
            yc_file.file if yc_file is not None else None,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"FTPF failed: {str(e)}")


@router.post("/fatigueFailure/hashinRotem/file", response_model=AnalysisResult)
async def run_fatigue_failure_hashinrotem_file(
    x_file: UploadFile = File(..., alias="xFile"),
    panel2_file: UploadFile = File(..., alias="panel2File"),
    panel3_file: UploadFile = File(..., alias="panel3File"),
    sn_model: FatigueModel = Query(..., alias="snModel"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    off_axis_angle1: float = Query(..., alias="offAxisAngle1"),
    off_axis_angle2: float = Query(..., alias="offAxisAngle2"),
    tensile_transverse_strength: float = Query(...),
    shear_strength: float = Query(...),
    tensile_strength1: float = Query(...),
    tensile_strength2: float = Query(...),
    tensile_strength_at_desirable_angle: float = Query(...),
    panel2_type: HashinRotemPanelType = Query(..., alias="panel2Type"),
    panel3_type: HashinRotemPanelType = Query(..., alias="panel3Type"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_hashinrotem(
            x_file.file,
            panel2_file.file,
            panel3_file.file,
            sn_model,
            desirable_angle,
            off_axis_angle1,
            off_axis_angle2,
            tensile_transverse_strength,
            shear_strength,
            tensile_strength1,
            tensile_strength2,
            tensile_strength_at_desirable_angle,
            panel2_type,
            panel3_type,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"HashinRotem failed: {str(e)}")


@router.post("/fatigueFailure/shokriehTaheri/file", response_model=AnalysisResult)
async def run_fatigue_failure_shokriehtaheri_file(
    agg_file: UploadFile = File(..., alias="aggFile"),
    reference_angle: float = Query(..., alias="referenceAngle"),
    reference_stress_ratio: float = Query(..., alias="referenceStressRatio"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    target_stress_ratio: float = Query(..., alias="targetStressRatio"),
    tensile_axial_strength: float = Query(...),
    compressive_axial_strength: float = Query(...),
    tensile_transverse_strength: float = Query(...),
    compressive_transverse_strength: float = Query(...),
    shear_strength: float = Query(...),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_shokriehtaheri(
            agg_file.file,
            reference_angle,
            reference_stress_ratio,
            desirable_angle,
            target_stress_ratio,
            tensile_axial_strength,
            compressive_axial_strength,
            tensile_transverse_strength,
            compressive_transverse_strength,
            shear_strength,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"ShokriehTaheri failed: {str(e)}")


@router.post("/fatigueFailure/fawazEllyin/file", response_model=AnalysisResult)
async def run_fatigue_failure_fawazellyin_file(
    snc_file: UploadFile = File(..., alias="sncFile"),
    sn_model: FatigueModel = Query(..., alias="snModel"),
    reference_angle: float = Query(..., alias="referenceAngle"),
    reference_static_strength: float = Query(..., alias="referenceStaticStrength"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    target_stress_ratio: float = Query(..., alias="targetStressRatio"),
    target_static_strength: float = Query(..., alias="targetStaticStrength"),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_fawazellyin(
            snc_file.file,
            sn_model,
            reference_angle,
            reference_static_strength,
            desirable_angle,
            target_stress_ratio,
            target_static_strength,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"FawazEllyin failed: {str(e)}")


@router.post("/fatigueFailure/kawai/file", response_model=AnalysisResult)
async def run_fatigue_failure_kawai_file(
    agg_file: UploadFile = File(..., alias="aggFile"),
    reference_angle: float = Query(..., alias="referenceAngle"),
    reference_stress_ratio: float = Query(..., alias="referenceStressRatio"),
    reference_static_strength: float = Query(..., alias="referenceStaticStrength"),
    desirable_angle: float = Query(..., alias="desirableAngle"),
    target_stress_ratio: float = Query(..., alias="targetStressRatio"),
    tensile_axial_strength: float = Query(...),
    tensile_transverse_strength: float = Query(...),
    shear_strength: float = Query(...),
) -> AnalysisResult:
    try:
        return run_fatigue_failure_kawai(
            agg_file.file,
            reference_angle,
            reference_stress_ratio,
            reference_static_strength,
            desirable_angle,
            target_stress_ratio,
            tensile_axial_strength,
            tensile_transverse_strength,
            shear_strength,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Kawai failed: {str(e)}")


@router.post("/damageSummation/file", response_model=bytes)
async def run_damage_summation_file(
    snc_file: UploadFile = File(..., alias="sncFile"),
    cyc_file: UploadFile = File(..., alias="cycFile"),
    method: DamageSummationMethod = Query(...),
) -> bytes:
    return run_damage_summation(snc_file.file, cyc_file.file, method)

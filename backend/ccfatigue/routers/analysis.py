"""
Handle /analysis requests
"""

from typing import List

from fastapi import APIRouter, File, Query, UploadFile

from ccfatigue.analyzer import run_cycle_counting, run_sn_curve
from ccfatigue.model import CycleCountingMethod, SnCurveMethod, SnCurveResult

router = APIRouter(
    prefix="/analysis",
    tags=["analysis"],
)


@router.post("/snCurve/file", response_model=SnCurveResult)
async def run_sn_curve_file(
    file: UploadFile = File(...),
    methods: List[SnCurveMethod] = Query(...),
    r_ratios: List[float] = Query(..., alias="rRatios"),
) -> SnCurveResult:
    return run_sn_curve(file.file, methods, r_ratios)


@router.post("/cycleCounting/file", response_model=bytes)
async def run_cycle_counting_file(
    file: UploadFile = File(...),
    method: CycleCountingMethod = Query(...),
) -> bytes:
    return run_cycle_counting(file.file, method)

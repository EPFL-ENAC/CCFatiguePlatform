"""
Handle /analysis requests
"""

from typing import List
from fastapi import APIRouter, Query, UploadFile, File
from fastapi_pagination import Page
from ccfatigue.models.api import ExperimentModel
from ccfatigue.analyzer import run_sn_curve
from ccfatigue.model import SnCurveMethod, SnCurveResult

router = APIRouter(
    prefix="/analysis",
    tags=["analysis"],
)


@router.post("/snCurve/file", response_model=Page[ExperimentModel])
async def run_sn_curve_file(
    file: UploadFile = File(...),
    methods: List[SnCurveMethod] = Query(...),
    r_ratios: List[float] = Query(..., alias="rRatios"),
) -> SnCurveResult:
    return run_sn_curve(file.file, methods, r_ratios)

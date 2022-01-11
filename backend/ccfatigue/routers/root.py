"""
Handle / requests
"""

from typing import List
from fastapi import APIRouter
from ccfatigue import __name__, __version__
from ccfatigue.models import AppInfo, UnitInfo

router = APIRouter()


@router.get("/", response_model=AppInfo)
async def root():
    """
    Get AppInfo
    """
    return AppInfo(name=__name__, version=__version__)


@router.get("/units", response_model=List[UnitInfo])
async def get_units():
    """
    Get All standardized units
    """
    return [
        UnitInfo(subject=unit[0], unit=unit[1])
        for unit in (
            ("humidity", "%"),
            ("temperature", "°C"),
            ("pressure", "kN"),
            ("stress", "MPa"),
            ("time", "sec"),
            ("length", "mm"),
        )
    ]

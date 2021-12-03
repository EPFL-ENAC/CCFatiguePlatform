"""
Handle / requests
"""

from fastapi import APIRouter
from ccfatigue import __name__, __version__
from ccfatigue.models import AppInfo

router = APIRouter()


@router.get("/", response_model=AppInfo)
async def root():
    """
    Get AppInfo
    """
    return AppInfo(name=__name__, version=__version__)

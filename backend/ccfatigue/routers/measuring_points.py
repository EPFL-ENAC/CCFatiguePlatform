"""
Handle /measuring_points requests
"""

from fastapi import APIRouter, Depends
from fastapi_pagination import Page
from sqlalchemy.ext.asyncio import AsyncSession

from ccfatigue.models.api import TestMeasuringPointModel
from ccfatigue.models.database import Test_Measuring_Point
from ccfatigue.services.database import get_session
from ccfatigue.utils.fastapi import get_page

router = APIRouter(
    prefix="/measuring_points",
    tags=["measuring_points"],
)


@router.get("", response_model=Page[TestMeasuringPointModel])
async def get_measuring_points(
    test_id: int,
    session: AsyncSession = Depends(get_session),
) -> Page[TestMeasuringPointModel]:
    """
    Get all measuring_points for specific test
    """
    page = await get_page(
        Test_Measuring_Point,
        TestMeasuringPointModel,
        session,
        Test_Measuring_Point.test_id == test_id,
    )
    return page

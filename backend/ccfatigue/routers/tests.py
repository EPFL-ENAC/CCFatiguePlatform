"""
Handle /tests requests
"""

from fastapi import APIRouter, Depends
from fastapi_pagination import Page
from sqlalchemy.ext.asyncio import AsyncSession

from ccfatigue.models.api import TestModel
from ccfatigue.models.database import Test
from ccfatigue.services.database import get_session
from ccfatigue.utils.fastapi import get_page

router = APIRouter(
    prefix="/tests",
    tags=["tests"],
)


@router.get("", response_model=Page[TestModel])
async def get_tests(
    experiment_id: int,
    session: AsyncSession = Depends(get_session),
) -> Page[TestModel]:
    """
    Get all tests for specific experiment
    """
    page = await get_page(
        Test,
        TestModel,
        session,
        Test.experiment_id == experiment_id,
    )
    return page

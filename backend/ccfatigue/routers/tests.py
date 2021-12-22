"""
Handle /tests requests
"""

from fastapi import APIRouter, Depends
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Test
from ccfatigue.models.api import TestModel


router = APIRouter(
    prefix="/tests",
    tags=["tests"],
)


@router.get("", response_model=Page[TestModel])
async def get_tests(
    experiment_id: int,
    session: AsyncSession = Depends(get_session),
):
    """
    Get all tests for specific experiment
    """
    return await paginate(
        session,
        select(Test).where(Test.experiment_id == experiment_id),
    )

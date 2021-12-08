"""
Handle /experiments requests
"""

from fastapi import APIRouter, Depends
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Experiment
from ccfatigue.models.api import ExperimentModel


router = APIRouter(
    prefix="/experiments",
    tags=["experiments"],
)


@router.get("", response_model=Page[ExperimentModel])
async def get_experiments(
    session: AsyncSession = Depends(get_session),
):
    """
    Get all experiments
    """
    return await paginate(session, select(Experiment))

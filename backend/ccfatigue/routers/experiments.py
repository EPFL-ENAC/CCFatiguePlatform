'''
Handle /experiments requests
'''

from typing import List
from fastapi import APIRouter, Depends
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Experiment
from ccfatigue.models.api import ExperimentModel


router = APIRouter(
    prefix='/experiments',
    tags=['experiments'],
)


@router.get('', response_model=List[ExperimentModel])
async def get_experiments(
    session: AsyncSession = Depends(get_session),
):
    '''
    Get all experiments
    '''
    result = await session.execute(select(Experiment))
    return result.scalars().all()

"""
Handle /experiments requests
"""

from typing import List, Any, Optional
from fastapi import APIRouter, Depends, Query, Path
from sqlalchemy import and_
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Experiment
from ccfatigue.models.api import ExperimentModel, ExperimentFieldNames
from ccfatigue.utils.routers import get_where_clauses


router = APIRouter(
    prefix="/experiments",
    tags=["experiments"],
)


@router.get("", response_model=Page[ExperimentModel])
async def get_experiments(
    session: AsyncSession = Depends(get_session),
    query: Optional[str] = Query(""),
    text_search: Optional[str] = Query(""),
):
    """
    Get all experiments
    """
    query_where_clauses: List[Any] = get_where_clauses(query, Experiment)
    text_search_where_clauses: List[Any] = get_where_clauses(
        f"%{text_search}%", Experiment
    )
    return await paginate(
        session,
        select(Experiment).where(
            and_(*query_where_clauses, *text_search_where_clauses)
        ),
    )


@router.get("/{field}/distinct", response_model=List[str])
async def get_field_distinct(
    session: AsyncSession = Depends(get_session),
    field: ExperimentFieldNames = Path(...),
):
    """
    Get all distinct values for field column, sorted
    """
    return sorted(
        [
            item[0]
            for item in await session.execute(
                select(Experiment.__dict__[field.value]).distinct()
            )
        ]
    )

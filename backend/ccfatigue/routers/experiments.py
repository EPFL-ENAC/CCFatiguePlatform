"""
Handle /experiments requests
"""

from typing import List, Any, Optional
from fastapi import APIRouter, Depends, Query
from sqlalchemy import and_
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from sqlalchemy.orm.attributes import InstrumentedAttribute
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Experiment
from ccfatigue.models.api import ExperimentModel
from ccfatigue.utils.routers import get_where_clauses


router = APIRouter(
    prefix="/experiments",
    tags=["experiments"],
)


@router.get("", response_model=Page[ExperimentModel])
async def get_experiments(
    session: AsyncSession = Depends(get_session),
    query: Optional[str] = Query(None),
    text_search: Optional[str] = Query(None),
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


@router.get("/fracture_mode/distinct", response_model=List[str])
async def get_fracture_mode_distinct(
    session: AsyncSession = Depends(get_session),
):
    """
    Get all distinct values for fracture_mode, sorted
    """
    return await get_field_distinct(Experiment.fracture_mode, session)


@router.get("/material_type_fiber_material/distinct", response_model=List[str])
async def get_material_type_fiber_material_distinct(
    session: AsyncSession = Depends(get_session),
):
    """
    Get all distinct values for material_type_fiber_material, sorted
    """
    return await get_field_distinct(Experiment.material_type_fiber_material, session)


@router.get("/material_type_resin/distinct", response_model=List[str])
async def get_material_type_resin_distinct(
    session: AsyncSession = Depends(get_session),
):
    """
    Get all distinct values for material_type_resin, sorted
    """
    return await get_field_distinct(Experiment.material_type_resin, session)


@router.get(
    "/laminates_and_assemblies_stacking_sequence/distinct", response_model=List[str]
)
async def get_laminates_and_assemblies_stacking_sequence_distinct(
    session: AsyncSession = Depends(get_session),
):
    """
    Get all distinct values for laminates_and_assemblies_stacking_sequence, sorted
    """
    return await get_field_distinct(
        Experiment.laminates_and_assemblies_stacking_sequence, session
    )


async def get_field_distinct(
    field: InstrumentedAttribute,
    session: AsyncSession = Depends(get_session),
):
    """
    Get all distinct values for one column field in the DB, sorted
    """
    return sorted([item[0] for item in await session.execute(select(field).distinct())])

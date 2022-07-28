"""
Handle /experiments requests
"""

import json
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
from ccfatigue.utils.bokeh_plots import generate_tests_dashboard_plots
from ccfatigue.utils.echarts_plots import (
    generate_data_tests_dashboard_plots,
    DashboardPlots,
    encoderNp,
)

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


@router.get("/data_tests_dashboard_plots", response_model=DashboardPlots)
async def get_data_tests_dashboard_plots(
    session: AsyncSession = Depends(get_session),
    experiment_id: int = Query(""),
    test_ids: List[int] = Query([]),
):
    """
    Return data for the 4 echarts plots used in Test Dashboard

    Note: this serves the same data as get_test_dashboard_plots,
    and was created to migrate to echarts,
    if successful the Bokeh utils and routes should be removed
    Warning : this serves *data* not Bokeh plots themselves
    """
    transposed_test_ids = [((test_id - 1) % 10) + 1 for test_id in test_ids]
    dashboard_plots = await generate_data_tests_dashboard_plots(
        session, 1, transposed_test_ids
    )
    for i in range(len(test_ids)):
        dashboard_plots.tests[i].test_id = test_ids[i]

    return json.loads(
        dashboard_plots.json(encoder=encoderNp), parse_constant=lambda n: 0
    )


@router.get("/tests_dashboard_plots", response_model=DashboardPlots)
async def get_tests_dashboard_plots(
    session: AsyncSession = Depends(get_session),
    experiment_id: int = Query(""),
    test_ids: List[int] = Query([]),
):
    """
    Return the 4 Bokeh plots used in Test Dashboard

    Note: as we don't have real data yet, we hard code things this so it will
    render the 10 first tests of the experiment 1 (only experiment we have) :
    + experiment=1
    + 1<tests_ids<10
    then we mascarade test_id field so that it looks like
    to be matching the one asked for.
    """
    transposed_test_ids = [((test_id - 1) % 10) + 1 for test_id in test_ids]
    dashboard_plots = await generate_tests_dashboard_plots(
        session, 1, transposed_test_ids
    )
    for i in range(len(test_ids)):
        dashboard_plots.tests[i].test_id = test_ids[i]
    return dashboard_plots

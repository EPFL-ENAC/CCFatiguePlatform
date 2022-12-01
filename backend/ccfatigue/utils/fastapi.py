from math import isnan
from typing import Any, List, Set, TypeVar

from fastapi import FastAPI
from fastapi.routing import APIRoute
from fastapi_pagination.default import Page
from pydantic.main import BaseModel
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.sql.elements import BooleanClauseList, ColumnElement
from sqlalchemy.sql.schema import Column

from ccfatigue.services.database import Base
from ccfatigue.utils.pagination import paginate

D = TypeVar("D", bound=Base)  # type: ignore
M = TypeVar("M", bound=BaseModel)


def __get_non_nan(value: Any) -> Any:
    if isinstance(value, float) and isnan(value):
        return None
    else:
        return value


async def get_page(
    database_type: type[D],
    model_type: type[M],
    session: AsyncSession,
    predicate: BooleanClauseList | ColumnElement,
) -> Page[M]:
    model_keys: Set[str] = set(model_type.__fields__.keys())
    model_columns: List[Column] = [
        column for key, column in database_type.__dict__.items() if key in model_keys
    ]
    page = await paginate(
        session,
        select(*model_columns).where(predicate),
        model_type,
        __get_non_nan,
    )
    return page


def use_route_names_as_operation_ids(app: FastAPI) -> None:
    """
    Simplify operation IDs so that generated API clients have simpler function
    names.
    Should be called only after all routes have been added.
    https://fastapi.tiangolo.com/advanced/path-operation-advanced-configuration/#openapi-operationid
    """
    for route in app.routes:
        if isinstance(route, APIRoute):
            route.operation_id = route.name

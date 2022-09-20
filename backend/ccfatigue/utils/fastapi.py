from math import isnan
from typing import Any, List, Set, TypeVar
from sqlalchemy.ext.asyncio import AsyncSession
from fastapi_pagination.default import Page
from ccfatigue.utils.pagination import paginate
from pydantic.main import BaseModel
from sqlalchemy.sql.schema import Column
from ccfatigue.services.database import Base
from sqlalchemy import select
from sqlalchemy.sql.elements import BooleanClauseList, ColumnElement

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
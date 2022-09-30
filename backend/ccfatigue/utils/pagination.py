"""
https://uriyyo-fastapi-pagination.netlify.app/customization/
"""
from typing import Any, Callable, Generic, Optional, Type, TypeVar

from fastapi import Query
from fastapi_pagination.api import resolve_params
from fastapi_pagination.bases import AbstractParams
from fastapi_pagination.default import Page, Params
from fastapi_pagination.ext.sqlalchemy import paginate_query
from sqlalchemy import func, select
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.sql import Select

T = TypeVar("T")


class ExtendedParams(Params):
    size: int = Query(10, ge=1, le=1000, description="Page size")


class ExtendedPage(Page[T], Generic[T]):
    __params_type__ = ExtendedParams


async def paginate(
    session: AsyncSession,
    query: Select,
    type: Type[T],
    map_value: Callable[[Any], Any],
    params: Optional[AbstractParams] = None,
) -> Page[T]:
    params = resolve_params(params)

    total = await session.scalar(select(func.count()).select_from(query.subquery()))
    result = await session.execute(paginate_query(query, params))
    items = result.unique().all()
    mapped_items = [
        {k: map_value(v) for k, v in item._mapping.items()} for item in items
    ]
    return ExtendedPage.create(
        [type(**item) for item in mapped_items],
        total,
        params,
    )

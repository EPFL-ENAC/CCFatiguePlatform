"""
Provides functions used by all routers

largely inspired by DT in resslabtool
"""

from decimal import Decimal
from distutils.util import strtobool
from typing import Any, List, Optional
from sqlalchemy import and_, or_, Enum
from sqlalchemy.sql.schema import Column
from sqlalchemy.sql.sqltypes import Boolean, Integer, Numeric, String


def get_where_clauses(query: Optional[str], table) -> List[Any]:
    """
    Get all where clauses
    it is formated as : key:value;key:value;...
    """
    if query:
        return [__get_where_clause(q, table) for q in query.split(";")]
    else:
        return []


def __get_where_clause(query: str, table) -> Any:
    """
    Get one where clause
    """
    if ":" not in query:
        return or_(
            *[column.cast(String).ilike(query) for column in table.__table__.columns]
        )
    key, expression = query.split(":", 2)
    column: Column[Any] = table.__dict__[key]
    return __get_predicate(column, expression)


def __get_predicate(column: Column[Any], expression: str) -> Any:
    if "&" in expression:
        return and_(*[__get_predicate(column, e) for e in expression.split("&")])
    if "," in expression:
        return column.in_(expression.split(","))
    if "<<" in expression:
        [min, max] = expression.split("<<", 2)
        return column.between(Decimal(min), Decimal(max))
    if expression.startswith("<"):
        return column < int(expression[1:])
    if expression.startswith(">"):
        return column > int(expression[1:])
    if isinstance(column.type, Enum):
        return column == expression
    if isinstance(column.type, String):
        return column.ilike(expression)
    if isinstance(column.type, Integer):
        return column == int(expression)
    if isinstance(column.type, Numeric):
        return column == Decimal(expression)
    if isinstance(column.type, Boolean):
        return column == bool(strtobool(expression))
    return column == expression

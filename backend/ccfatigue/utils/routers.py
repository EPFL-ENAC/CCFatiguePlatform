from decimal import Decimal
from distutils.util import strtobool
from typing import Any, List, Optional

from sqlalchemy import Enum, and_, or_
from sqlalchemy.sql.schema import Column
from sqlalchemy.sql.sqltypes import Boolean, Integer, Numeric, String

import logging

logger = logging.getLogger("uvicorn.debug")


def get_where_clauses(query: Optional[str], table) -> List[Any]:
    if not query:
        return []

    clauses = []
    for q in query.split(";"):
        if q == "fracture:true":
            clause = or_(
                table.__dict__["fa_experiment_type"] == "fracture",
                table.__dict__["qs_experiment_type"] == "fracture",
            )
        elif q == "fracture:false":
            clause = and_(
                or_(table.__dict__["fa_experiment_type"] != "fracture", table.__dict__["fa_experiment_type"].is_(None)),
                or_(table.__dict__["qs_experiment_type"] != "fracture", table.__dict__["qs_experiment_type"].is_(None)),
            )
        else:
            clause = __get_where_clause(q, table)

        clauses.append(clause)
        logger.debug(f"[DEBUG] WHERE clause added: {clause}")

    return clauses


def __get_where_clause(query: str, table) -> Any:
    if ":" not in query:
        return or_(
            *[column.cast(String).ilike(query) for column in table.__table__.columns]
        )

    key, expression = query.split(":", 1)

    # Special key handling
    if key == "fracture":
        if expression == "true":
            return or_(
                table.fa_experiment_type == "fracture",
                table.qs_experiment_type == "fracture"
            )
        elif expression == "false":
            return and_(
                or_(table.fa_experiment_type != "fracture", table.fa_experiment_type.is_(None)),
                or_(table.qs_experiment_type != "fracture", table.qs_experiment_type.is_(None))
            )
        else:
            raise ValueError(f"Invalid value for 'fracture': {expression}")

    # NOT handling
    negate = False
    if key.startswith("-"):
        key = key[1:]
        negate = True

    column: Column[Any] = table.__dict__[key]
    clause = __get_predicate(column, expression)
    return ~clause if negate else clause


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

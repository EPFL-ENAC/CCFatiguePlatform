import os
from typing import Any, Dict, List

from sqlalchemy import Column
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.config import settings
from ccfatigue.models.database import Test

DATA_DIRECTORY: str = os.path.join(settings.data_path, "preprocessed")  # type: ignore


async def get_test_fields(
    session: AsyncSession, experiment_id: int, test_id: int, fields: List[Column]
) -> Dict[str, Any]:
    values: Dict[str, Any] = (
        (
            await session.execute(
                select(*fields)
                .where(Test.experiment_id == experiment_id)
                .where(Test.id == test_id)
            )
        )
        .one()  # type: ignore
        ._asdict()
    )
    return values

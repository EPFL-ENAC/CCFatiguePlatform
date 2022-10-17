import os

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.config import settings
from ccfatigue.models.database import Test

DATA_DIRECTORY: str = os.path.join(settings.data_path, "preprocessed")  # type: ignore


async def get_specimen_id(
    session: AsyncSession, experiment_id: int, test_id: int
) -> int:
    specimen_id: int = await session.scalar(
        select(Test.specimen_number)
        .where(Test.experiment_id == experiment_id)
        .where(Test.id == test_id)
    )  # type: ignore
    return specimen_id

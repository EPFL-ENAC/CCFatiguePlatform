"""
Database Service
"""

from typing import AsyncIterator
from urllib.parse import quote_plus
from ccfatigue.config import settings
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine

__username = settings.postgres_user
__password = settings.postgres_password
__database = settings.postgres_db
__host = settings.postgres_host
__port = settings.postgres_port
__user = f'{__username}:{quote_plus(__password)}'
__socket = f'{__host}:{__port}'
__sync_url = f'postgresql://{__user}@{__socket}/{__database}'
__async_url = f'postgresql+asyncpg://{__user}@{__socket}/{__database}'

__async_engine = create_async_engine(__async_url, echo=True)
__async_session = sessionmaker(__async_engine, class_=AsyncSession)

Base = declarative_base(bind=__async_engine)


async def get_session() -> AsyncIterator[AsyncSession]:
    """
    Get session for FastAPI routers
    https://github.com/uriyyo/fastapi-pagination/blob/main/examples/pagination_async_sqlalchemy.py
    """
    async with __async_session() as session:
        yield session

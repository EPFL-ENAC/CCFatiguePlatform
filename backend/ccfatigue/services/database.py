"""
Database Service
"""

from typing import AsyncIterator
from urllib.parse import quote_plus

from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

from ccfatigue.config import settings

username = settings.postgres_user
password = settings.postgres_password
database = settings.postgres_db
host = settings.postgres_host
port = settings.postgres_port
user = f"{username}:{quote_plus(password)}"
socket = f"{host}:{port}"
sync_url = f"postgresql://{user}@{socket}/{database}"
async_url = f"postgresql+asyncpg://{user}@{socket}/{database}"

async_engine = create_async_engine(async_url, echo=True)
async_session = sessionmaker(async_engine, class_=AsyncSession)

Base = declarative_base(bind=async_engine)


async def get_session() -> AsyncIterator[AsyncSession]:
    """
    Get session for FastAPI routers
    https://github.com/uriyyo/fastapi-pagination/blob/main/examples/pagination_async_sqlalchemy.py
    """
    async with async_session() as session:
        yield session

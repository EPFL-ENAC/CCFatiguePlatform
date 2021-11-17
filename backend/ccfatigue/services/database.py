"""
Database Service
"""
from urllib.parse import quote_plus

import databases
import sqlalchemy
from ccfatigue.config import settings
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy_utils.functions.database import database_exists, create_database

__username = settings.postgres_user
__password = settings.postgres_password
__database = settings.postgres_db
__host = settings.postgres_host
__port = settings.postgres_port
__hostname = f'{quote_plus(__password)}@{__host}'
__url = f'postgresql://{__username}:{__hostname}:{__port}/{__database}'

database = databases.Database(__url)

if not database_exists(__url):
    print('DB does not exist -> create !')
    create_database(__url)
else:
    print('DB already exists')

engine = sqlalchemy.create_engine(__url, echo=True)

Base = declarative_base(bind=engine)

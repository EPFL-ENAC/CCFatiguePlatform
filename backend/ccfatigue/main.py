from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from ccfatigue import __name__, __version__

from ccfatigue.config import settings
from ccfatigue.routers import root
from ccfatigue.services.database import Base, database, engine

app = FastAPI(
    title=__name__,
    version=__version__,
    root_path=settings.root_path,  # type: ignore
)


if settings.cors_enabled:
    print('cors enabled')
else:
    print('cors disabled')
    app.add_middleware(
        CORSMiddleware,
        allow_origins=['*'],
        allow_credentials=True,
        allow_methods=['*'],
        allow_headers=['*'],
    )


Base.metadata.create_all(engine)


@app.on_event("startup")
async def startup():
    await database.connect()


@app.on_event("shutdown")
async def shutdown():
    await database.disconnect()

app.include_router(root.router)

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi_pagination import add_pagination

from ccfatigue import __name__, __version__
from ccfatigue.config import settings
from ccfatigue.routers import analysis, experiments, root, tests
from ccfatigue.utils.fastapi import use_route_names_as_operation_ids
from init_db import run_init_db

run_init_db()


app = FastAPI(
    title=__name__,
    version=__version__,
    root_path=settings.root_path,
)


if settings.cors_enabled:
    print("cors enabled")
else:
    print("cors disabled")
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

app.include_router(root.router)
app.include_router(experiments.router)
app.include_router(tests.router)
app.include_router(analysis.router)
add_pagination(app)
use_route_names_as_operation_ids(app)

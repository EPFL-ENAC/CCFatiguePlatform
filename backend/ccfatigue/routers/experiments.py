"""
Handle /experiments requests
"""

import glob
import tempfile
import zipfile
import subprocess
from typing import List, Any, Optional
from fastapi import APIRouter, Depends, Query, Path, File, UploadFile
from sqlalchemy import and_
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from ccfatigue.services.database import get_session
from ccfatigue.models.database import Experiment
from ccfatigue.models.api import ExperimentModel, ExperimentFieldNames
from ccfatigue.utils.routers import get_where_clauses
from ccfatigue.utils.bokeh_plots import (
    generate_tests_dashboard_plots,
    DashboardPlots,
)
from ccfatigue.model import Experiment_Data_Preprocessed
from preprocessing import tst_data_lib

router = APIRouter(
    prefix="/experiments",
    tags=["experiments"],
)


@router.get("", response_model=Page[ExperimentModel])
async def get_experiments(
    session: AsyncSession = Depends(get_session),
    query: Optional[str] = Query(""),
    text_search: Optional[str] = Query(""),
):
    """
    Get all experiments
    """
    query_where_clauses: List[Any] = get_where_clauses(query, Experiment)
    text_search_where_clauses: List[Any] = get_where_clauses(
        f"%{text_search}%", Experiment
    )
    return await paginate(
        session,
        select(Experiment).where(
            and_(*query_where_clauses, *text_search_where_clauses)
        ),
    )


@router.get("/{field}/distinct", response_model=List[str])
async def get_field_distinct(
    session: AsyncSession = Depends(get_session),
    field: ExperimentFieldNames = Path(...),
):
    """
    Get all distinct values for field column, sorted
    """
    return sorted(
        [
            item[0]
            for item in await session.execute(
                select(Experiment.__dict__[field.value]).distinct()
            )
            if item[0] is not None
        ]
    )


@router.get("/tests_dashboard_plots", response_model=DashboardPlots)
async def get_tests_dashboard_plots(
    session: AsyncSession = Depends(get_session),
    experiment_id: int = Query(""),
    test_ids: List[int] = Query([]),
):
    """
    Return the 4 Bokeh plots used in Test Dashboard

    Note: as we don't have real data yet, we hard code things this so it will
    render the 10 first tests of the experiment 1 (only experiment we have) :
    + experiment=1
    + 1<tests_ids<10
    then we mascarade test_id field so that it looks like
    to be matching the one asked for.
    """
    dashboard_plots = await generate_tests_dashboard_plots(
        session, experiment_id, test_ids
    )
    return dashboard_plots


@router.post("/data_preprocess_check", response_model=Experiment_Data_Preprocessed)
async def post_data_preprocess_check(
    session: AsyncSession = Depends(get_session),
    file: UploadFile = File(...),
):
    # Save uploaded zip to temp file
    with tempfile.TemporaryFile(
        mode="w+b", prefix="data_preprocess_check_zip"
    ) as tmp_uploaded_zip:
        tmp_uploaded_zip.write(file.file.read())
        # Create temp directory to unzip it all
        with tempfile.TemporaryDirectory(prefix="data_preprocess_check") as tmp_dir_fp:
            print(f"Created temp directory {tmp_dir_fp=}")
            # Unzip uploaded ZIP file
            with zipfile.ZipFile(tmp_uploaded_zip, "r") as zip_received:
                zip_received.extractall(tmp_dir_fp)
            # List all files unZipped
            tree_process_completed = subprocess.run(
                ["tree", "-a", tmp_dir_fp], capture_output=True
            )
            print(tree_process_completed.stdout.decode())
            # run preprocessing script on it
            with tst_data_lib.Logger(write_to_stdout=False) as logger:
                logger.info(f"Parsing experiment {file.filename}")

                exp_fp_folders = glob.glob(f"{tmp_dir_fp}/TST_*")

                for experiment_raw_fp_folder in exp_fp_folders:
                    print(f"Parsing experiment {experiment_raw_fp_folder}")
                    tst_data_lib.Experiment(experiment_raw_fp_folder, logger)

                if len(exp_fp_folders) == 0:
                    logger.error("No experiment folder found")

                # answer with output + success answer
                return Experiment_Data_Preprocessed(
                    output=logger.messages,
                    success=logger.error_count == 0,
                )

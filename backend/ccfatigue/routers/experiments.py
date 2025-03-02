"""
Handle /experiments requests
"""

import glob
import os
import subprocess
import tempfile
import zipfile
from typing import Any, List, Optional

from fastapi import APIRouter, Depends, File, Path, Query, UploadFile
from fastapi_pagination import Page
from fastapi_pagination.ext.async_sqlalchemy import paginate
from sqlalchemy import and_
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.experiment.fatigue import FatigueTest, fatigue_test
from ccfatigue.experiment.quasi_static import QuasiStaticTest, quasi_static_test
from ccfatigue.model import Experiment_Data_Preprocessed
from ccfatigue.models.api import ExperimentFieldNames, ExperimentModel
from ccfatigue.models.database import Experiment
from ccfatigue.services.database import get_session
from ccfatigue.utils.routers import get_where_clauses
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
    text_search_where_clauses: List[Any] = (
        get_where_clauses(f"%{text_search}%", Experiment) if text_search else []
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
            )  # type: ignore
            if item[0] is not None
        ]
    )


@router.get("/{experiment_id}/fatigue/{test_id}", response_model=FatigueTest)
async def get_fatigue_test(
    session: AsyncSession = Depends(get_session),
    experiment_id: int = Path(...),
    test_id: int = Path(),
) -> FatigueTest:
    """
    Return test result data
    """
    result = await fatigue_test(session, experiment_id, test_id)
    return result


@router.get("/{experiment_id}/quasi-static/{test_id}", response_model=QuasiStaticTest)
async def get_quasi_static_test(
    session: AsyncSession = Depends(get_session),
    experiment_id: int = Path(...),
    test_id: int = Path(),
) -> QuasiStaticTest:
    """
    Return quasi static test
    """
    result = await quasi_static_test(session, experiment_id, test_id)
    return result


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

                try:
                    exp_fp_folders = glob.glob(f"{tmp_dir_fp}/TST_*")
                    at_least_one_experiment = False
                    for experiment_raw_fp_folder in exp_fp_folders:
                        if os.path.isdir(experiment_raw_fp_folder):
                            print(f"Parsing experiment {experiment_raw_fp_folder}")
                            tst_data_lib.Experiment(experiment_raw_fp_folder, logger)
                            at_least_one_experiment = True
                        else:
                            logger.warning(
                                f"{experiment_raw_fp_folder} is not a directory, "
                                "skipping"
                            )

                    if not at_least_one_experiment:
                        logger.error("No experiment folder found")
                except Exception as e:
                    logger.error(f"Error while parsing experiment: {e}")

                # answer with output + success answer
                return Experiment_Data_Preprocessed(
                    output=logger.messages,
                    success=logger.error_count == 0,
                )

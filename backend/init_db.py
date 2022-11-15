#!/usr/bin/env python3
"""
- Run alembic DB migration
- Empty the DB from all previous content
- Inject all Experiments & Tests from Data/preprocessed folder
"""

import argparse
import glob
import json
import os
import re

import pandas as pd
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from alembic.command import upgrade
from alembic.config import Config
from ccfatigue.models.database import Experiment, Test, Test_Measuring_Point
from ccfatigue.services.database import sync_url

DATA_DIR = os.path.abspath(f"{__file__}/../../Data/preprocessed")
EXPERIMENTS_TO_INJECT = glob.glob(f"{DATA_DIR}/TST_*")


def alembic_upgrade():
    """
    Run database migration with alembic
    https://alembic.sqlalchemy.org/en/latest/api/commands.html
    """
    alembic_cfg = Config("alembic.ini")
    upgrade(alembic_cfg, "head")


def empty_database(session):
    """
    Empty all database content
    """
    session.query(Test_Measuring_Point).delete()
    session.query(Test).delete()
    session.query(Experiment).delete()
    session.execute("ALTER SEQUENCE experiment_id_seq RESTART WITH 1")
    session.execute("ALTER SEQUENCE test_id_seq RESTART WITH 1")
    session.execute("ALTER SEQUENCE test_measuring_point_id_seq RESTART WITH 1")
    session.commit()


def inject_exp_from_folder(exp_folder, session):
    """
    Inject Experiment + Test described in a dedicated folder
    Test is not yet implemented.
    """

    def to_bool_or_none(val):
        """
        return bool or None
        """
        if val is False or val is True:
            return val
        elif type(val) == str:
            if val.lower().startswith("y"):
                return True
            elif val.lower().startswith("n"):
                return False
        else:
            return None

    # Experiment loaded from JSON
    json_file = glob.glob(f"{exp_folder}/*.json")[0]
    with open(json_file, "r") as f:
        exp = json.load(f)

    experiment = Experiment(
        laboratory=exp["general"].get("laboratory", None),
        researcher=exp["general"]["researcher"],
        date=exp["general"]["date"],
        experiment_type=exp["general"]["experiment type"],
        fracture=exp["general"]["fracture"],
        fracture_mode=exp["general"].get("fracture mode", None),
        fatigue_test_type=exp["general"].get("fatigue test type", None),
        quasi_static_test_type=exp["general"].get("quasi-static test type", None),
        temperature_test_type=exp["general"].get("temperature test type", None),
        measuring_equipment=exp["general"].get("measuring equipment", None),
        reliability_level=exp["general"].get("reliability level", None),
        control_mode=exp["general"].get("control mode", None),
        publication_title=exp.get("publication", {}).get("title", None),
        publication_author=exp.get("publication", {}).get("author", None),
        publication_year=exp.get("publication", {}).get("year", None),
        publication_doi=exp.get("publication", {}).get("doi", None),
        publication_images_repository=exp.get("publication", {}).get(
            "images repository", None
        ),
        material_type_sample_type=exp.get("material type", {}).get("sample type", None),
        material_type_fiber_material=exp.get("material type", {}).get(
            "fiber material", None
        ),
        material_type_fiber_form=exp.get("material type", {}).get("fiber form", None),
        material_type_area_density=exp.get("material type", {}).get(
            "area density", None
        ),
        material_type_resin=exp.get("material type", {}).get("resin", None),
        material_type_hardener=exp.get("material type", {}).get("hardener", None),
        material_type_mixing_ratio=exp.get("material type", {}).get(
            "mixing ratio", None
        ),
        laminates_and_assemblies_curing_time=exp.get(
            "laminates and assemblies", {}
        ).get("curing time", None),
        laminates_and_assemblies_curing_temperature=exp.get(
            "laminates and assemblies", {}
        ).get("curing temperature", None),
        laminates_and_assemblies_curing_pressure=exp.get(
            "laminates and assemblies", {}
        ).get("curing pressure", None),
        laminates_and_assemblies_fiber_volume_ratio=exp.get(
            "laminates and assemblies", {}
        ).get("fiber volume ratio", None),
        laminates_and_assemblies_stacking_sequence=exp.get(
            "laminates and assemblies", {}
        ).get("stacking sequence", None),
        measurement_measuring_points=exp.get("measurement", {}).get(
            "measuring points", None
        ),
        dic_analysis_subset_size=exp.get("dic analysis", {}).get("subset size", None),
        dic_analysis_step_size=exp.get("dic analysis", {}).get("step size", None),
    )
    session.add(experiment)

    # Test data from CSV
    tests_csv_file = f"{exp_folder}/tests.csv"
    tests_df = pd.read_csv(
        tests_csv_file,
        dtype={
            "specimen number": "Int64",
            "specimen name": "str",
        },
        low_memory=False,
    )

    for (_, test_serie) in tests_df.iterrows():
        test = Test(
            experiment=experiment,
            specimen_number=test_serie.get("specimen number", default=None),
            specimen_name=test_serie.get("specimen name", default=None),
            stress_ratio=test_serie.get("stress ratio", default=None),
            maximum_stress=test_serie.get("maximum stress", default=None),
            frequency=test_serie.get("frequency", default=None),
            run_out=to_bool_or_none(test_serie.get("run out", default=None)),
            displacement_controlled_loading_rate=test_serie.get(
                "displacement controlled loading rate", default=None
            ),
            load_controlled_loading_rate=test_serie.get(
                "load controlled loading rate", default=None
            ),
            length=test_serie.get("length", default=None),
            width=test_serie.get("width", default=None),
            thickness=test_serie.get("thickness", default=None),
            temperature=test_serie.get("temperature", default=None),
            humidity=test_serie.get("humidity", default=None),
            initial_crack_length=test_serie.get("initial crack length", default=None),
        )
        session.add(test)

        for column in filter(
            lambda col: col.startswith("x coordinate of measuring point "),
            tests_df.columns,
        ):
            measuring_point_id = re.match(
                r"x coordinate of measuring point (\d+)$", column
            ).group(1)

            measuring_point = Test_Measuring_Point(
                test=test,
                measuring_point_id=measuring_point_id,
                x_coordinate=test_serie.get(
                    f"x coordinate of measuring point {measuring_point_id}",
                    default=None,
                ),
                y_coordinate=test_serie.get(
                    f"y coordinate of measuring point {measuring_point_id}",
                    default=None,
                ),
            )
            session.add(measuring_point)


def run_init_db():
    """
    Init DB
    - alembic upgrade
    - empty database
    - inject experiments in database
    """
    alembic_upgrade()

    sync_engine = create_engine(sync_url, echo=True)
    Session = sessionmaker(bind=sync_engine)
    session = Session()

    empty_database(session)

    for exp_folder in EXPERIMENTS_TO_INJECT:
        inject_exp_from_folder(exp_folder, session)

    session.commit()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Inject initial data from researcher Data into DB"
    )
    args = parser.parse_args()

    run_init_db()

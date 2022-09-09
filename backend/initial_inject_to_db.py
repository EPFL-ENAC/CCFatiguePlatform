#!/usr/bin/env python3

import os
import glob
import json
import argparse
from ccfatigue.models.database import Experiment  # TODO  , Test
from ccfatigue.services.database import __sync_url
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker


DATA_DIR = os.path.abspath(f"{__file__}/../../Data/preprocessed")
EXPERIMENTS_TO_INJECT = glob.glob(f"{DATA_DIR}/TST_*")
print(f"{EXPERIMENTS_TO_INJECT=}")


def inject_exp_from_folder(exp_folder, session):
    """
    Inject Experiment + Test described in a dedicated folder
    Test is not yet implemented.
    """
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
    # TODO


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Inject initial data from researcher Data into DB"
    )
    args = parser.parse_args()

    sync_engine = create_engine(__sync_url, echo=True)
    Session = sessionmaker(bind=sync_engine)
    session = Session()

    for exp_folder in EXPERIMENTS_TO_INJECT:
        inject_exp_from_folder(exp_folder, session)

    session.commit()

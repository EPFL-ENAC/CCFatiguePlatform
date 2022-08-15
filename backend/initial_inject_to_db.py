#!/usr/bin/env python3

import os
import glob
import json
import argparse
from ccfatigue.models.database import Experiment  # TODO  , Test
from ccfatigue.services.database import __sync_url
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker


DATA_DIR = os.path.abspath(f"{__file__}/../../Data")
REAL_EXPERIMENTS_TO_INJECT = [
    f"{DATA_DIR}/preprocessed/TST_Khalooei_2021-10_FA",
]


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
        laboratory=exp["General"]["Laboratory"],
        researcher=exp["General"]["Researcher"],
        date=exp["General"]["Date"],
        experiment_type=exp["General"]["Experiment Type"],
        fracture=exp["General"].get("Fracture", False),
        fracture_mode=exp["General"].get("Fracture Mode", None),
        initial_crack_length=exp["General"].get("Initial Crack length", None),
        fatigue_test_type=exp["General"].get("Fatigue Test Type", None),
        measuring_equipment=exp["General"].get("Measuring Equipment", None),
        reliability_level=exp["General"].get("Reliability Level", None),
        control_mode=exp["General"].get("Control mode", None),
        publication_title=exp.get("Publication", {}).get("Title", None),
        publication_author=exp.get("Publication", {}).get("Author", None),
        publication_year=exp.get("Publication", {}).get("Year", None),
        publication_doi=exp.get("Publication", {}).get("DOI", None),
        publication_images_repository=exp.get("Publication", {}).get(
            "Images Repository", None
        ),
        material_type_fiber_material=exp.get("Material Type", {}).get(
            "Fiber Material", None
        ),
        material_type_fiber_geometry=exp.get("Material Type", {}).get(
            "Fiber Geometry", None
        ),
        material_type_area_density=exp.get("Material Type", {}).get(
            "Area Density", None
        ),
        material_type_resin=exp.get("Material Type", {}).get("Resin", None),
        material_type_hardener=exp.get("Material Type", {}).get("Hardener", None),
        material_type_mixing_ratio=exp.get("Material Type", {}).get(
            "Mixing ratio", None
        ),
        geometry_length=exp["Geometry"]["Length"],
        geometry_width=exp["Geometry"]["Width"],
        geometry_thickness=exp["Geometry"]["Thickness"],
        laminates_and_assemblies_curing_time=exp.get(
            "Laminates and Assemblies", {}
        ).get("Curing Time", None),
        laminates_and_assemblies_curing_temperature=exp.get(
            "Laminates and Assemblies", {}
        ).get("Curing Temperature", None),
        laminates_and_assemblies_curing_pressure=exp.get(
            "Laminates and Assemblies", {}
        ).get("Curing Pressure", None),
        laminates_and_assemblies_fiber_content=exp.get(
            "Laminates and Assemblies", {}
        ).get("Fiber Content", None),
        laminates_and_assemblies_stacking_sequence=exp.get(
            "Laminates and Assemblies", {}
        ).get("Stacking Sequence", None),
        test_condtions_temperature=exp.get("Test condtions", {}).get(
            "Temperature", None
        ),
        test_condtions_humidity=exp.get("Test condtions", {}).get("Humidity", None),
        dic_analysis_subset_size=exp.get("DIC Analysis", {}).get("Subset Size", None),
        dic_analysis_step_size=exp.get("DIC Analysis", {}).get("Step Size", None),
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

    for exp_folder in REAL_EXPERIMENTS_TO_INJECT:
        inject_exp_from_folder(exp_folder, session)

    session.commit()

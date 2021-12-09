#!/usr/bin/env python3

import os
import re
import glob
import json
import random
from ccfatigue.models.database import Experiment, Test
from ccfatigue.services.database import __sync_url
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker


DATA_DIR = os.path.abspath(f"{__file__}/../../Data")
EXPERIMENTS_TO_INJECT = [
    f"{DATA_DIR}/TST_FakeResearcher_2019-09_QS",
    f"{DATA_DIR}/TST_FakeResearcher_2020-06_FA",
    f"{DATA_DIR}/TST_FakeResearcher_2021-06_FA",
    f"{DATA_DIR}/TST_Khalooei_2021-10_FA",
]


class RandomGen:
    def __init__(self):
        self.laboratories = ("LAFOO", "LABAR", "LABAZ")
        self.researchers = ("M. Tiger", "R. Blue", "R. Stoody", "G. Teez")
        self.experiment_types = ("FA", "QS")
        self.fracture_modes = ("Mode I", "Mode II", "Mode III", "Combined")
        self.fatigue_test_types = ("CA", "VA", "BL", "Combined")
        self.measuring_equipments = ("Laser ABC", "Fuzion CDE", "Roo XV")
        self.control_modes = ("Load Controlled", "Displacement Controlled")
        self.material_type_resins = (
            "Biresin® CR83",
            "Tripleresin® CR58",
            "Quadresin® CV102",
        )
        self.material_type_fiber_materials = (
            "E-glass fiber fabrics (EC 9-68)",
            "P-pong",
            "Z-thoopledge",
        )
        self.last_publication_num = 0

    def get_one(self, field):
        if field in ("stress_ratio", "maximum_stress", "loading_rate"):
            return random.random()
        if field in ("run_out",):
            return random.choice((True, False))

        if field == "laboratory":
            return random.choice(self.laboratories)
        if field == "researcher":
            return random.choice(self.researchers)
        if field == "date":
            return f"{random.randint(2010, 2021)}-{random.randint(1, 12):02}"
        if field == "experiment_type":
            return random.choice(self.experiment_types)
        if field in ("fracture",):
            return random.choice((True, False))
        if field == "fracture_mode":
            return random.choice(self.fracture_modes)
        if field in ("initial_crack_length",):
            return random.random() * 5.0
        if field == "fatigue_test_type":
            return random.choice(self.fatigue_test_types)
        if field == "measuring_equipment":
            return random.choice(self.measuring_equipments)
        if field in ("reliability_level",):
            return random.random()
        if field == "control_mode":
            return random.choice(self.control_modes)
        if field == "publication_title":
            self.last_publication_num += 1
            return f"fake {self.last_publication_num:03}"
        if field == "publication_doi":
            return f"fake DOI {self.last_publication_num:03}"
        if field == "publication_author":
            return "SB"
        if field == "publication_year":
            return random.randint(2010, 2021)
        if field in ("geometry_length", "geometry_width", "geometry_thickness"):
            return float(random.randint(10, 500))
        if field == "material_type_resin":
            return random.choice(self.material_type_resins)
        if field == "material_type_fiber_material":
            return random.choice(self.material_type_fiber_materials)

        if field in (
            "material_type_area_density",
            "laminates_and_assemblies_curing_time",
            "laminates_and_assemblies_curing_temperature",
            "laminates_and_assemblies_curing_pressure",
            "laminates_and_assemblies_fiber_content",
            "test_condtions_temperature",
            "test_condtions_humidity",
        ):
            return float(random.randint(10, 500)) / 10.0

        if field in ("dic_analysis_subset_size", "dic_analysis_step_size"):
            random.randint(1, 15)
        else:
            return ""


def generate_random_test(rnd_gen, experiment, specimen_number):
    """
    return randomly generated Test, linked to specified Experiment
    """
    return Test(
        experiment=experiment,
        specimen_number=specimen_number,
        stress_ratio=rnd_gen.get_one("stress_ratio"),
        maximum_stress=rnd_gen.get_one("maximum_stress"),
        loading_rate=rnd_gen.get_one("loading_rate"),
        run_out=rnd_gen.get_one("run_out"),
    )


def generate_random_experiment(rnd_gen):
    """
    return randomly generated Experiment
    """

    experiment_type = rnd_gen.get_one("experiment_type")
    if experiment_type == "FA":
        fatigue_test_type = rnd_gen.get_one("fatigue_test_type")
    else:
        fatigue_test_type = ""

    fracture = rnd_gen.get_one("fracture")
    if fracture:
        fracture_mode = rnd_gen.get_one("fracture_mode")
        initial_crack_length = rnd_gen.get_one("initial_crack_length")
    else:
        fracture_mode = None
        initial_crack_length = None

    return Experiment(
        laboratory=rnd_gen.get_one("laboratory"),
        researcher=rnd_gen.get_one("researcher"),
        date=rnd_gen.get_one("date"),
        experiment_type=experiment_type,
        fracture=fracture,
        fracture_mode=fracture_mode,
        initial_crack_length=initial_crack_length,
        fatigue_test_type=fatigue_test_type,
        measuring_equipment=rnd_gen.get_one("measuring_equipment"),
        reliability_level=rnd_gen.get_one("reliability_level"),
        control_mode=rnd_gen.get_one("control_mode"),
        publication_title=rnd_gen.get_one("publication_title"),
        publication_author=rnd_gen.get_one("publication_author"),
        publication_year=rnd_gen.get_one("publication_year"),
        publication_doi=rnd_gen.get_one("publication_doi"),
        publication_images_repository=rnd_gen.get_one("publication_images_repository"),
        material_type_fiber_material=rnd_gen.get_one("material_type_fiber_material"),
        material_type_fiber_geometry=rnd_gen.get_one("material_type_fiber_geometry"),
        material_type_area_density=rnd_gen.get_one("material_type_area_density"),
        material_type_resin=rnd_gen.get_one("material_type_resin"),
        material_type_hardener=rnd_gen.get_one("material_type_hardener"),
        material_type_mixing_ratio=rnd_gen.get_one("material_type_mixing_ratio"),
        geometry_length=rnd_gen.get_one("geometry_length"),
        geometry_width=rnd_gen.get_one("geometry_width"),
        geometry_thickness=rnd_gen.get_one("geometry_thickness"),
        laminates_and_assemblies_curing_time=rnd_gen.get_one(
            "laminates_and_assemblies_curing_time"
        ),
        laminates_and_assemblies_curing_temperature=rnd_gen.get_one(
            "laminates_and_assemblies_curing_temperature"
        ),
        laminates_and_assemblies_curing_pressure=rnd_gen.get_one(
            "laminates_and_assemblies_curing_pressure"
        ),
        laminates_and_assemblies_fiber_content=rnd_gen.get_one(
            "laminates_and_assemblies_fiber_content"
        ),
        laminates_and_assemblies_stacking_sequence=rnd_gen.get_one(
            "laminates_and_assemblies_stacking_sequence"
        ),
        test_condtions_temperature=rnd_gen.get_one("test_condtions_temperature"),
        test_condtions_humidity=rnd_gen.get_one("test_condtions_humidity"),
        dic_analysis_subset_size=rnd_gen.get_one("dic_analysis_subset_size"),
        dic_analysis_step_size=rnd_gen.get_one("dic_analysis_step_size"),
    )


if __name__ == "__main__":
    rnd_gen = RandomGen()
    sync_engine = create_engine(__sync_url, echo=True)
    Session = sessionmaker(bind=sync_engine)
    session = Session()

    for exp_fold in EXPERIMENTS_TO_INJECT:
        # Experiment loaded from JSON
        json_file = glob.glob(f"{exp_fold}/*.json")[0]
        with open(json_file, "r") as f:
            exp = json.load(f)["Experiment"]

        experiment = Experiment(
            laboratory=exp["General"]["Laboratory"],
            researcher=exp["General"]["Researcher"],
            date=exp["General"]["Date"],
            experiment_type=exp["General"]["Experiment Type"],
            fracture=exp["General"].get("Fracture", False),
            fracture_mode=exp["General"]["Fracture Mode"],
            initial_crack_length=exp["General"]["Initial Crack length"],
            fatigue_test_type=exp["General"]["Fatigue Test Type"],
            measuring_equipment=exp["General"]["Measuring Equipment"],
            reliability_level=exp["General"]["Reliability Level"],
            control_mode=exp["General"]["Control mode"],
            publication_title=exp["Publication"]["Title"],
            publication_author=exp["Publication"]["Author"],
            publication_year=exp["Publication"]["Year"],
            publication_doi=exp["Publication"]["DOI"],
            publication_images_repository=exp["Publication"]["Images Repository"],
            material_type_fiber_material=exp["Material Type"]["Fiber Material"],
            material_type_fiber_geometry=exp["Material Type"]["Fiber Geometry"],
            material_type_area_density=exp["Material Type"]["Area Density"],
            material_type_resin=exp["Material Type"]["Resin"],
            material_type_hardener=exp["Material Type"]["Hardener"],
            material_type_mixing_ratio=exp["Material Type"]["Mixing ratio"],
            geometry_length=exp["Geometry"]["Length"],
            geometry_width=exp["Geometry"]["Width"],
            geometry_thickness=exp["Geometry"]["Thickness"],
            laminates_and_assemblies_curing_time=exp["Laminates and Assemblies"][
                "Curing Time"
            ],
            laminates_and_assemblies_curing_temperature=exp["Laminates and Assemblies"][
                "Curing Temperature"
            ],
            laminates_and_assemblies_curing_pressure=exp["Laminates and Assemblies"][
                "Curing Pressure"
            ],
            laminates_and_assemblies_fiber_content=exp["Laminates and Assemblies"][
                "Fiber Content"
            ],
            laminates_and_assemblies_stacking_sequence=exp["Laminates and Assemblies"][
                "Stacking Sequence"
            ],
            test_condtions_temperature=exp["Test condtions"]["Temperature"],
            test_condtions_humidity=exp["Test condtions"]["Humidity"],
            dic_analysis_subset_size=exp["DIC Analysis"]["Subset Size"],
            dic_analysis_step_size=exp["DIC Analysis"]["Step Size"],
        )
        session.add(experiment)

        # Test data from CSV
        for csv_file in glob.glob(f"{exp_fold}/*.csv"):
            specimen_number = re.findall(r"\d+", csv_file)[-1]
            test = generate_random_test(rnd_gen, experiment, specimen_number)
            session.add(test)

    # Generate 30 fake Experiments + test data
    for _ in range(30):
        experiment = generate_random_experiment(rnd_gen)
        session.add(experiment)
        for i in range(random.randint(5, 20)):
            test = generate_random_test(rnd_gen, experiment, f"{i:02}")
            session.add(test)

    session.commit()

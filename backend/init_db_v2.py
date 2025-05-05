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
from ccfatigue.models.database_v2 import Experiment, Test, Test_Measuring_Point
from ccfatigue.services.database import sync_url

DATA_DIR = os.path.abspath(f"{__file__}/../../Data/preprocessed")
EXPERIMENTS_TO_INJECT = glob.glob(f"{DATA_DIR}/TST_*")

def alembic_upgrade():
    alembic_cfg = Config("alembic.ini")
    upgrade(alembic_cfg, "head")

def empty_database(session):
    session.query(Test_Measuring_Point).delete()
    session.query(Test).delete()
    session.query(Experiment).delete()
    session.execute("ALTER SEQUENCE experiment_id_seq RESTART WITH 1")
    session.execute("ALTER SEQUENCE test_id_seq RESTART WITH 1")
    session.execute("ALTER SEQUENCE test_measuring_point_id_seq RESTART WITH 1")
    session.commit()

def to_bool_or_none(val):
    if isinstance(val, bool):
        return val
    if isinstance(val, str):
        val = val.strip().lower()
        if val.startswith("y"):
            return True
        elif val.startswith("n"):
            return False
    return None

def inject_exp_from_folder(exp_folder, session):
    json_file = glob.glob(f"{exp_folder}/*.json")[0]
    with open(json_file, "r") as f:
        exp = json.load(f)

    is_fracture = (
        exp["general"].get("fa experiment type", "").strip().lower() == "fracture"
        or exp["general"].get("qs experiment type", "").strip().lower() == "fracture"
    )

    experiment = Experiment(
        laboratory=exp["general"].get("laboratory"),
        researcher=exp["general"]["researcher"],
        date=exp["general"]["date"],
        experiment_type=exp["general"].get("experiment type"),
        fa_experiment_type=exp["general"].get("fa experiment type"),
        qs_experiment_type=exp["general"].get("qs experiment type"),
        ot_experiment_type=exp["general"].get("ot experiment type"),
        ot_add_info=exp["general"].get("ot add info"),
        fracture_mode_fm=exp["general"].get("fracture mode (fm)"),
        fm_add_info=exp["general"].get("fm add info"),
        control_mode=exp["general"].get("control mode"),
        fatigue_loading_type_flt=exp["general"].get("fatigue loading type (flt)"),
        flt_add_info=exp["general"].get("flt add info"),
        measuring_equipment=exp["general"].get("measuring equipment"),
        loading_rate=exp["general"].get("loading rate"),
        publication_doi=exp.get("publication", {}).get("doi"),
        material_tested=exp.get("material info", {}).get("material tested"),
        material_type_sample_type=exp.get("material info", {}).get("sample type"),
        sample_type_add_info=exp.get("material info", {}).get("sample type add info"),
        material_type_fiber_material=exp.get("material info", {}).get("fiber material"),
        material_type_fiber_form=exp.get("material info", {}).get("fiber form"),
        material_type_area_density=exp.get("material info", {}).get("area density"),
        material_type_resin=exp.get("material info", {}).get("resin"),
        material_type_hardener=exp.get("material info", {}).get("hardener"),
        material_type_mixing_ratio=exp.get("material info", {}).get("mixing ratio"),
        other_polymers_add_info=exp.get("material info", {}).get("other polymers add info"),
        curing_time=exp.get("material info", {}).get("curing time"),
        curing_temperature=exp.get("material info", {}).get("curing temperature"),
        curing_pressure=exp.get("material info", {}).get("curing pressure"),
        postcuring_time=exp.get("material info", {}).get("postcuring time"),
        postcuring_temperature=exp.get("material info", {}).get("postcuring temperature"),
        postcuring_pressure=exp.get("material info", {}).get("postcuring pressure"),
        glue=exp.get("material info", {}).get("glue"),
        glue_curing_time=exp.get("material info", {}).get("glue curing time"),
        glue_curing_temperature=exp.get("material info", {}).get("glue curing temperature"),
        glue_curing_pressure=exp.get("material info", {}).get("glue curing pressure"),
        laminates_and_assemblies_stacking_sequence=exp.get("laminates and assemblies", {}).get("stacking sequence"),
        laminates_and_assemblies_fiber_volume_ratio=exp.get("laminates and assemblies", {}).get("fiber volume ratio"),
        fatigue_r_ratio=exp.get("fatigue", {}).get("r ratio"),
        fatigue_frequency=exp.get("fatigue", {}).get("frequency"),
    )
    session.add(experiment)

    tests_csv_file = f"{exp_folder}/tests.csv"
    tests_df = pd.read_csv(tests_csv_file, low_memory=False)

    for (_, test_serie) in tests_df.iterrows():
        test = Test(
            experiment=experiment,
            sequential_number=test_serie.get("sequential number"),
            specimen_name=test_serie.get("specimen name"),
            number_of_cycles=test_serie.get("number of cycles"),
            maximum_load=test_serie.get("maximum load"),
            run_out=to_bool_or_none(test_serie.get("run out")),
            length=test_serie.get("length"),
            width=test_serie.get("width"),
            thickness=test_serie.get("thickness"),
            initial_crack_length=test_serie.get("initial crack length"),
            temperature=test_serie.get("temperature"),
            humidity=test_serie.get("humidity"),
            subset_size=test_serie.get("subset size"),
            step_size=test_serie.get("step size"),
        )
        session.add(test)

        for column in filter(lambda col: col.startswith("x coordinate of measuring point "), tests_df.columns):
            mp_id = re.match(r"x coordinate of measuring point (\d+)$", column).group(1)
            measuring_point = Test_Measuring_Point(
                test=test,
                measuring_point_id=int(mp_id),
                x_coordinate=test_serie.get(f"x coordinate of measuring point {mp_id}"),
                y_coordinate=test_serie.get(f"y coordinate of measuring point {mp_id}"),
            )
            session.add(measuring_point)

def run_init_db():
    alembic_upgrade()
    engine = create_engine(sync_url, echo=True)
    Session = sessionmaker(bind=engine)
    session = Session()

    empty_database(session)

    for exp_folder in EXPERIMENTS_TO_INJECT:
        from ccfatigue.models.database import Experiment
        print("COLUMNS IN DB MODEL:")
        for col in Experiment.__table__.columns:
            print(" -", col.name)
        inject_exp_from_folder(exp_folder, session)

    session.commit()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Inject initial data from researcher Data into DB")
    args = parser.parse_args()
    run_init_db()

#!/usr/bin/env python3

import os
import re
import glob
import json
import random
from ccfatigue.models.database import Experiment, Test
from ccfatigue.services.database import engine
from sqlalchemy.orm import sessionmaker


DATA_DIR = os.path.abspath(f'{__file__}/../../Data')
EXPERIMENTS_TO_INJECT = [
    f'{DATA_DIR}/TST_FakeResearcher_2019-09_QS',
    f'{DATA_DIR}/TST_FakeResearcher_2020-06_FA',
    f'{DATA_DIR}/TST_FakeResearcher_2021-06_FA',
    f'{DATA_DIR}/TST_Khalooei_2021-10_FA',
]


def generate_random_test(experiment, specimen_number):
    '''
    return randomly generated Test, linked to specified Experiment
    '''
    return Test(
        experiment=experiment,

        specimen_number=specimen_number,
        stress_ratio=random.random(),
        maximum_stress=random.random(),
        loading_rate=random.random(),
        run_out=random.choice((True, False)),
    )


if __name__ == '__main__':
    Session = sessionmaker(bind=engine)
    session = Session()

    for exp_fold in EXPERIMENTS_TO_INJECT:
        # Experiment loaded from JSON
        json_file = glob.glob(f'{exp_fold}/*.json')[0]
        with open(json_file, 'r') as f:
            exp = json.load(f)['Experiment']

        experiment = Experiment(
            laboratory=exp['General']['Laboratory'],
            researcher=exp['General']['Researcher'],
            date=exp['General']['Date'],
            experiment_type=exp['General']['Experiment Type'],
            fracture=exp['General'].get('Fracture', False),
            fracture_mode=exp['General']['Fracture Mode'],
            initial_crack_length=exp['General']['Initial Crack length'],
            fatigue_test_type=exp['General']['Fatigue Test Type'],
            measuring_equipment=exp['General']['Measuring Equipment'],
            reliability_level=exp['General']['Reliability Level'],
            control_mode=exp['General']['Control mode'],

            publication_title=exp['Publication']['Title'],
            publication_author=exp['Publication']['Author'],
            publication_year=exp['Publication']['Year'],
            publication_doi=exp['Publication']['DOI'],
            publication_images_repository=exp['Publication'][
                'Images Repository'],

            material_type_fiber_material=exp['Material Type'][
                'Fiber Material'],
            material_type_fiber_geometry=exp['Material Type'][
                'Fiber Geometry'],
            material_type_area_density=exp['Material Type']['Area Density'],
            material_type_resin=exp['Material Type']['Resin'],
            material_type_hardener=exp['Material Type']['Hardener'],
            material_type_mixing_ratio=exp['Material Type']['Mixing ratio'],

            geometry_length=exp['Geometry']['Length'],
            geometry_width=exp['Geometry']['Width'],
            geometry_thickness=exp['Geometry']['Thickness'],

            laminates_and_assemblies_curing_time=exp[
                'Laminates and Assemblies']['Curing Time'],
            laminates_and_assemblies_curing_temperature=exp[
                'Laminates and Assemblies']['Curing Temperature'],
            laminates_and_assemblies_curing_pressure=exp[
                'Laminates and Assemblies']['Curing Pressure'],
            laminates_and_assemblies_fiber_content=exp[
                'Laminates and Assemblies']['Fiber Content'],
            laminates_and_assemblies_stacking_sequence=exp[
                'Laminates and Assemblies']['Stacking Sequence'],

            test_condtions_temperature=exp['Test condtions']['Temperature'],
            test_condtions_humidity=exp['Test condtions']['Humidity'],

            dic_analysis_subset_size=exp['DIC Analysis']['Subset Size'],
            dic_analysis_step_size=exp['DIC Analysis']['Step Size'],
        )
        session.add(experiment)

        # Test data from CSV
        for csv_file in glob.glob(f'{exp_fold}/*.csv'):
            specimen_number = re.findall(r'\d+', csv_file)[-1]
            test = generate_random_test(experiment, specimen_number)
            session.add(test)

        session.commit()

#!/usr/bin/env python3

import os
import json
from ccfatigue.models.database import Experience, Test
from ccfatigue.services.database import engine
from sqlalchemy.orm import sessionmaker


JSON_FILE = os.path.abspath(f'{__file__}/../Preprocessing/vahid_CA_skel.json')


def get_value_or_None(expected_type, value):
    '''
    cast value to the expected type.
    If raises an error, then return None
    '''
    try:
        return expected_type(value)
    except ValueError:
        return None


if __name__ == '__main__':
    with open(JSON_FILE, 'r') as f:
        data = json.load(f)

    test = Test(
        specimen_number=data['Test']['Specimen number'],
        number_of_cycles_to_failure=data['Test']['Number of Cycles to Failure']
    )

    experiment = Experience(
        laboratory=data['Experiment']['Laboratory'],
        researcher=data['Experiment']['Researcher'],
        experiment_type=data['Experiment']['Experiment Type'],

        publication_title=data['Experiment']['Publication']['Title'],
        publication_doi=data['Experiment']['Publication']['DOI'],

        geometry_length=get_value_or_None(int, data['Experiment']['Geometry']['Length']),
        geometry_width=get_value_or_None(int, data['Experiment']['Geometry']['Width']),
        geometry_thickness=get_value_or_None(int, data['Experiment']['Geometry']['Thickness']),

        laminates_and_assemblies_curing_time=get_value_or_None(int, data['Experiment']['Laminates and Assemblies']['Curing Time']),
        laminates_and_assemblies_curing_temperature=get_value_or_None(int, data['Experiment']['Laminates and Assemblies']['Curing Temperature']),
        laminates_and_assemblies_curing_pressure=get_value_or_None(int, data['Experiment']['Laminates and Assemblies']['Curing Pressure']),
        laminates_and_assemblies_fiber_content=get_value_or_None(int, data['Experiment']['Laminates and Assemblies']['Fiber Content']),
        laminates_and_assemblies_stacking_sequence=data['Experiment']['Laminates and Assemblies']['Stacking Sequence'],

        tests=[test, ]
    )

    Session = sessionmaker(bind=engine)
    session = Session()

    session.add(test)
    session.add(experiment)
    session.commit()

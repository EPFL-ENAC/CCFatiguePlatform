'''
Define the model as it is in the DB
'''

from ccfatigue.services.database import Base
from sqlalchemy import (Column, ForeignKey, Integer,
                        Float, String, Boolean, Enum)
from sqlalchemy.orm import relationship


class Experiment(Base):
    '''
    Defines how experiment data is structured in DB
    '''
    __tablename__ = 'experiment'
    id = Column(Integer, primary_key=True)

    laboratory = Column(String)
    researcher = Column(String)
    date = Column(String)
    experiment_type = Column(String)

    fracture = Column(Boolean)
    fracture_mode = Column(
        Enum('Mode I', 'Mode II', 'Mode III', 'Combined', '',
             name='fracture_mode'),
        default='',
    )
    initial_crack_length = Column(Float)

    fatigue_test_type = Column(
        Enum('CA', 'VA', 'BL', 'Combined', '',
             name='fatigue_test_type'),
        default='',
    )

    measuring_equipment = Column(String)
    reliability_level = Column(Float)

    control_mode = Column(
        Enum('Load Controlled', 'Displacement Controlled', '',
             name='control_mode'),
        default='',
    )

    publication_title = Column(String)
    publication_author = Column(String)
    publication_year = Column(String)
    publication_doi = Column(String)
    publication_images_repository = Column(String)

    material_type_fiber_material = Column(String)
    material_type_fiber_geometry = Column(String)
    material_type_area_density = Column(Float)
    material_type_resin = Column(String)
    material_type_hardener = Column(String)
    material_type_mixing_ratio = Column(String)

    geometry_length = Column(Float)
    geometry_width = Column(Float)
    geometry_thickness = Column(Float)

    laminates_and_assemblies_curing_time = Column(Float)
    laminates_and_assemblies_curing_temperature = Column(Float)
    laminates_and_assemblies_curing_pressure = Column(Float)
    laminates_and_assemblies_fiber_content = Column(Float)
    laminates_and_assemblies_stacking_sequence = Column(String)

    test_condtions_temperature = Column(Float)
    test_condtions_humidity = Column(Float)

    dic_analysis_subset_size = Column(Integer)
    dic_analysis_step_size = Column(Integer)

    tests = relationship('Test', cascade='all, delete')


class Test(Base):
    '''
    Defines how test part of experiment is structured in DB
    '''
    __tablename__ = 'test'
    id = Column(Integer, primary_key=True)

    experiment_id = Column(Integer, ForeignKey('experiment.id'))
    experiment = relationship('Experiment', back_populates='tests')

    specimen_number = Column(String)
    stress_ratio = Column(Float)
    maximum_stress = Column(Float)
    loading_rate = Column(Float)
    run_out = Column(Boolean)

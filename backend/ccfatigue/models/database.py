'''
Define the model as it is in the DB
'''

from ccfatigue.services.database import Base
from sqlalchemy import Column, ForeignKey, Integer, Float, String
from sqlalchemy.orm import relationship


class Experience(Base):
    '''
    Defines how experience data is structured in DB
    '''
    __tablename__ = 'experience'
    id = Column(Integer, primary_key=True)

    laboratory = Column(String)
    researcher = Column(String)
    experiment_type = Column(String)

    publication_title = Column(String)
    publication_doi = Column(String)

    geometry_length = Column(Integer)
    geometry_width = Column(Integer)
    geometry_thickness = Column(Integer)

    laminates_and_assemblies_curing_time = Column(Integer)
    laminates_and_assemblies_curing_temperature = Column(Integer)
    laminates_and_assemblies_curing_pressure = Column(Integer)
    laminates_and_assemblies_fiber_content = Column(Integer)
    laminates_and_assemblies_stacking_sequence = Column(String)

    tests = relationship('Test', cascade='all, delete')


class Test(Base):
    '''
    Defines how test part of experience is structured in DB
    '''
    __tablename__ = 'test'
    id = Column(Integer, primary_key=True)

    experience_id = Column(Integer, ForeignKey('experience.id'))
    experience = relationship('Experience', back_populates='tests')

    specimen_number = Column(String)
    number_of_cycles_to_failure = Column(Integer)
    stress_at_failure = Column(Float)

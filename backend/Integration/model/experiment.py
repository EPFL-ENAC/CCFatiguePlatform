from sqlalchemy import Column, Integer, String
from sqlalchemy import Float, Date, Enum, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship


## Experiment class ##
from Integration.base import Base


class Experiment(Base):
    __tablename__ = "experiment"
    id = Column(Integer, primary_key=True, autoincrement=True)
    Laboratory = Column(String, nullable=False)
    Researcher = Column(String, nullable=False)
    Date = Column(Date)
    Experiment_Type = Column(Enum('FA', 'QS', name='Experiment_Types'), nullable=False)
    Fracture = Column(Boolean, nullable=False)
    Fracture_Mode = Column(Enum('Mode I', 'Mode II', 'Mode III', 'Combined', name='Fracture_Mode'))
    Initial_Crack_length = Column(Float, nullable=False)
    Fatigue_Test_Type = Column(Enum('CA', 'VA', 'BL', 'Combined', name='Fatigue_Test_Type'))
    Measuring_Equipment = Column(String)
    Reliability_Level = Column(String)
    Control_mode = Column(Enum('Load Controlled', 'Displacement Controlled', name='Control_Mode'), nullable=False)
    Title = Column(String)
    Author = Column(String)
    Year = Column(String)
    DOI = Column(String)
    Images_Repository = Column(String)
    Fiber_Material = Column(String, nullable=False)
    Fiber_Geometry = Column(String)
    Area_Density = Column(Float)
    Resin = Column(String, nullable=False)
    Hardener = Column(String)
    Mixing_ratio = Column(String)
    Length = Column(Float, nullable=False)
    Width = Column(Float, nullable=False)
    Thickness = Column(Float, nullable=False)
    Curing_Time = Column(Float)
    Curing_Temperature = Column(Float)
    Curing_Pressure = Column(Float)
    Fiber_Content = Column(Float)
    Stacking_Sequence = Column(String, nullable=False)
    Temperature = Column(Float, nullable=False)
    Humidity = Column(Float, nullable=False)
    Subset_Size = Column(Integer)
    Step_Size = Column(Integer)
    children = relationship("Test", back_populates="parent")

def insertExperiment(experiment,DBSession):
    DBSession.add(experiment)
    DBSession.commit()


def bulkInsertExperiment(experiments,DBSession):
    DBSession.bulk_save_objects(experiments)
    DBSession.commit()
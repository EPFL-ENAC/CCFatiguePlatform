from sqlalchemy import Column, Integer, String, create_engine, ForeignKey
from sqlalchemy import Float, Date, Enum, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import scoped_session, sessionmaker, relationship

Base = declarative_base()
DBSession = scoped_session(sessionmaker())
engine = None

# This is the ORM mapped class as interface for the DB

## Experiment class ##
class Experiment(Base):
    __tablename__ = "experiment"
    id = Column(Integer, primary_key=True, autoincrement=True)
    Laboratory = Column(String, nullable=False)
    Researcher = Column(String, nullable=False)
    Date = Column(Date)
    Experiment_Type = Column(Enum('FA', 'QS', 'NO_VALUE', name='Experiment_Types'), default='NO_VALUE', nullable=False)
    Fracture = Column(Boolean, nullable=False, default=False)
    Fracture_Mode = Column(Enum('Mode I', 'Mode II', 'Mode III', 'Combined', 'NO_VALUE', name='Fracture_Mode'), default='NO_VALUE')
    Initial_Crack_length = Column(Float, default=0)#, nullable=False) # Modified for the sake of the exercise
    Fatigue_Test_Type = Column(Enum('CA', 'VA', 'BL', 'Combined', 'NO_VALUE', name='Fatigue_Test_Type'), default='NO_VALUE')
    Measuring_Equipment = Column(String)
    Reliability_Level = Column(String)
    Control_mode = Column(Enum('Load Controlled', 'Displacement Controlled', 'NO_VALUE', name='Control_Mode'), default='NO_VALUE', nullable=False)
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

## Test class ##
class Test(Base):
    __tablename__ = "test"
    id = Column(Integer, primary_key=True, autoincrement=True)
    parent_id = Column(Integer, ForeignKey('experiment.id'))
    Specimen_number = Column(Integer, nullable=False)
    Stress_Ratio = Column(Float, nullable=False)
    Maximum_Stress = Column(Float, nullable=False)
    Loading_rate = Column(Float, nullable=False)
    Run_out = Column(Boolean, nullable=False)
    parent = relationship("Experiment", back_populates="children")
    children = relationship("Test_results", back_populates="parent")

## Test results class ##
class Test_results(Base):
    __tablename__ = "test_results"
    id = Column(Integer, primary_key=True, autoincrement=True)
    parent_id = Column(Integer, ForeignKey('test.id'))
    Machine_N_cycles = Column(Integer, nullable=False)
    Machine_Load = Column(Float, nullable=False)
    Machine_Displacement = Column(Float, nullable=False)
    index = Column(Integer)
    Camera_N_cycles = Column(Integer)
    exx = Column(Float)
    eyy = Column(Float)
    exy = Column(Float)
    crack_length = Column(Float)
    Th_time = Column(Integer)
    Th_N_cycles = Column(Integer)
    Th_specimen_max = Column(Integer)
    Th_specimen_mean = Column(Integer)
    Th_chamber = Column(Integer)
    Th_uppergrips = Column(Integer)
    Th_lowergrips = Column(Integer)
    parent = relationship("Test", back_populates="children")

def init(dbname):
    global engine
    print("Connection to the DB :  " + dbname)
    engine = create_engine(dbname, echo=True)
    DBSession.remove()
    DBSession.configure(bind=engine, autoflush=False, expire_on_commit=False)
    Base.metadata.drop_all(engine)
    Base.metadata.create_all(engine)
    print("Tables created in the DB")


def insertExperiment(experiment):
    DBSession.add(experiment)
    DBSession.commit()


def bulkInsertExperiment(experiments):
    DBSession.bulk_save_objects(experiments)
    DBSession.commit()

def insertTest(test):
    DBSession.add(test)
    DBSession.commit()


def bulkInsertTest(tests):
    DBSession.bulk_save_objects(tests)
    DBSession.commit()

def insertTestResult(test_result):
    DBSession.add(test_result)
    DBSession.commit()


def bulkInsertTestResult(test_results):
    DBSession.bulk_save_objects(test_results)
    DBSession.commit()


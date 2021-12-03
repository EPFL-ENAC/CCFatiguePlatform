from sqlalchemy import Column, Integer, ForeignKey
from sqlalchemy import Float
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship


## Test results class ##
from Integration.base import Base


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

def insertTestResult(test_result,DBSession):
    DBSession.add(test_result)
    DBSession.commit()


def bulkInsertTestResult(test_results,DBSession):
    DBSession.bulk_save_objects(test_results)
    DBSession.commit()
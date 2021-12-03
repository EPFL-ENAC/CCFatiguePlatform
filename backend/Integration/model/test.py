from sqlalchemy import Column, Integer, ForeignKey
from sqlalchemy import Float, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship


## Test class ##
from Integration.base import Base


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

def insertTest(test,DBSession):
    DBSession.add(test)
    DBSession.commit()

def bulkInsertTest(tests,DBSession):
    DBSession.bulk_save_objects(tests)
    DBSession.commit()
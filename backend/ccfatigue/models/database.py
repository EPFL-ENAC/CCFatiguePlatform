from sqlalchemy import (
    Boolean,
    Column,
    Enum,
    Float,
    ForeignKey,
    Integer,
    String,
)
from sqlalchemy.orm import relationship
from ccfatigue.services.database import Base


class Experiment(Base):
    __tablename__ = "experiment"
    __table_args__ = {'extend_existing': True}
    id = Column(Integer, primary_key=True)
    ...
    # General
    laboratory = Column(String)
    researcher = Column(String)
    date = Column(String)
    experiment_type = Column(Enum("FA", "QS", "OT", name="experiment_type"))
    fa_experiment_type = Column(String)
    qs_experiment_type = Column(String)
    ot_experiment_type = Column(String)
    ot_add_info = Column(String)
    fracture_mode_fm = Column(Enum("Mode I", "Mode II", "Mode III", "Mixed-Mode", name="fracture_mode_fm"))
    fm_add_info = Column(String)
    control_mode = Column(Enum("Load Controlled", "Displacement Controlled", "Strain controlled", name="control_mode"))
    fatigue_loading_type_flt = Column(Enum("CA", "VA", "BL", "Other", name="fatigue_loading_type_flt"))
    flt_add_info = Column(String)
    measuring_equipment = Column(String)
    loading_rate = Column(Float)

    # Publication
    publication_doi = Column(String)

    # Material Info
    material_tested = Column(String)
    material_type_sample_type = Column(String)
    sample_type_add_info = Column(String)
    material_type_fiber_material = Column(String)
    material_type_fiber_form = Column(String)
    material_type_area_density = Column(String)
    material_type_resin = Column(String)
    material_type_hardener = Column(String)
    material_type_mixing_ratio = Column(String)
    other_polymers_add_info = Column(String)

    curing_time = Column(Float)
    curing_temperature = Column(Float)
    curing_pressure = Column(Float)
    postcuring_time = Column(Float)
    postcuring_temperature = Column(Float)
    postcuring_pressure = Column(Float)
    glue = Column(String)
    glue_curing_time = Column(Float)
    glue_curing_temperature = Column(Float)
    glue_curing_pressure = Column(Float)

    # Laminates
    laminates_and_assemblies_stacking_sequence = Column(String)
    laminates_and_assemblies_fiber_volume_ratio = Column(Float)

    # Fatigue
    fatigue_r_ratio = Column(Float)
    fatigue_frequency = Column(Float)

    tests = relationship("Test", cascade="all, delete", back_populates="experiment")


class Test(Base):
    __tablename__ = "test"
    __table_args__ = {'extend_existing': True}
    id = Column(Integer, primary_key=True)

    experiment_id = Column(Integer, ForeignKey("experiment.id"))
    experiment = relationship("Experiment", back_populates="tests")

    sequential_number = Column(Integer)
    specimen_name = Column(String)
    number_of_cycles = Column(Integer)
    maximum_load = Column(Float)
    run_out = Column(Boolean)

    length = Column(Float)
    width = Column(Float)
    thickness = Column(Float)
    initial_crack_length = Column(Float)
    t = Column(Float)  
    l_prime = Column(Float)  

    temperature = Column(Float)
    humidity = Column(Float)

    subset_size = Column(Float)
    step_size = Column(Float)

    measuring_points = relationship("Test_Measuring_Point", cascade="all, delete", back_populates="test")


class Test_Measuring_Point(Base):
    __tablename__ = "test_measuring_point"
    __table_args__ = {'extend_existing': True}
    id = Column(Integer, primary_key=True)

    test_id = Column(Integer, ForeignKey("test.id"))
    test = relationship("Test", back_populates="measuring_points")

    measuring_point_id = Column(Integer)
    x_coordinate = Column(Float)
    y_coordinate = Column(Float)

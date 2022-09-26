"""
Define the model as it is in the DB
"""

from sqlalchemy import Boolean, Column, Enum, Float, ForeignKey, Integer, String
from sqlalchemy.orm import relationship

from ccfatigue.services.database import Base


class Experiment(Base):
    """
    Defines how experiment data is structured in DB
    """

    __tablename__ = "experiment"
    id = Column(Integer, primary_key=True)

    laboratory = Column(String, default="")
    researcher = Column(String)
    date = Column(String)
    experiment_type = Column(Enum("FA", "QS", "TM", name="experiment_type"))

    fracture = Column(Boolean)
    fracture_mode = Column(
        Enum("Mode I", "Mode II", "Mode III", "Mixed-Mode", "", name="fracture_mode"),
        default="",
    )

    fatigue_test_type = Column(
        Enum("CA", "VA", "BL", "Combined", "", name="fatigue_test_type"),
        default="",
    )
    quasi_static_test_type = Column(
        Enum(
            "Tensile",
            "Compressive",
            "Shear",
            "Bending",
            "Fracture",
            "Unspecified",
            "",
            name="quasi_static_test_type",
        ),
        default="",
    )

    temperature_test_type = Column(
        Enum("DMA", "DSC", "FIRE", "Unspecified", "", name="temperature_test_type"),
        default="",
    )
    measuring_equipment = Column(String)
    reliability_level = Column(Float)

    control_mode = Column(
        Enum("Load Controlled", "Displacement Controlled", "", name="control_mode"),
        default="",
    )

    publication_title = Column(String)
    publication_author = Column(String)
    publication_year = Column(String)
    publication_doi = Column(String)
    publication_images_repository = Column(String)

    material_type_sample_type = Column(String)
    material_type_fiber_material = Column(String)
    material_type_fiber_form = Column(String)
    material_type_area_density = Column(Float)
    material_type_resin = Column(String)
    material_type_hardener = Column(String)
    material_type_mixing_ratio = Column(String)

    laminates_and_assemblies_curing_time = Column(Float)
    laminates_and_assemblies_curing_temperature = Column(Float)
    laminates_and_assemblies_curing_pressure = Column(Float)
    laminates_and_assemblies_fiber_volume_ratio = Column(Float)
    laminates_and_assemblies_stacking_sequence = Column(String)

    measurement_measuring_points = Column(Integer)

    dic_analysis_subset_size = Column(Integer)
    dic_analysis_step_size = Column(Integer)

    tests = relationship("Test", cascade="all, delete")


class Test(Base):
    """
    Defines how test part of experiment is structured in DB
    """

    __tablename__ = "test"
    id = Column(Integer, primary_key=True)

    experiment_id = Column(Integer, ForeignKey("experiment.id"))
    experiment = relationship("Experiment", back_populates="tests")

    specimen_number = Column(Integer)
    specimen_name = Column(String)
    stress_ratio = Column(Float)
    maximum_stress = Column(Float)
    frequency = Column(Float)
    run_out = Column(Boolean)
    displacement_controlled_loading_rate = Column(Float)
    load_controlled_loading_rate = Column(Float)
    length = Column(Float)
    width = Column(Float)
    thickness = Column(Float)
    temperature = Column(Float)
    humidity = Column(Float)
    initial_crack_length = Column(Float)
    measuring_points = relationship("Test_Measuring_Point", cascade="all, delete")


class Test_Measuring_Point(Base):
    """
    Defines how a measuring point is strictured in DB
    """

    __tablename__ = "test_measuring_point"
    id = Column(Integer, primary_key=True)

    test_id = Column(Integer, ForeignKey("test.id"))
    test = relationship("Test", back_populates="measuring_points")

    x_coordinate = Column(Float)
    y_coordinate = Column(Float)

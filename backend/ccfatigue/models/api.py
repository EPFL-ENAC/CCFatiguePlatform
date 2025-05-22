from enum import Enum
from math import isnan
from typing import Optional

from pydantic import BaseModel, validator


def _change_nan_to_none(v: Optional[float]) -> Optional[float]:
    if v is not None and isnan(v):
        return None
    return v


class OrmModel(BaseModel):
    class Config:
        orm_mode = True


class ExperimentModel(OrmModel):
    id: int
    laboratory: Optional[str]
    researcher: str
    date: Optional[str]
    experiment_type: str

    fa_experiment_type: Optional[str]
    qs_experiment_type: Optional[str]
    ot_experiment_type: Optional[str]
    ot_add_info: Optional[str]
    fracture_mode_fm: Optional[str]
    fm_add_info: Optional[str]
    control_mode: Optional[str]
    fatigue_loading_type_flt: Optional[str]
    flt_add_info: Optional[str]
    measuring_equipment: Optional[str]
    loading_rate: Optional[float]

    publication_doi: Optional[str]

    material_tested: Optional[str]
    material_type_sample_type: Optional[str]
    sample_type_add_info: Optional[str]
    material_type_fiber_material: Optional[str]
    material_type_fiber_form: Optional[str]
    material_type_area_density: Optional[str]
    material_type_resin: Optional[str]
    material_type_hardener: Optional[str]
    material_type_mixing_ratio: Optional[str]
    other_polymers_add_info: Optional[str]

    curing_time: Optional[float]
    curing_temperature: Optional[float]
    curing_pressure: Optional[float]
    postcuring_time: Optional[float]
    postcuring_temperature: Optional[float]
    postcuring_pressure: Optional[float]
    glue: Optional[str]
    glue_curing_time: Optional[float]
    glue_curing_temperature: Optional[float]
    glue_curing_pressure: Optional[float]

    laminates_and_assemblies_stacking_sequence: Optional[str]
    laminates_and_assemblies_fiber_volume_ratio: Optional[float]

    fatigue_r_ratio: Optional[float]
    fatigue_frequency: Optional[float]


class TestMeasuringPointModel(OrmModel):
    id: int
    test_id: int
    measuring_point_id: int
    x_coordinate: float
    y_coordinate: float


class TestModel(OrmModel):
    @validator(
        "maximum_load",
        "length",
        "width",
        "thickness",
        "temperature",
        "humidity",
        "initial_crack_length",
        "subset_size",
        "step_size",
    )
    def change_nan_to_none(cls, v) -> Optional[float]:
        return _change_nan_to_none(v)

    id: int
    experiment_id: int
    sequential_number: Optional[int]
    specimen_name: Optional[str]
    number_of_cycles: Optional[int]
    maximum_load: Optional[float]
    run_out: Optional[bool]
    length: Optional[float]
    width: Optional[float]
    thickness: Optional[float]
    initial_crack_length: Optional[float]
    temperature: Optional[float]
    humidity: Optional[float]
    subset_size: Optional[float]
    step_size: Optional[float]


class ExperimentFieldNames(str, Enum):
    experiment_type = "experiment_type"
    fa_experiment_type = "fa_experiment_type"
    qs_experiment_type = "qs_experiment_type"
    ot_experiment_type = "ot_experiment_type"
    fracture_mode_fm = "fracture_mode_fm"
    control_mode = "control_mode"
    fatigue_loading_type_flt = "fatigue_loading_type_flt"

    material_tested = "material_tested"
    material_type_sample_type = "material_type_sample_type"
    sample_type_add_info = "sample_type_add_info"
    material_type_fiber_material = "material_type_fiber_material"
    material_type_fiber_form = "material_type_fiber_form"
    material_type_area_density = "material_type_area_density"
    material_type_resin = "material_type_resin"
    material_type_hardener = "material_type_hardener"
    material_type_mixing_ratio = "material_type_mixing_ratio"
    other_polymers_add_info = "other_polymers_add_info"

    glue = "glue"
    laminates_and_assemblies_stacking_sequence = "laminates_and_assemblies_stacking_sequence"
    laminates_and_assemblies_fiber_volume_ratio = "laminates_and_assemblies_fiber_volume_ratio"

    fatigue_r_ratio = "fatigue_r_ratio"
    fatigue_frequency = "fatigue_frequency"

    curing_time = "curing_time"
    curing_temperature = "curing_temperature"
    curing_pressure = "curing_pressure"


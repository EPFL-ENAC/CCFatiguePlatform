from datetime import date
from enum import Enum
from typing import Any, Dict, List

from pydantic import BaseModel


class Experiment(BaseModel):
    id: str
    laboratory: str
    researcher: str
    type: str
    date: date


class Test(BaseModel):
    number: int
    color: str
    total_dissipated_energy: int
    strain_at_failure: float


class Plot(BaseModel):
    stress_strain: Any
    creep: Any
    hysteresis_area: Any
    stiffness: Any


class Dashboard(BaseModel):
    experiment: Any
    tests: List[Test]
    plot: Plot


class SnCurveMethod(str, Enum):
    LIN_LOG = 'LinLog'
    LOG_LOG = 'LogLog'
    SENDECKYJ = 'Sendeckyj'
    WHITNEY = 'Whitney'


class SnCurveResult(BaseModel):
    outputs: Dict[SnCurveMethod, bytes]
    plot: Any

from datetime import date
from typing import Any, List

from pydantic import BaseModel


class Experience(BaseModel):
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
    experience: Any
    tests: List[Test]
    plot: Plot


class SnCurveResult(BaseModel):
    content: bytes

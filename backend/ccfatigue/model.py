import datetime
from decimal import Decimal
from enum import Enum
from typing import Any, Dict, List

from pydantic import BaseModel


class Experiment(BaseModel):
    id: str
    laboratory: str
    researcher: str
    type: str
    date: datetime.date


class Experiment_Data_Preprocessed(BaseModel):
    output: List[Any]
    success: bool


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
    LIN_LOG = "LinLog"
    LOG_LOG = "LogLog"
    SENDECKYJ = "Sendeckyj"


class EchartLine(BaseModel):
    name: str
    xData: List[Decimal]
    yData: List[Decimal]


class SnCurveResult(BaseModel):
    outputs: Dict[SnCurveMethod, bytes]
    lines: List[EchartLine]


class CycleCountingMethod(str, Enum):
    RANGE_MEAN = "RangeMean"


class CldMethod(str, Enum):
    HARRIS = "Harris"


class FatigueFailureMethod(str, Enum):
    FTPT = "FTPT"


class DamageSummationMethod(str, Enum):
    HARRIS = "Harris"

import io
import json
import math
from pathlib import Path

import pandas as pd
import pytest

from ccfatigue.analysis.damage_summation import ConstantLifeDiagram, execute
from ccfatigue.analyzer import run_damage_summation
from ccfatigue.model import (
    CldMethod,
    CycleCountingMethod,
    DamageSummationInputType,
    SnCurveMethod,
)


DATA_DIR = Path(__file__).parents[2] / "Data" / "samples"


def _simple_cld() -> io.StringIO:
    return io.StringIO(
        "cycles_to_failure,stress_amplitude,stress_mean\n"
        "1000,0,-200\n"
        "1000,100,0\n"
        "1000,0,200\n"
        "1000000,0,-200\n"
        "1000000,10,0\n"
        "1000000,0,200\n"
    )


def _run(cycle_count: float, factor: float = 1.0):
    cyc = io.StringIO(
        "stress_range,stress_mean,n_cycles\n" f"20,0,{cycle_count}\n"
    )
    csv_output = io.StringIO()
    json_output = io.BytesIO()
    execute(
        cyc,
        _simple_cld(),
        csv_output,
        json_output,
        factor,
        factor,
        1,
        "Rainflow",
        "LogLog",
        "PiecewiseLinear",
    )
    csv_output.seek(0)
    json_output.seek(0)
    return pd.read_csv(csv_output).iloc[0], json.load(json_output)


def test_damage_is_miner_sum_and_output_is_spectrum_life():
    row, metadata = _run(cycle_count=2)

    assert row.damage_per_pass == pytest.approx(2e-6)
    assert row.spectrum_passes_to_failure == pytest.approx(500_000)
    assert metadata["summary"]["damage_per_pass"] == pytest.approx(2e-6)
    assert metadata["summary"]["spectrum_passes_to_failure"] == pytest.approx(
        500_000
    )


def test_repeating_more_cycles_in_one_pass_increases_damage_linearly():
    first, _ = _run(cycle_count=2)
    doubled, _ = _run(cycle_count=4)

    assert doubled.damage_per_pass == pytest.approx(2 * first.damage_per_pass)
    assert doubled.spectrum_passes_to_failure == pytest.approx(
        first.spectrum_passes_to_failure / 2
    )


def test_cld_inversion_interpolates_in_log_log_space():
    diagram = ConstantLifeDiagram.from_csv(_simple_cld())

    life, static_overload = diagram.cycles_to_failure(0, 10)

    assert not static_overload
    assert life == pytest.approx(1_000_000)


def test_cld_inversion_follows_constant_stress_ratio_ray():
    diagram = ConstantLifeDiagram.from_csv(_simple_cld())

    life, static_overload = diagram.cycles_to_failure(20, 20)

    short_life_amplitude = 100 / 1.5
    long_life_amplitude = 10 / 1.05
    interpolation_fraction = (
        (math.log10(20) - math.log10(short_life_amplitude))
        / (
            math.log10(long_life_amplitude)
            - math.log10(short_life_amplitude)
        )
    )
    expected_life = 10 ** (3 + interpolation_fraction * 3)

    assert not static_overload
    assert life == pytest.approx(expected_life)


def test_static_strength_boundary_is_immediate_failure():
    diagram = ConstantLifeDiagram.from_csv(_simple_cld())

    life, static_overload = diagram.cycles_to_failure(150, 50)

    assert static_overload
    assert life == 1


@pytest.mark.parametrize(
    ("data_type", "filename", "sn_method", "ucs", "uts"),
    [
        (
            DamageSummationInputType.SNC,
            "SNC_sample_2022-10.csv",
            None,
            367.2,
            416.5,
        ),
        (
            DamageSummationInputType.AGG,
            "AGG_sample_2022-09.csv",
            SnCurveMethod.LOG_LOG,
            120.0,
            120.0,
        ),
    ],
)
def test_complete_pipeline_accepts_snc_and_agg_inputs(
    data_type, filename, sn_method, ucs, uts
):
    with (
        (DATA_DIR / "LDS_sample_2022-11.csv").open("rb") as load_history,
        (DATA_DIR / filename).open("rb") as fatigue_data,
    ):
        result = run_damage_summation(
            load_history,
            fatigue_data,
            data_type,
            CycleCountingMethod.RAINFLOW,
            CldMethod.PIECEWISELINEAR,
            ucs,
            uts,
            sn_method,
            factor_min=1,
            factor_max=1,
            factor_count=1,
        )

    output = pd.read_csv(io.BytesIO(result.csv_data))
    assert list(output["scale_factor"]) == [1]
    assert output.loc[0, "damage_per_pass"] > 0
    assert output.loc[0, "spectrum_passes_to_failure"] == pytest.approx(
        1 / output.loc[0, "damage_per_pass"]
    )

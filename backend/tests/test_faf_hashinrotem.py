"""
Focused tests for the Hashin-Rotem book-alignment work (faf_hashinrotem.py):
fiber-failure-mode addition (Eqs. 6.1/6.2a), the Eq. 6.3 matrix-mode path
staying bit-identical, degrees-only output, the removed silent fallbacks,
the stress-ratio/fatigue_model validation, and the invalid-point guards.

These are independent of the smoke tests in test_analysis.py (which just
exercise execute() against the repo's sample data files) - here every
scenario is built from small, hand-picked synthetic S-N curves so the
expected behaviour can be predicted and asserted exactly.
"""

import io
import json
import warnings
from itertools import chain

import numpy as np
import pandas as pd
import pytest

import ccfatigue.analysis.faf_hashinrotem as fh
import ccfatigue.analysis.utils.faf as faf
from ccfatigue.model import HashinRotemPanelType as PanelType

# Common static strengths reused across scenarios unless a test needs to
# vary one specifically.
TT = 84.94  # tensile_transverse_strength
SS = 61.38  # shear_strength
TENSILE_STRENGTH_AT_DESIRABLE_ANGLE = 89.47


def _snc_buffer(
    stress_ratio: float, a: float, b: float, confidence_interval: float = 50.0
):
    """In-memory SNC-format json input, as expected by execute()'s file args."""
    return io.StringIO(
        json.dumps(
            [
                {
                    "stress_ratio": stress_ratio,
                    "confidence_interval": confidence_interval,
                    "a": a,
                    "b": b,
                    "fp": 0,
                }
            ]
        )
    )


def _run(
    *,
    a_x,
    b_x,
    a_2,
    b_2,
    a_3,
    b_3,
    fatigue_model=faf.FatigueModel.LOG_LOG,
    desirable_angle=30.0,
    off_axis_angle1=0.0,
    off_axis_angle2=0.0,
    tensile_transverse_strength=TT,
    shear_strength=SS,
    tensile_strength1=100.0,
    tensile_strength2=100.0,
    tensile_strength_at_desirable_angle=TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
    tensile_axial_strength=200.0,
    panel2_type=PanelType.TRANSVERSE,
    panel3_type=PanelType.SHEAR,
    stress_ratio=0.1,
    on_invalid_point="nan",
) -> pd.DataFrame:
    """Run execute() against synthetic in-memory curves, return the parsed CSV."""
    csv_out = io.BytesIO()
    json_out = io.BytesIO()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        fh.execute(
            _snc_buffer(stress_ratio, a_x, b_x),
            _snc_buffer(stress_ratio, a_2, b_2),
            _snc_buffer(stress_ratio, a_3, b_3),
            csv_out,
            json_out,
            fatigue_model,
            desirable_angle,
            off_axis_angle1,
            off_axis_angle2,
            tensile_transverse_strength,
            shear_strength,
            tensile_strength1,
            tensile_strength2,
            tensile_strength_at_desirable_angle,
            tensile_axial_strength,
            panel2_type,
            panel3_type,
            on_invalid_point=on_invalid_point,
        )
    csv_out.seek(0)
    return pd.read_csv(csv_out)


def _reference_matrix_stress_max(
    panel2_type,
    panel2_curve,
    panel3_type,
    panel3_curve,
    off_axis_angle1,
    off_axis_angle2,
    tensile_strength1,
    tensile_strength2,
    desirable_angle,
    tensile_strength_at_desirable_angle,
):
    """
    The pre-item-1 behaviour: Eq. 6.3 applied unconditionally (no fiber/
    matrix mode selection). Used as the "current code" reference for the
    matrix-mode regression test - built from the same reconstruct_s2f_s12f
    and hashin_rotem_off_axis_fatigue_function this module already ships
    (both explicitly left untouched by the fiber-mode work).
    """
    cycles = pd.Series(fh.LIST_CYCLES_TO_FAILURE)
    s2f, s12f = fh.reconstruct_s2f_s12f(
        panel2_type,
        panel2_curve,
        panel3_type,
        panel3_curve,
        off_axis_angle1,
        off_axis_angle2,
        tensile_strength1,
        tensile_strength2,
        SS,
        TT,
        cycles,
    )
    fT = s2f / TT
    fs = s12f / SS
    f_prime = fh.hashin_rotem_off_axis_fatigue_function(
        f_tau=fs,
        tau_s=SS,
        theta=np.radians(desirable_angle),
        sigma_s_t=TT,
        f_t=fT,
    )
    return tensile_strength_at_desirable_angle * f_prime


# ---------------------------------------------------------------------------
# 1. Matrix-mode regression: fiber-mode addition must not disturb the
#    already-verified Eq. 6.3 path when matrix mode governs everywhere.
# ---------------------------------------------------------------------------

# (a_x, b_x) is deliberately huge/slow-decaying relative to the transverse/
# shear/off-axis curves below - since theta_c = arctan(s12f/s1f) (the
# tensile_axial_strength cancels out of Eq. 6.1), an oversized s1f forces
# theta_c far below 10 deg regardless of the other curves' details.
_MATRIX_REGRESSION_CASES = {
    "Log-Log": dict(
        fatigue_model=faf.FatigueModel.LOG_LOG,
        a_x=100000.0,
        b_x=0.05,
        b_transverse=0.12,
        b_shear=0.10,
        b_offaxis1=0.10,
        b_offaxis2=0.13,
    ),
    "Lin-Log": dict(
        fatigue_model=faf.FatigueModel.LIN_LOG,
        a_x=100000.0,
        b_x=-1000.0,
        b_transverse=-3.0,
        b_shear=-3.0,
        b_offaxis1=-3.0,
        b_offaxis2=-3.0,
    ),
}


@pytest.mark.parametrize("model_name", ["Log-Log", "Lin-Log"])
@pytest.mark.parametrize("config", ["direct", "transverse_offaxis", "offaxis_offaxis"])
def test_matrix_mode_regression(config, model_name):
    p = _MATRIX_REGRESSION_CASES[model_name]
    desirable_angle = 30.0

    if config == "direct":
        panel2_type, panel3_type = PanelType.TRANSVERSE, PanelType.SHEAR
        a_2, b_2 = 80.0, p["b_transverse"]
        a_3, b_3 = 90.0, p["b_shear"]
        oa1 = oa2 = 0.0
        ts1 = ts2 = 100.0
    elif config == "transverse_offaxis":
        panel2_type, panel3_type = PanelType.TRANSVERSE, PanelType.OFF_AXIS
        a_2, b_2 = 80.0, p["b_transverse"]
        a_3, b_3 = 70.0, p["b_offaxis1"]
        oa1, oa2 = 0.0, 30.0
        ts1, ts2 = 100.0, 139.12
    else:  # offaxis_offaxis
        panel2_type, panel3_type = PanelType.OFF_AXIS, PanelType.OFF_AXIS
        a_2, b_2 = 70.0, p["b_offaxis1"]
        a_3, b_3 = 50.0, p["b_offaxis2"]
        oa1, oa2 = 10.0, 50.0
        ts1, ts2 = 84.94, 139.12

    df = _run(
        a_x=p["a_x"],
        b_x=p["b_x"],
        a_2=a_2,
        b_2=b_2,
        a_3=a_3,
        b_3=b_3,
        fatigue_model=p["fatigue_model"],
        desirable_angle=desirable_angle,
        off_axis_angle1=oa1,
        off_axis_angle2=oa2,
        tensile_strength1=ts1,
        tensile_strength2=ts2,
        panel2_type=panel2_type,
        panel3_type=panel3_type,
    )

    # theta_c should be comfortably below 10 deg everywhere, matrix mode
    # should govern at every N.
    assert (df.critical_angle < 10.0).all()
    assert (df.failure_mode == "matrix").all()

    get_stress = (
        fh.get_loglog_stress if model_name == "Log-Log" else fh.get_linlog_stress
    )
    cycles = pd.Series(fh.LIST_CYCLES_TO_FAILURE)
    panel2_curve = get_stress(a_2, b_2, cycles)
    panel3_curve = get_stress(a_3, b_3, cycles)
    reference = _reference_matrix_stress_max(
        panel2_type,
        panel2_curve,
        panel3_type,
        panel3_curve,
        oa1,
        oa2,
        ts1,
        ts2,
        desirable_angle,
        TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
    )
    rel_diff = np.abs(df.stress_max.to_numpy() - reference.to_numpy()) / np.abs(
        reference.to_numpy()
    )
    assert rel_diff.max() < 1e-12


# ---------------------------------------------------------------------------
# 2. Fiber mode activates for a small desirable_angle.
# ---------------------------------------------------------------------------


def test_fiber_mode_activates():
    tensile_axial_strength = 200.0
    df = _run(
        a_x=50.0,
        b_x=0.10,
        a_2=80.0,
        b_2=0.10,  # transverse - irrelevant to mode selection
        a_3=90.0,
        b_3=0.10,  # shear - same exponent as X curve, so theta_c is
        # constant (~61 deg) across all of N: arctan(s12f/s1f) = arctan(90/50)
        desirable_angle=3.0,
        tensile_axial_strength=tensile_axial_strength,
    )

    assert (df.failure_mode == "fiber").any()
    assert (df.failure_mode == "fiber").all()  # theta_c ~61deg >> 3deg everywhere

    expected = TENSILE_STRENGTH_AT_DESIRABLE_ANGLE * df.s1f / tensile_axial_strength
    assert np.allclose(df.stress_max, expected, rtol=1e-10)


def test_json_outputs_angles_in_degrees():
    csv_out = io.BytesIO()
    json_out = io.BytesIO()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        fh.execute(
            _snc_buffer(0.1, 50.0, 0.10),
            _snc_buffer(0.1, 80.0, 0.10),
            _snc_buffer(0.1, 90.0, 0.10),
            csv_out,
            json_out,
            faf.FatigueModel.LOG_LOG,
            30.0,
            12.5,
            47.5,
            TT,
            SS,
            100.0,
            100.0,
            TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
            200.0,
            PanelType.TRANSVERSE,
            PanelType.SHEAR,
        )

    payload = json.loads(json_out.getvalue().decode("utf-8"))
    assert payload[0]["off_axis_angle1"] == 12.5
    assert payload[0]["off_axis_angle2"] == 47.5


# ---------------------------------------------------------------------------
# 3. Mode switches exactly once along the N grid.
# ---------------------------------------------------------------------------


def test_mode_switches_along_n():
    # b_x < b_3: the shear curve decays faster than the axial curve, so
    # theta_c(N) = arctan(s12f(N)/s1f(N)) decreases monotonically from
    # ~61deg (N=1) to ~8.2deg (N=2e7), crossing desirable_angle=20deg once.
    df = _run(
        a_x=50.0,
        b_x=0.05,
        a_2=80.0,
        b_2=0.10,
        a_3=90.0,
        b_3=0.20,
        desirable_angle=20.0,
    )

    modes = df.failure_mode.to_numpy()
    assert set(modes) == {"fiber", "matrix"}
    n_switches = int(np.sum(modes[1:] != modes[:-1]))
    assert n_switches == 1
    # theta_c decreases with N, so fiber (large theta_c) comes first.
    assert modes[0] == "fiber"
    assert modes[-1] == "matrix"


# ---------------------------------------------------------------------------
# 4. Eq. 6.3 round-trip: transverse+off-axis -> shear+off-axis recovers the
#    original transverse curve.
# ---------------------------------------------------------------------------


def test_eq63_round_trip():
    cycles = pd.Series(fh.LIST_CYCLES_TO_FAILURE)
    transverse_curve = fh.get_loglog_stress(100.0, 0.10, cycles)
    off_axis_curve = fh.get_loglog_stress(70.0, 0.08, cycles)
    off_axis_strength = 139.12
    angle = 25.0

    s2f_1, s12f_1 = fh.reconstruct_s2f_s12f(
        PanelType.TRANSVERSE,
        transverse_curve,
        PanelType.OFF_AXIS,
        off_axis_curve,
        0.0,
        angle,
        100.0,
        off_axis_strength,
        SS,
        TT,
        cycles,
    )
    assert np.allclose(s2f_1, transverse_curve)

    s2f_2, _s12f_2 = fh.reconstruct_s2f_s12f(
        PanelType.SHEAR,
        s12f_1,
        PanelType.OFF_AXIS,
        off_axis_curve,
        0.0,
        angle,
        100.0,
        off_axis_strength,
        SS,
        TT,
        cycles,
    )
    rel_diff = np.abs(s2f_2.to_numpy() - transverse_curve.to_numpy()) / np.abs(
        transverse_curve.to_numpy()
    )
    assert rel_diff.max() < 1e-12


# ---------------------------------------------------------------------------
# 5. Book worked example (Sec. 6.3.1.1, pp. 167-169, Table 6.1) - SKIPPED.
# ---------------------------------------------------------------------------
#
# Table 6.1's digitised glass/epoxy UD data (90deg/10deg curves at R=0.1,
# plus off-axis data at 5/10/15/20/30/60deg) is not available in this repo
# or environment (no copy of Vassilopoulos & Keller, "Fatigue of
# Fiber-reinforced Composites" is present - see `find ... -iname "*.pdf"`
# turning up only unrelated CCFatigue platform docs). Per the task's own
# instruction, no numbers have been invented to fake this test - it is
# skipped and flagged for human follow-up instead.
@pytest.mark.skip(
    reason=(
        "Book Table 6.1 (pp. 167-169) data is not available in this "
        "environment to transcribe - needs a human with the book to "
        "supply real numbers instead of invented ones."
    )
)
def test_book_worked_example_6_3_1_1():
    raise NotImplementedError


# ---------------------------------------------------------------------------
# 6. Error paths.
# ---------------------------------------------------------------------------


def test_stress_ratio_above_one_raises():
    with pytest.raises(ValueError, match="tension-tension"):
        _run(
            a_x=50.0, b_x=0.10, a_2=80.0, b_2=0.10, a_3=90.0, b_3=0.10, stress_ratio=1.5
        )


def test_stress_ratio_below_zero_raises():
    with pytest.raises(ValueError, match="tension-tension"):
        _run(
            a_x=50.0,
            b_x=0.10,
            a_2=80.0,
            b_2=0.10,
            a_3=90.0,
            b_3=0.10,
            stress_ratio=-0.5,
        )


def test_disagreeing_stress_ratio_raises():
    csv_out = io.BytesIO()
    json_out = io.BytesIO()
    with pytest.raises(ValueError, match="disagree on stress_ratio"):
        fh.execute(
            _snc_buffer(0.1, 50.0, 0.10),
            _snc_buffer(0.15, 80.0, 0.10),
            _snc_buffer(0.1, 90.0, 0.10),
            csv_out,
            json_out,
            faf.FatigueModel.LOG_LOG,
            30.0,
            0.0,
            0.0,
            TT,
            SS,
            100.0,
            100.0,
            TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
            200.0,
            PanelType.TRANSVERSE,
            PanelType.SHEAR,
        )


def test_disagreeing_confidence_interval_raises():
    csv_out = io.BytesIO()
    json_out = io.BytesIO()
    with pytest.raises(ValueError, match="disagree on confidence_interval"):
        fh.execute(
            _snc_buffer(0.1, 50.0, 0.10, confidence_interval=50.0),
            _snc_buffer(0.1, 80.0, 0.10, confidence_interval=60.0),
            _snc_buffer(0.1, 90.0, 0.10, confidence_interval=50.0),
            csv_out,
            json_out,
            faf.FatigueModel.LOG_LOG,
            30.0,
            0.0,
            0.0,
            TT,
            SS,
            100.0,
            100.0,
            TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
            200.0,
            PanelType.TRANSVERSE,
            PanelType.SHEAR,
        )


def test_unknown_fatigue_model_raises():
    with pytest.raises(ValueError, match="unknown fatigue_model"):
        _run(
            a_x=50.0,
            b_x=0.10,
            a_2=80.0,
            b_2=0.10,
            a_3=90.0,
            b_3=0.10,
            fatigue_model="Bogus",
        )


def test_identical_off_axis_angles_raises():
    with pytest.raises(ValueError, match="two distinct"):
        _run(
            a_x=50.0,
            b_x=0.10,
            a_2=70.0,
            b_2=0.10,
            a_3=50.0,
            b_3=0.10,
            panel2_type=PanelType.OFF_AXIS,
            panel3_type=PanelType.OFF_AXIS,
            off_axis_angle1=20.0,
            off_axis_angle2=20.0,
            tensile_strength1=100.0,
            tensile_strength2=100.0,
        )


def test_both_panels_same_non_offaxis_type_raises():
    with pytest.raises(ValueError, match="cannot both be"):
        _run(
            a_x=50.0,
            b_x=0.10,
            a_2=80.0,
            b_2=0.10,
            a_3=90.0,
            b_3=0.10,
            panel2_type=PanelType.TRANSVERSE,
            panel3_type=PanelType.TRANSVERSE,
        )


# ---------------------------------------------------------------------------
# Item 6: invalid-point guards (radicand/denominator/positivity) - both
# policies.
# ---------------------------------------------------------------------------


def test_invalid_point_policy_nan_drops_only_bad_rows():
    # Transverse curve 35 - 5*log10(N) crosses zero around N ~ 1.3e7 -
    # exactly the "Lin-Log fit crosses zero" scenario called out in item 6.
    df = _run(
        a_x=200.0,
        b_x=-5.0,
        a_2=35.0,
        b_2=-5.0,
        a_3=60.0,
        b_3=-3.0,
        fatigue_model=faf.FatigueModel.LIN_LOG,
        on_invalid_point="nan",
    )
    assert df.stress_max.isna().any()
    assert not df.stress_max.isna().all()
    # Rows before the zero-crossing are still valid and computed.
    assert df.stress_max.notna().sum() > 0


def test_invalid_point_policy_raise_aborts():
    with pytest.raises(ValueError, match="not strictly positive"):
        _run(
            a_x=200.0,
            b_x=-5.0,
            a_2=35.0,
            b_2=-5.0,
            a_3=60.0,
            b_3=-3.0,
            fatigue_model=faf.FatigueModel.LIN_LOG,
            on_invalid_point="raise",
        )


# ---------------------------------------------------------------------------
# 7. N grid unchanged by this work.
# ---------------------------------------------------------------------------


def test_n_grid_unchanged():
    expected = list(
        chain(
            range(1, 1000, 50),
            range(1000, 1001),
            range(10000, 2000000, 10000),
            range(2000000, 20000001, 1000000),
        )
    )
    assert len(expected) == 239
    assert fh.LIST_CYCLES_TO_FAILURE == expected

#!/usr/bin/env python
"""
This code takes in SNC and outputs FAF data.

Hashin-Rotem (HR) fatigue failure criterion for unidirectional (UD)
laminates under tension-tension loading only. Multidirectional laminates
(book Eq. 6.4, interlaminar mode) and compression loading are out of scope.

H-R is described in Fatigue of Fiber-reinforced Composites [1], Sec. 6.2.1,
pp. 159-161 (Section 6.2.1 "Hashin-Rotem" begins on p. 159; Eqs. 6.1-6.3,
which this module implements, are on p. 160).
The original paper is
[1] https://link.springer.com/book/10.1007/978-1-84996-181-3
    DOI 10.1007/978-1-84996-181-3
[2] https://journals.sagepub.com/doi/10.1177/002199837300700404
    https://drive.google.com/drive/u/1/folders/1SVank5TEcCxHG5vIgcmxaR9zAov8qj2l?ths=true

Failure mode selection (book Eqs. 6.1, 6.2a, 6.2b):
    Below the critical fiber angle theta_c(N), the fiber mode governs and
    the off-axis strength is set by the (constant) axial fatigue function
    alone (Eq. 6.2a). At or above theta_c(N), the matrix mode governs and
    the off-axis strength follows the quadratic interaction of transverse
    and shear stresses (Eq. 6.2b), whose fatigue form is Eq. 6.3
    (hashin_rotem_off_axis_fatigue_function below). Because the fatigue
    functions f_A/f_tau/f_T all vary with N, theta_c is a function of N too
    - the governing mode can switch partway along a single S-N curve.
"""

import warnings
from itertools import chain
from typing import Literal, Optional

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.faf as faf
from ccfatigue.model import HashinRotemPanelType

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(2000000, 20000001, 1000000),
    )
)

# Absolute tolerance used to guard denominators/radicands that would
# otherwise blow up (division by ~0) or go slightly negative under a square
# root due to floating-point noise right at a curve's zero-crossing. The
# quantities guarded by it are dimensionless ratios of order 1, so an
# absolute tolerance is appropriate.
_EPS = 1e-9

InvalidPointPolicy = Literal["raise", "nan"]


def hashin_rotem_off_axis_fatigue_function(
    f_tau: float,
    tau_s: float,
    theta: float,
    sigma_s_t: float,
    f_t: float,
):
    """Off-axis fatigue function, matrix-failure-mode branch.

    Book Eq. 6.3 (Sec. 6.2.1, p. 160). Equivalent to Eq. 23 of the original
    paper (Z. Hashin, A. Rotem, J. Compos. Mater. 7, 448-464 (1973), p. 455)
    - same equation, both references describe it.

    Parameters
    ----------
        f_tau = material fatigue function
        tau_s = shear loading alone
        theta = reinforcement angle
        sigma_s_t = failure stresses in transverse loading alone
        f_t = material fatigue function
    Returns
    -------
        f = material fatigue function
    """
    f = f_tau * np.sqrt(
        (1 + ((tau_s / sigma_s_t) * np.tan(theta)) ** 2)
        / (1 + ((tau_s / sigma_s_t) * (f_tau / f_t) * np.tan(theta)) ** 2)
    )
    return f


def get_loglog_stress(a: float, b: float, cycles_to_failure) -> float:
    """
    Get stress according to a log-log slope
    Parameters
    ----------
        a: float
        b: float
        cycles_to_failure: float
            Number of cycles to failure (N) [-]
    Returns
    -------
        stress: float
    """
    stress = a * cycles_to_failure**-b
    return stress


def get_linlog_stress(a: float, b: float, cycles_to_failure) -> float:
    """
    Get stress according to a lin-log slope
    Parameters
    ----------
        a: float
        b: float
        cycles_to_failure: float
            Number of cycles to failure (N) [-]
    Returns
    -------
        stress: float
    """
    stress = a + b * np.log10(cycles_to_failure)
    return stress


def _format_n_range(cycles_to_failure: pd.Series, mask: pd.Series) -> str:
    ns = cycles_to_failure[mask]
    return (
        f"N in [{ns.min():.0f}, {ns.max():.0f}] "
        f"({int(mask.sum())} of {len(mask)} points)"
    )


def _guard(
    ok: pd.Series,
    message: str,
    cycles_to_failure: pd.Series,
    on_invalid_point: InvalidPointPolicy,
) -> pd.Series:
    """
    Validate a per-N boolean condition. `ok` must be True wherever the
    guarded quantity (a radicand, a denominator, a reconstructed strength...)
    is valid at that N.

    - on_invalid_point == "raise": raise ValueError immediately, naming the
      offending N range, aborting the whole curve.
    - on_invalid_point == "nan": log a warning naming the offending N range
      and return `ok` unchanged so the caller can NaN-out just those rows
      and keep computing the rest of the curve. This matches the legacy
      Fortran's observable behaviour (it silently drops offending points
      from the written curve, via `IF (ft2.LE.0) GO TO <end of loop>`) more
      closely than aborting the whole run outright.

    This is a policy decision with no single obviously-right default - see
    item 7 of the Hashin-Rotem book-alignment task. "nan" is used as the
    default here (most useful, matches legacy behaviour) but this choice
    needs human review.

    Returns
    -------
        pd.Series[bool] - identical to `ok` (NaNs treated as False);
        returned so call sites can accumulate `keep &= _guard(...)`.
    """
    ok = ok.fillna(False)
    bad = ~ok
    if not bad.any():
        return ok

    full_message = (
        f"HashinRotem: {message} for {_format_n_range(cycles_to_failure, bad)}."
    )

    if on_invalid_point == "raise":
        raise ValueError(full_message)
    if on_invalid_point != "nan":
        raise ValueError(
            f"HashinRotem: unknown on_invalid_point={on_invalid_point!r}; "
            "expected 'raise' or 'nan'."
        )
    warnings.warn(full_message + " Emitting NaN for these rows.")
    return ok


def reconstruct_s2f_s12f(
    panel2_type: HashinRotemPanelType,
    panel2_curve: pd.Series,
    panel3_type: HashinRotemPanelType,
    panel3_curve: pd.Series,
    off_axis_angle1: float,
    off_axis_angle2: float,
    tensile_strength1: float,
    tensile_strength2: float,
    shear_strength: float,
    tensile_transverse_strength: float,
    cycles_to_failure: pd.Series,
    on_invalid_point: InvalidPointPolicy = "nan",
):
    """
    Reconstruct S2f(N) and S12f(N) from whichever two of {Transverse, Shear,
    Off-axis} panel2/panel3 actually are (book Sec. 6.2.1, Eq. 6.3).
    Resolved point-by-point across the N grid - panel2_curve/panel3_curve
    are already per-N Series, never collapsed to a single global value.
    Parameters
    ----------
        panel2_type: HashinRotemPanelType
        panel2_curve: pd.Series
            Panel 2's own fitted S-N curve, evaluated over the N grid.
        panel3_type: HashinRotemPanelType
        panel3_curve: pd.Series
        off_axis_angle1: float
            theta1 [degrees] - only meaningful if panel2_type is OFF_AXIS.
        off_axis_angle2: float
            theta2 [degrees] - only meaningful if panel3_type is OFF_AXIS.
        tensile_strength1: float
            Static strength at theta1 - only meaningful if panel2_type is
            OFF_AXIS.
        tensile_strength2: float
            Static strength at theta2 - only meaningful if panel3_type is
            OFF_AXIS.
        shear_strength: float
            ss, static shear strength.
        tensile_transverse_strength: float
            sigma_sT, static transverse strength.
        cycles_to_failure: pd.Series
            N grid, same index as panel2_curve/panel3_curve - used only to
            name the offending N range in guard messages, never in the
            reconstruction algebra itself.
        on_invalid_point: InvalidPointPolicy
            See `_guard`.
    Returns
    -------
        (s2f: pd.Series, s12f: pd.Series)
    """
    transverse = HashinRotemPanelType.TRANSVERSE
    shear = HashinRotemPanelType.SHEAR
    off_axis = HashinRotemPanelType.OFF_AXIS

    if panel2_type == panel3_type and panel2_type != off_axis:
        raise ValueError(
            f"HashinRotem: panels cannot both be '{panel2_type.value}' - "
            "pick distinct types (Transverse / Shear / Off-axis)."
        )

    def curve_for(target_type):
        return panel2_curve if panel2_type == target_type else panel3_curve

    r = shear_strength / tensile_transverse_strength
    types = {panel2_type, panel3_type}

    if types == {transverse, shear}:
        # Direct mode - the two curves already are S2f/S12f themselves.
        transverse_curve = curve_for(transverse)
        shear_curve = curve_for(shear)
        keep = _guard(
            transverse_curve > 0,
            "S2f (direct transverse curve) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        keep &= _guard(
            shear_curve > 0,
            "S12f (direct shear curve) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        return transverse_curve.where(keep), shear_curve.where(keep)

    if types == {transverse, off_axis}:
        transverse_curve = curve_for(transverse)
        off_axis_curve = curve_for(off_axis)
        off_axis_angle = off_axis_angle1 if panel2_type == off_axis else off_axis_angle2
        off_axis_strength = (
            tensile_strength1 if panel2_type == off_axis else tensile_strength2
        )

        keep = _guard(
            transverse_curve > 0,
            "S_T (transverse curve, case 2) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        keep &= _guard(
            off_axis_curve > 0,
            "off-axis curve (case 2) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )

        t2 = np.tan(np.radians(off_axis_angle)) ** 2
        fT = transverse_curve / tensile_transverse_strength
        F2 = off_axis_curve / off_axis_strength

        keep &= _guard(
            np.abs(fT) > _EPS,
            "fT denominator (case 2) is too close to zero",
            cycles_to_failure,
            on_invalid_point,
        )
        fT_safe = fT.where(keep, 1.0)

        radicand = (1 + r**2 * t2) - (F2**2 * r**2 * t2) / fT_safe**2
        # radicand is itself used under a sqrt() that is then divided into
        # (fs = F2 / sqrt(radicand)), so it must be strictly positive, not
        # merely non-negative - a zero radicand would still blow fs up to
        # infinity.
        keep &= _guard(
            radicand > _EPS,
            "case 2 reconstruction radicand under a square root is "
            "non-positive - check the off-axis/transverse strength inputs",
            cycles_to_failure,
            on_invalid_point,
        )
        radicand_safe = radicand.where(keep, 1.0).clip(lower=_EPS)

        fs = F2 / np.sqrt(radicand_safe)
        keep &= _guard(
            fs > 0,
            "reconstructed S12f (case 2) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        return transverse_curve.where(keep), (fs * shear_strength).where(keep)

    if types == {shear, off_axis}:
        shear_curve = curve_for(shear)
        off_axis_curve = curve_for(off_axis)
        off_axis_angle = off_axis_angle1 if panel2_type == off_axis else off_axis_angle2
        off_axis_strength = (
            tensile_strength1 if panel2_type == off_axis else tensile_strength2
        )

        keep = _guard(
            shear_curve > 0,
            "S12f (shear curve, case 3) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        keep &= _guard(
            off_axis_curve > 0,
            "off-axis curve (case 3) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )

        t1 = np.tan(np.radians(off_axis_angle)) ** 2
        fs = shear_curve / shear_strength
        F1 = off_axis_curve / off_axis_strength

        radicand = fs**2 * (1 + r**2 * t1) - F1**2
        # Same reasoning as case 2 - sqrt(radicand) is a denominator of fT
        # below, so it must be strictly positive.
        keep &= _guard(
            radicand > _EPS,
            "case 3 reconstruction radicand under a square root is "
            "non-positive - check the off-axis/shear strength inputs",
            cycles_to_failure,
            on_invalid_point,
        )
        radicand_safe = radicand.where(keep, 1.0).clip(lower=_EPS)

        fT = (F1 * r * np.sqrt(t1) * fs) / np.sqrt(radicand_safe)
        keep &= _guard(
            fT > 0,
            "reconstructed S2f (case 3) is not strictly positive",
            cycles_to_failure,
            on_invalid_point,
        )
        return (fT * tensile_transverse_strength).where(keep), shear_curve.where(keep)

    # types == {off_axis} - both panels are off-axis (panel2 <-> theta1,
    # panel3 <-> theta2, by construction of how off_axis_angle1/2 and
    # tensile_strength1/2 are wired to panel2/panel3 respectively).
    if np.isclose(off_axis_angle1, off_axis_angle2):
        raise ValueError(
            "HashinRotem: case 1 reconstruction needs two distinct "
            "off-axis angles (theta1 != theta2)."
        )

    keep = _guard(
        panel2_curve > 0,
        "off-axis curve at theta1 (case 1) is not strictly positive",
        cycles_to_failure,
        on_invalid_point,
    )
    keep &= _guard(
        panel3_curve > 0,
        "off-axis curve at theta2 (case 1) is not strictly positive",
        cycles_to_failure,
        on_invalid_point,
    )

    t1 = np.tan(np.radians(off_axis_angle1)) ** 2
    t2 = np.tan(np.radians(off_axis_angle2)) ** 2
    F1 = panel2_curve / tensile_strength1
    F2 = panel3_curve / tensile_strength2

    det = (1 + r**2 * t1) * (-(F2**2) * r**2 * t2) - (
        -(F1**2) * r**2 * t1
    ) * (1 + r**2 * t2)
    keep &= _guard(
        np.abs(det) > 1e-12,
        "case 1 reconstruction is degenerate (determinant near zero) - "
        "check the off-axis angles and strengths",
        cycles_to_failure,
        on_invalid_point,
    )
    det_safe = det.where(keep, 1.0)

    u = (
        F1**2 * (-(F2**2) * r**2 * t2) - (-(F1**2) * r**2 * t1) * F2**2
    ) / det_safe
    w = ((1 + r**2 * t1) * F2**2 - F1**2 * (1 + r**2 * t2)) / det_safe

    keep &= _guard(
        u >= 0,
        "case 1 reconstruction hit a negative value under a square root "
        "(fs) - check the off-axis strength inputs",
        cycles_to_failure,
        on_invalid_point,
    )
    # w is a denominator (fT = fs / sqrt(w)), so it must be strictly
    # positive and bounded away from zero, not merely non-negative.
    keep &= _guard(
        w > _EPS,
        "case 1 reconstruction hit a non-positive/near-zero value under a "
        "square root (fT) - check the off-axis strength inputs",
        cycles_to_failure,
        on_invalid_point,
    )

    u_safe = u.where(keep, 1.0).clip(lower=0)
    w_safe = w.where(keep, 1.0)

    fs = np.sqrt(u_safe)
    fT = fs / np.sqrt(w_safe)

    keep &= _guard(
        fs > 0,
        "reconstructed S12f (case 1) is not strictly positive",
        cycles_to_failure,
        on_invalid_point,
    )
    keep &= _guard(
        fT > 0,
        "reconstructed S2f (case 1) is not strictly positive",
        cycles_to_failure,
        on_invalid_point,
    )

    return (fT * tensile_transverse_strength).where(keep), (fs * shear_strength).where(
        keep
    )


def execute(
    snc_input_x_json_file: FilePath | ReadCsvBuffer,
    snc_input_panel2_json_file: FilePath | ReadCsvBuffer,
    snc_input_panel3_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    fatigue_model: faf.FatigueModel,
    desirable_angle: float,
    off_axis_angle1: float,
    off_axis_angle2: float,
    tensile_transverse_strength: float,
    shear_strength: float,
    tensile_strength1: float,
    tensile_strength2: float,
    tensile_strength_at_desirable_angle: float,
    tensile_axial_strength: float,
    panel2_type: HashinRotemPanelType,
    panel3_type: HashinRotemPanelType,
    *,
    on_invalid_point: InvalidPointPolicy = "nan",
) -> None:
    """
    Execute the Hashin-Rotem algorithm. UD laminates, tension loading only.
    Parameters
    ----------
        snc_input_x_json_file: FilePath | ReadCsvBuffer
            Longitudinal (X) fatigue data - always required, S1f(N) source
            for the fiber failure mode (book Eq. 6.2a).
        snc_input_panel2_json_file: FilePath | ReadCsvBuffer
            Panel 2 fatigue data - meaning set by panel2_type.
        snc_input_panel3_json_file: FilePath | ReadCsvBuffer
            Panel 3 fatigue data - meaning set by panel3_type.
        faf_output_csv_file: FilePath | WriteBuffer
        faf_output_json_file: Optional[FilePath | WriteBuffer]
        fatigue_model: faf.FatigueModel
            Book Eq. 6.28 (sigma_max = sigma_0 * N^(-1/k)) is Log-Log, used
            throughout Chapter 6 (see p. 174). Lin-Log is a supported
            extension of this implementation, not a book-sanctioned form.
        desirable_angle: float
        off_axis_angle1: float
            theta1 [degrees] - panel 2's off-axis angle, only meaningful if
            panel2_type is OFF_AXIS.
        off_axis_angle2: float
            theta2 [degrees] - panel 3's off-axis angle, only meaningful if
            panel3_type is OFF_AXIS.
        tensile_transverse_strength: float
        shear_strength: float
        tensile_strength1: float
            Static strength at theta1, only meaningful if panel2_type is
            OFF_AXIS.
        tensile_strength2: float
            Static strength at theta2, only meaningful if panel3_type is
            OFF_AXIS.
        tensile_strength_at_desirable_angle: float
        tensile_axial_strength: float
            sigma_A^s, static axial (0 deg, fiber-direction) tensile
            strength - the Fortran's TenAxStr. Feeds Eq. 6.1 (critical fiber
            angle) and Eq. 6.2a (fiber failure mode).
        panel2_type: HashinRotemPanelType
        panel3_type: HashinRotemPanelType
        on_invalid_point: InvalidPointPolicy
            What to do with an N for which the reconstruction or the fiber
            fatigue function hits an invalid (negative-radicand,
            near-zero-denominator, non-positive-strength) point - see
            `_guard`. Defaults to "nan" (drop just that row's result,
            log a warning, keep the rest of the curve).
    Returns
    -------
        None
    """

    # Import input files (SNC format)
    snc_1_df = pd.read_json(snc_input_x_json_file, orient="records")
    snc_2_df = pd.read_json(snc_input_panel2_json_file, orient="records")
    snc_a_df = pd.read_json(snc_input_panel3_json_file, orient="records")

    r1 = float(snc_1_df.iloc[0].stress_ratio)
    r2 = float(snc_2_df.iloc[0].stress_ratio)
    ra = float(snc_a_df.iloc[0].stress_ratio)

    # This implementation covers tension-tension (UD, 0 <= R < 1) loading
    # only - R < 0 (tension-compression, e.g. R=-1 fully reversed) and R>=1
    # are computed with tension-only formulas elsewhere in this module,
    # which is not justified for those regimes. Checked on each file's own
    # stress_ratio individually, not on a reconciled/averaged value, so a
    # single out-of-range file can't be masked by the other two.
    for label, r in (("X", r1), ("panel2", r2), ("panel3", ra)):
        if not (0 <= r < 1):
            raise ValueError(
                "HashinRotem covers tension-tension loading only "
                f"(0 <= stress_ratio < 1); {label} file has stress_ratio={r}."
            )

    ci1 = float(snc_1_df.iloc[0].confidence_interval)
    ci2 = float(snc_2_df.iloc[0].confidence_interval)
    cia = float(snc_a_df.iloc[0].confidence_interval)

    # The 3 input files are expected to already agree on stress_ratio and
    # confidence_interval (they describe the same test campaign at
    # different angles/loading directions). Previously, disagreement was
    # silently papered over by falling back to stress_ratio=1 / CI=0 -
    # producing a full, plausible-looking curve computed from mutually
    # inconsistent data. Raise instead and name the conflicting values.
    r_avg = (r1 + r2 + ra) / 3
    if not np.isclose(r_avg, r1):
        raise ValueError(
            "HashinRotem: input files disagree on stress_ratio "
            f"(X={r1}, panel2={r2}, panel3={ra})."
        )
    stress_ratio = r1

    ci_avg = (ci1 + ci2 + cia) / 3
    if not np.isclose(ci_avg, ci1):
        raise ValueError(
            "HashinRotem: input files disagree on confidence_interval "
            f"(X={ci1}, panel2={ci2}, panel3={cia})."
        )
    confidence_interval = ci1

    theta = np.radians(desirable_angle)

    # Select appropriate stress function (log-log vs lin-log). Any value
    # other than the 2 known FatigueModel members used to silently fall
    # through to Lin-Log - raise instead.
    if fatigue_model == faf.FatigueModel.LOG_LOG:
        get_stress = get_loglog_stress
    elif fatigue_model == faf.FatigueModel.LIN_LOG:
        get_stress = get_linlog_stress
    else:
        raise ValueError(
            f"HashinRotem: unknown fatigue_model={fatigue_model!r}; "
            "expected 'Log-Log' or 'Lin-Log'."
        )

    faf_csv_df = pd.DataFrame(
        LIST_CYCLES_TO_FAILURE,
        columns=["cycles_to_failure"],
    )
    faf_csv_df["stress_ratio"] = stress_ratio
    cycles = faf_csv_df.cycles_to_failure

    # S1f/S2f/S12f (book Sec. 6.2.1) - each panel's own fitted curve
    # evaluated directly over the N grid, S2f/S12f then reconstructed from
    # whichever two of {Transverse, Shear, Off-axis} panel2/panel3 actually
    # are.
    a_x, b_x = snc_1_df.iloc[0].a, snc_1_df.iloc[0].b
    a_panel2, b_panel2 = snc_2_df.iloc[0].a, snc_2_df.iloc[0].b
    a_panel3, b_panel3 = snc_a_df.iloc[0].a, snc_a_df.iloc[0].b

    faf_csv_df["s1f"] = get_stress(a_x, b_x, cycles)
    panel2_curve = get_stress(a_panel2, b_panel2, cycles)
    panel3_curve = get_stress(a_panel3, b_panel3, cycles)

    faf_csv_df["s2f"], faf_csv_df["s12f"] = reconstruct_s2f_s12f(
        panel2_type=panel2_type,
        panel2_curve=panel2_curve,
        panel3_type=panel3_type,
        panel3_curve=panel3_curve,
        off_axis_angle1=off_axis_angle1,
        off_axis_angle2=off_axis_angle2,
        tensile_strength1=tensile_strength1,
        tensile_strength2=tensile_strength2,
        shear_strength=shear_strength,
        tensile_transverse_strength=tensile_transverse_strength,
        cycles_to_failure=cycles,
        on_invalid_point=on_invalid_point,
    )

    # --- Failure mode selection (book Eqs. 6.1, 6.2a, 6.2b) ---------------
    # f_A(N): axial fatigue function, feeds the fiber mode (Eq. 6.2a). Under
    # uniaxial off-axis loading, sigma_A = sigma_x * cos^2(theta), so
    # Eq. 6.2a (sigma_A = sigma_A^u) gives sigma_x = sigma_A^s * f_A(N) /
    # cos^2(theta); since the static off-axis strength in the fiber mode is
    # itself sigma_A^s / cos^2(theta), the off-axis fatigue function in the
    # fiber mode reduces to f''_fiber(N) = f_A(N).
    f_a = faf_csv_df["s1f"] / tensile_axial_strength
    keep_fa = _guard(
        f_a > 0,
        "S1f-derived fiber fatigue function f_A (=s1f/tensile_axial_strength) "
        "is not strictly positive",
        cycles,
        on_invalid_point,
    )
    f_a_safe = f_a.where(keep_fa)

    fT = faf_csv_df["s2f"] / tensile_transverse_strength
    fs = faf_csv_df["s12f"] / shear_strength

    # f_matrix(N): matrix-mode off-axis fatigue function (Eq. 6.3), derived
    # exclusively from the verified s2f/s12f reconstruction above.
    f_matrix = hashin_rotem_off_axis_fatigue_function(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=tensile_transverse_strength,
        f_t=fT,
    )

    # A row is only usable once both the reconstruction (s2f/s12f) and f_A
    # are valid at that N.
    row_valid = faf_csv_df["s2f"].notna() & faf_csv_df["s12f"].notna() & keep_fa

    # Eq. 6.1 - critical fiber angle, evaluated per N (f_A and f_tau both
    # vary with N, so theta_c is a curve, not a single number - the
    # governing mode can switch partway along a single S-N curve).
    theta_c = np.arctan((shear_strength / tensile_axial_strength) * (fs / f_a_safe))
    fiber_governs = (theta < theta_c) & row_valid
    matrix_governs = (theta >= theta_c) & row_valid

    # --- Diagnostic: Eq. 6.1 (small-angle) vs the exact Eq. 6.2a/6.2b ------
    # crossover. Eq. 6.1 is derived by dropping the sin^4(theta) term of Eq.
    # 6.2b's quadratic interaction (negligible for small theta, since
    # sin^2(theta) ~ theta^2 there dominates sin^4(theta) ~ theta^4) - so
    # its mode selection can disagree with the exact criterion near
    # theta_c. This is diagnostic only: the actual mode selection above
    # always follows the book's Eq. 6.1 test, never this exact comparison.
    with np.errstate(divide="ignore", invalid="ignore"):
        sin_t, cos_t = np.sin(theta), np.cos(theta)
        sigma_fiber = tensile_axial_strength * f_a_safe / cos_t**2
        matrix_denorm = (sin_t**4) / (tensile_transverse_strength * fT) ** 2 + (
            sin_t**2 * cos_t**2
        ) / (shear_strength * fs) ** 2
        sigma_matrix_exact = 1 / np.sqrt(matrix_denorm)
        fiber_governs_exact = (sigma_fiber < sigma_matrix_exact) & row_valid
    disagreement = row_valid & (fiber_governs != fiber_governs_exact)
    if disagreement.any():
        warnings.warn(
            "HashinRotem: Eq. 6.1's small-angle critical-angle test and the "
            "exact Eq. 6.2a/6.2b crossover disagree on which failure mode "
            f"governs for {_format_n_range(cycles, disagreement)}. Eq. 6.1 "
            "drops the sin^4(theta) term of Eq. 6.2b (negligible only for "
            "small theta); near theta_c the two criteria can pick different "
            "modes. The book's Eq. 6.1 test is used for the reported "
            "failure_mode/stress_max, as specified - this disagreement "
            "needs human review."
        )

    faf_csv_df["failure_mode"] = pd.Series(
        np.select([fiber_governs, matrix_governs], ["fiber", "matrix"], default=np.nan),
        index=faf_csv_df.index,
    )
    faf_csv_df["critical_angle"] = np.degrees(theta_c).where(row_valid)

    f_prime = f_a_safe.where(fiber_governs, f_matrix).where(row_valid)
    faf_csv_df["stress_max"] = tensile_strength_at_desirable_angle * f_prime

    faf_json_df = pd.DataFrame(
        {
            "stress_ratio": [stress_ratio],
            "confidence_interval": [confidence_interval],
            "off_axis_angle1": off_axis_angle1,
            "off_axis_angle2": off_axis_angle2,
            "panel2_type": panel2_type.value,
            "panel3_type": panel3_type.value,
        }
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[
        [
            "stress_ratio",
            "cycles_to_failure",
            "stress_max",
            "s1f",
            "s2f",
            "s12f",
            "failure_mode",
            "critical_angle",
        ]
    ].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

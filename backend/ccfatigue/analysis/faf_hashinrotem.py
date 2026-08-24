#!/usr/bin/env python
"""
This code takes in SNC and outputs FAF data
H-R is described in Fatigue of Fiber-reinforced Composites [1], p159
The original paper is
[1] https://link.springer.com/book/10.1007/978-1-84996-181-3
    DOI 10.1007/978-1-84996-181-3
[2] https://journals.sagepub.com/doi/10.1177/002199837300700404
    https://drive.google.com/drive/u/1/folders/1SVank5TEcCxHG5vIgcmxaR9zAov8qj2l?ths=true
"""

from itertools import chain
from typing import Optional

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


def hashin_equation_23(
    f_tau: float,
    tau_s: float,
    theta: float,
    sigma_s_t: float,
    f_t: float,
):
    """Eq 23, ref [2] p455
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
        return curve_for(transverse), curve_for(shear)

    if types == {transverse, off_axis}:
        transverse_curve = curve_for(transverse)
        off_axis_curve = curve_for(off_axis)
        off_axis_angle = off_axis_angle1 if panel2_type == off_axis else off_axis_angle2
        off_axis_strength = (
            tensile_strength1 if panel2_type == off_axis else tensile_strength2
        )
        t2 = np.tan(np.radians(off_axis_angle)) ** 2
        fT = transverse_curve / tensile_transverse_strength
        F2 = off_axis_curve / off_axis_strength
        radicand = (1 + r**2 * t2) - (F2**2 * r**2 * t2) / fT**2
        if (radicand < 0).any():
            raise ValueError(
                "HashinRotem: case 2 reconstruction hit a negative value "
                "under a square root - check the off-axis/transverse "
                "strength inputs."
            )
        fs = F2 / np.sqrt(radicand)
        return transverse_curve, fs * shear_strength

    if types == {shear, off_axis}:
        shear_curve = curve_for(shear)
        off_axis_curve = curve_for(off_axis)
        off_axis_angle = off_axis_angle1 if panel2_type == off_axis else off_axis_angle2
        off_axis_strength = (
            tensile_strength1 if panel2_type == off_axis else tensile_strength2
        )
        t1 = np.tan(np.radians(off_axis_angle)) ** 2
        fs = shear_curve / shear_strength
        F1 = off_axis_curve / off_axis_strength
        radicand = fs**2 * (1 + r**2 * t1) - F1**2
        if (radicand < 0).any():
            raise ValueError(
                "HashinRotem: case 3 reconstruction hit a negative value "
                "under a square root - check the off-axis/shear strength "
                "inputs."
            )
        fT = (F1 * r * np.sqrt(t1) * fs) / np.sqrt(radicand)
        return fT * tensile_transverse_strength, shear_curve

    # types == {off_axis} - both panels are off-axis (panel2 <-> theta1,
    # panel3 <-> theta2, by construction of how off_axis_angle1/2 and
    # tensile_strength1/2 are wired to panel2/panel3 respectively).
    if np.isclose(off_axis_angle1, off_axis_angle2):
        raise ValueError(
            "HashinRotem: case 1 reconstruction needs two distinct "
            "off-axis angles (theta1 != theta2)."
        )
    t1 = np.tan(np.radians(off_axis_angle1)) ** 2
    t2 = np.tan(np.radians(off_axis_angle2)) ** 2
    F1 = panel2_curve / tensile_strength1
    F2 = panel3_curve / tensile_strength2

    det = (1 + r**2 * t1) * (-(F2**2) * r**2 * t2) - (
        -(F1**2) * r**2 * t1
    ) * (1 + r**2 * t2)
    if (np.abs(det) < 1e-12).any():
        raise ValueError(
            "HashinRotem: case 1 reconstruction is degenerate (determinant "
            "near zero) for at least one N - check the off-axis angles and "
            "strengths."
        )

    u = (
        F1**2 * (-(F2**2) * r**2 * t2) - (-(F1**2) * r**2 * t1) * F2**2
    ) / det
    w = ((1 + r**2 * t1) * F2**2 - F1**2 * (1 + r**2 * t2)) / det

    if (u < 0).any():
        raise ValueError(
            "HashinRotem: case 1 reconstruction hit a negative value under "
            "a square root (fs) - check the off-axis strength inputs."
        )
    if (w <= 0).any():
        raise ValueError(
            "HashinRotem: case 1 reconstruction hit a non-positive value "
            "under a square root (fT) - check the off-axis strength inputs."
        )

    fs = np.sqrt(u)
    fT = fs / np.sqrt(w)
    return fT * tensile_transverse_strength, fs * shear_strength


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
    panel2_type: HashinRotemPanelType,
    panel3_type: HashinRotemPanelType,
) -> None:
    """
    Execute the Hashin-Rotem algorithm. Tension loading only.
    Parameters
    ----------
        snc_input_x_json_file: FilePath | ReadCsvBuffer
            Longitudinal (X) fatigue data - always required, S1f(N) source.
        snc_input_panel2_json_file: FilePath | ReadCsvBuffer
            Panel 2 fatigue data - meaning set by panel2_type.
        snc_input_panel3_json_file: FilePath | ReadCsvBuffer
            Panel 3 fatigue data - meaning set by panel3_type.
        faf_output_csv_file: FilePath | WriteBuffer
        faf_output_json_file: Optional[FilePath | WriteBuffer]
        fatigue_model: faf.FatigueModel
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
        panel2_type: HashinRotemPanelType
        panel3_type: HashinRotemPanelType
    Returns
    -------
        None
    """

    # Import input files (SNC format)
    snc_1_df = pd.read_json(snc_input_x_json_file, orient="records")
    snc_2_df = pd.read_json(snc_input_panel2_json_file, orient="records")
    snc_a_df = pd.read_json(snc_input_panel3_json_file, orient="records")

    r1 = snc_1_df.iloc[0].stress_ratio
    r2 = snc_2_df.iloc[0].stress_ratio
    ra = snc_a_df.iloc[0].stress_ratio

    # Checked on each file's own stress_ratio individually, not on the
    # reconciled/averaged value below - that reconciliation silently falls
    # back to 1 whenever the 3 files disagree, which would let a single
    # compression-side file (stress_ratio > 1) slip through undetected if
    # the other two happened to average close enough to mask it.
    if r1 > 1 or r2 > 1 or ra > 1:
        raise ValueError(
            "Hashin-Rotem only supports tension loading, stress_ratio must " "be <= 1"
        )

    theta = np.radians(desirable_angle)
    oangle1 = np.radians(off_axis_angle1)
    oangle2 = np.radians(off_axis_angle2)

    stress_ratio = (
        float(r1)
        if np.isclose(
            (r1 + r2 + ra) / 3,
            r1,
        )
        else 1
    )

    confidence_interval = (
        float(snc_1_df.iloc[0].confidence_interval)
        if np.isclose(
            (
                snc_1_df.iloc[0].confidence_interval
                + snc_2_df.iloc[0].confidence_interval
                + snc_a_df.iloc[0].confidence_interval
            )
            / 3,
            snc_1_df.iloc[0].confidence_interval,
        )
        else 0
    )

    faf_csv_df = pd.DataFrame(
        LIST_CYCLES_TO_FAILURE,
        columns=["cycles_to_failure"],
    )

    faf_csv_df["stress_ratio"] = stress_ratio

    # Select appropriate stress function (log-log vs lin-log)
    if fatigue_model == "Log-Log":
        get_stress = get_loglog_stress
    else:
        get_stress = get_linlog_stress

    # S1f/S2f/S12f (book Sec. 6.2.1) - each panel's own fitted curve
    # evaluated directly over the N grid, S2f/S12f then reconstructed from
    # whichever two of {Transverse, Shear, Off-axis} panel2/panel3 actually
    # are.
    a_x, b_x = snc_1_df.iloc[0].a, snc_1_df.iloc[0].b
    a_panel2, b_panel2 = snc_2_df.iloc[0].a, snc_2_df.iloc[0].b
    a_panel3, b_panel3 = snc_a_df.iloc[0].a, snc_a_df.iloc[0].b

    faf_csv_df["s1f"] = get_stress(a_x, b_x, faf_csv_df.cycles_to_failure)
    panel2_curve = get_stress(a_panel2, b_panel2, faf_csv_df.cycles_to_failure)
    panel3_curve = get_stress(a_panel3, b_panel3, faf_csv_df.cycles_to_failure)

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
    )

    # S-N curve at the desired angle (book Eq. 6.3, hashin_equation_23),
    # derived exclusively from the verified s2f/s12f reconstruction above -
    # replaces the old legacy case1/2/3 blend, which was never verified
    # against the book (it cited a different reference, "Eq 23, ref [2]").
    fT = faf_csv_df["s2f"] / tensile_transverse_strength
    fs = faf_csv_df["s12f"] / shear_strength
    f_prime = hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=tensile_transverse_strength,
        f_t=fT,
    )
    faf_csv_df["stress_max"] = tensile_strength_at_desirable_angle * f_prime

    faf_json_df = pd.DataFrame(
        {
            "stress_ratio": [stress_ratio],
            "confidence_interval": [confidence_interval],
            "off_axis_angle1": oangle1,
            "off_axis_angle2": oangle2,
            "panel2_type": panel2_type.value,
            "panel3_type": panel3_type.value,
        }
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[
        ["stress_ratio", "cycles_to_failure", "stress_max", "s1f", "s2f", "s12f"]
    ].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

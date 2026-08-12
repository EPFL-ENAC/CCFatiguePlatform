#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf_kawai.py
This code takes in raw multi-stress-ratio fatigue data (AGG format) and outputs FAF data

Kawai normalizes off-axis fatigue data recorded at several stress ratios
into a single normalized-stress-vs-reversals curve (fitted independently
per reference stress ratio group, forced through the fixed point
RSigma=1, 2N=10), then re-expands each fitted curve for a target
(desirable) angle and stress ratio using a Tsai-Hill-type strength
combination factor (Omega).
Translated from a Fortran source (program KW) provided by the user; no
public reference for this file was found in the current CCFatiguePlatform
repository.
"""

from itertools import chain
from typing import Optional

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(2000000, 20000001, 1000000),
    )
)


def get_omega(
    angle: float,
    tensile_axial_strength: float,
    tensile_transverse_strength: float,
    shear_strength: float,
) -> float:
    """
    Kawai strength-combination factor (Omega in the original Fortran).
    Parameters
    ----------
        angle: float
            Angle [rad]
        tensile_axial_strength: float
        tensile_transverse_strength: float
        shear_strength: float
    Returns
    -------
        omega: float
    """
    omega = (
        np.cos(angle) ** 4 / tensile_axial_strength**2
        + np.sin(angle) ** 4 / tensile_transverse_strength**2
        + (np.sin(angle) * np.cos(angle)) ** 2
        * (1 / shear_strength**2 - 1 / tensile_axial_strength**2)
    ) ** 0.5
    return omega


def get_normalized_stress(
    stress_max, reference_stress_ratio: float, reference_static_strength: float
):
    """
    Kawai normalized (reversed) stress ratio, called RSigma in the original
    Fortran.
    Parameters
    ----------
        stress_max: float
            Applied cyclic stress amplitude (sigma_a) [MPa]
        reference_stress_ratio: float
            Reference stress ratio (R) [-]
        reference_static_strength: float
            Static strength associated with the reference stress ratio [MPa]
    Returns
    -------
        normalized_stress: float
    """
    ratio = stress_max / reference_static_strength
    normalized_stress = (0.5 * (1 - reference_stress_ratio) * ratio) / (
        1 - 0.5 * (1 + reference_stress_ratio) * ratio
    )
    return normalized_stress


def get_nstar(log_normalized_stress: pd.Series, log_reversals: pd.Series) -> float:
    """
    Fit nstar (the S-N curve exponent) by forcing the fitted line through
    the fixed point (log_normalized_stress=0, log_reversals=1).
    Parameters
    ----------
        log_normalized_stress: pd.Series
            log10(RSigma) for every point in the group
        log_reversals: pd.Series
            log10(2*cycles_to_failure) for every point in the group
    Returns
    -------
        nstar: float
    """
    count = len(log_normalized_stress)
    nstar = -(log_reversals.sum() - count) / log_normalized_stress.sum()
    return nstar


def get_stress_max(
    nstar: float, omega: float, target_stress_ratio: float, cycles_to_failure
):
    """
    Invert the Kawai master curve back to a physical stress amplitude at a
    target angle/stress ratio (SPN in the original Fortran).
    Parameters
    ----------
        nstar: float
        omega: float
        target_stress_ratio: float
        cycles_to_failure: float
    Returns
    -------
        stress_max: float
    """
    sigma = (2 * cycles_to_failure) ** (-1 / nstar)
    stress_max = (2 * sigma) / (
        omega * ((1 - target_stress_ratio) + (1 + target_stress_ratio) * sigma)
    )
    return stress_max


def execute(
    agg_input_csv_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    reference_angle: float,
    reference_stress_ratio: float,
    reference_static_strength: float,
    desirable_angle: float,
    target_stress_ratio: float,
    tensile_axial_strength: float,
    tensile_transverse_strength: float,
    shear_strength: float,
) -> None:
    """
    Execute the Kawai algorithm.

    The reference data (agg_input_csv_file) may contain several stress
    ratio groups (e.g. R=0.1, R=-1, ...). Each group is normalized with the
    SAME reference_stress_ratio/reference_static_strength, then fitted
    independently in log-log (reversals) space, and each resulting curve is
    re-expanded at the target (desirable_angle, target_stress_ratio) -
    producing one predicted curve per reference stress ratio group, so
    results can be cross-checked across groups.
    Parameters
    ----------
        agg_input_csv_file: FilePath | ReadCsvBuffer
            Reference multi-stress-ratio fatigue data (AGG csv)
        faf_output_csv_file: FilePath | WriteBuffer
            output (FAF CSV)
        faf_output_json_file: FilePath | WriteBuffer
            output (FAF json)
        reference_angle: float
            Off-axis angle of the reference (tested) data [degrees]
        reference_stress_ratio: float
            Stress ratio (R) used to normalize every group's applied stress
            [-]
        reference_static_strength: float
            Static strength associated with reference_stress_ratio [MPa]
        desirable_angle: float
            Target/design angle [degrees]
        target_stress_ratio: float
            Target/design stress ratio (R) [-]
        tensile_axial_strength: float
        tensile_transverse_strength: float
        shear_strength: float
    Returns
    -------
        None
    """
    if not abs(target_stress_ratio) < 1:
        raise NotImplementedError(f"target_stress_ratio={target_stress_ratio}")

    agg_df = pd.read_csv(agg_input_csv_file)

    target_angle_rad = np.radians(desirable_angle)
    omega = get_omega(
        target_angle_rad,
        tensile_axial_strength,
        tensile_transverse_strength,
        shear_strength,
    )

    agg_df["normalized_stress"] = get_normalized_stress(
        agg_df.stress_max, reference_stress_ratio, reference_static_strength
    )
    agg_df["log_normalized_stress"] = np.log10(agg_df.normalized_stress)
    agg_df["log_reversals"] = np.log10(2 * agg_df.cycles_to_failure)

    faf_csv_df = pd.DataFrame()
    faf_json_records = []

    for reference_group_stress_ratio, group_df in agg_df.groupby("stress_ratio"):
        nstar = get_nstar(group_df.log_normalized_stress, group_df.log_reversals)

        curve_df = pd.DataFrame(LIST_CYCLES_TO_FAILURE, columns=["cycles_to_failure"])
        curve_df["stress_ratio"] = reference_group_stress_ratio
        curve_df["stress_max"] = get_stress_max(
            nstar,
            omega,
            target_stress_ratio,
            curve_df.cycles_to_failure,
        )
        faf_csv_df = pd.concat([faf_csv_df, curve_df])

        faf_json_records.append(
            {
                "stress_ratio": reference_group_stress_ratio,
                "reference_angle": reference_angle,
                "target_stress_ratio": target_stress_ratio,
                "desirable_angle": desirable_angle,
                "nstar": nstar,
                "omega": omega,
            }
        )

    faf_json_df = pd.DataFrame(faf_json_records)

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[["stress_ratio", "cycles_to_failure", "stress_max"]].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

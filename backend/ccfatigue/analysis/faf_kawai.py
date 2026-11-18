#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf_kawai.py
This code takes in raw multi-stress-ratio fatigue data (AGG format) and outputs FAF data

Kawai normalizes fatigue data recorded at one reference angle and one or more
stress ratios into a single master S-N curve.  The implementation uses a
free-intercept least-squares fit in log space and stores the result as

    normalized_stress = A * cycles_to_failure ** (-1 / nstar)

The master curve is then transformed to a target angle and stress ratio using
the Kawai orientation factor (Omega). The equations follow Kawai (2004),
bundled with the backend tests as ``tests/data/kawai.pdf``.
"""

from typing import Optional

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

from ccfatigue.analysis.utils.faf import LIST_CYCLES_TO_FAILURE


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


def get_normalized_stress(stress_max, stress_ratio, reference_omega: float):
    """
    Kawai normalized (reversed) stress ratio, called RSigma in the original
    Fortran.
    Parameters
    ----------
        stress_max: float
            Maximum applied cyclic stress [MPa]
        stress_ratio: float
            Stress ratio for each input row (R) [-]
        reference_omega: float
            Orientation factor at the common reference angle [1/MPa]
    Returns
    -------
        normalized_stress: float
    """

    ratio = stress_max * reference_omega
    normalized_stress = (0.5 * (1 - stress_ratio) * ratio) / (
        1 - 0.5 * (1 + stress_ratio) * ratio
    )
    return normalized_stress


def get_master_curve_parameters(
    log_normalized_stress: pd.Series, log_reversals: pd.Series
) -> tuple[float, float]:
    """
    Fit the Kawai master curve by least squares in log space.

    The regression performed is

        log10(2*N) = slope * log10(normalized_stress) + intercept

    It is returned in the directly evaluable form

        normalized_stress = A * N ** (-1/nstar)

    Parameters
    ----------
        log_normalized_stress: pd.Series
            log10(normalized_stress) for every input point
        log_reversals: pd.Series
            log10(2*cycles_to_failure) for every point in the group
    Returns
    -------
        a: float
            Master-curve coefficient A (normalized stress at N=1)
        nstar: float
            Positive master-curve exponent
    """
    slope, log_reversal_intercept = np.polyfit(
        log_normalized_stress, log_reversals, deg=1
    )
    nstar = -float(slope)
    reversal_intercept = 10 ** float(log_reversal_intercept)
    a = (reversal_intercept / 2) ** (1 / nstar)
    return a, nstar


def get_stress_max(
    a: float,
    nstar: float,
    omega: float,
    target_stress_ratio: float,
    cycles_to_failure,
):
    """
    Invert the Kawai master curve back to a physical stress amplitude at a
    target angle/stress ratio (SPN in the original Fortran).

    The normalized master curve is evaluated as

        normalized_stress = A * N ** (-1/nstar)

    and then transformed using Kawai's equation 23.
    Parameters
    ----------
        a: float
            Master-curve coefficient A
        nstar: float
            Positive master-curve exponent
        omega: float
        target_stress_ratio: float
        cycles_to_failure: float
    Returns
    -------
        stress_max: float
    """
    normalized_stress = a * cycles_to_failure ** (-1 / nstar)
    stress_max = (2 * normalized_stress) / (
        omega
        * (
            (1 - target_stress_ratio)
            + (1 + target_stress_ratio) * normalized_stress
        )
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

    1. Normalize AGG stresses recorded at one reference angle. Rows may have
       different stress ratios; each row uses its own stress ratio.
    2. Fit one master curve to all normalized points:
       normalized_stress = A * N ** (-1/nstar).
    3. Evaluate that analytical master curve on the output cycle grid and
       transform it to the requested angle and stress ratio.
    Parameters
    ----------
        agg_input_csv_file: FilePath | ReadCsvBuffer
            Reference multi-stress-ratio fatigue data (AGG csv)
        faf_output_csv_file: FilePath | WriteBuffer
            output (FAF CSV)
        faf_output_json_file: FilePath | WriteBuffer
            Output metadata. ``a`` and ``nstar`` define the normalized master
            curve ``Sigma(N) = a * N ** (-1/nstar)``.
        reference_angle: float
            Off-axis angle of the reference (tested) data [degrees]
        reference_stress_ratio: float
            Reference material-curve stress ratio retained for API metadata;
            normalization uses each AGG row's stress ratio instead.
        reference_static_strength: float
            Reference static strength retained for API compatibility. The
            Kawai normalization uses the strengths in the orientation factor.
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
    if not (-1 <= target_stress_ratio < 1):
        raise ValueError(
            f"Kawai: Invalid target stress ratio R={target_stress_ratio} "
            "(must be in the [-1,1) interval)"
        )

    agg_df = pd.read_csv(agg_input_csv_file)

    if ((agg_df.stress_ratio >= 1) | (agg_df.stress_ratio < -1)).any():
        raise ValueError(
            "Kawai: Can't calculate normalized stress: at least one data "
            "point has R < -1 or R >= 1"
        )

    target_angle_rad = np.radians(desirable_angle)
    omega = get_omega(
        target_angle_rad,
        tensile_axial_strength,
        tensile_transverse_strength,
        shear_strength,
    )

    reference_omega = get_omega(
        np.radians(reference_angle),
        tensile_axial_strength,
        tensile_transverse_strength,
        shear_strength,
    )

    agg_df["normalized_stress"] = get_normalized_stress(
        agg_df.stress_max, agg_df.stress_ratio, reference_omega
    )

    agg_df["log_normalized_stress"] = np.log10(agg_df.normalized_stress)
    agg_df["log_reversals"] = np.log10(2 * agg_df.cycles_to_failure)

    faf_csv_df = pd.DataFrame()
    faf_json_records = []

    a, nstar = get_master_curve_parameters(
        agg_df.log_normalized_stress, agg_df.log_reversals
    )
    curve_df = pd.DataFrame(LIST_CYCLES_TO_FAILURE, columns=["cycles_to_failure"])
    curve_df["stress_ratio"] = target_stress_ratio
    curve_df["stress_max"] = get_stress_max(
        a,
        nstar,
        omega,
        target_stress_ratio,
        curve_df.cycles_to_failure,
    )
    faf_csv_df = pd.concat([faf_csv_df, curve_df])

    faf_json_records.append(
        {
            "reference_angle": reference_angle,
            "reference_stress_ratio": reference_stress_ratio,
            "target_stress_ratio": target_stress_ratio,
            "desirable_angle": desirable_angle,
            "a": a,
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

#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf_shokriehtaheri.py
This code takes in raw multi-stress-ratio fatigue data (AGG format) and outputs FAF data

Shokrieh-Taheri uses a Tsai-Hill-type static failure criterion to combine
off-axis fatigue data recorded at several stress ratios into a single
equivalent-stress S-N curve (fitted independently per reference stress ratio),
then re-expands each fitted curve for a target (desirable) angle and stress
ratio.
Translated from
https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/ST/Fatigue-Failure-Shokrieh-Taheri.for
"""

from itertools import chain
from typing import Optional

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(2000000, 20000001, 1000000),
    )
)


def get_tsai_hill_transform(
    angle: float,
    stress_ratio: float,
    tensile_axial_strength: float,
    compressive_axial_strength: float,
    tensile_transverse_strength: float,
    compressive_transverse_strength: float,
    shear_strength: float,
) -> float:
    """
    Tsai-Hill static-failure transform factor for a given off-axis angle,
    selecting tensile or compressive strengths according to the stress ratio
    regime (RTransform/Transform in the original Fortran).
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/ST/Fatigue-Failure-Shokrieh-Taheri.for#L27-L45
    Parameters
    ----------
        angle: float
            Off-axis angle [rad]
        stress_ratio: float
            Stress ratio (R) [-]
        tensile_axial_strength: float
        compressive_axial_strength: float
        tensile_transverse_strength: float
        compressive_transverse_strength: float
        shear_strength: float
    Returns
    -------
        transform: float
    """
    if 0 <= stress_ratio < 1:
        axial_strength = tensile_axial_strength
        transverse_strength = tensile_transverse_strength
    elif stress_ratio >= 1:
        axial_strength = compressive_axial_strength
        transverse_strength = compressive_transverse_strength
    else:
        raise NotImplementedError(f"stress_ratio={stress_ratio}")

    transform = (
        np.cos(angle) ** 4 / axial_strength**2
        + np.sin(angle) ** 4 / transverse_strength**2
        + (np.sin(angle) * np.cos(angle)) ** 2 / shear_strength**2
    )
    return transform


def get_equivalent_stress(stress_max, stress_ratio: float, transform: float):
    """
    Shokrieh-Taheri Tsai-Hill equivalent (normalized) stress, called DW in the
    original Fortran.
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/ST/Fatigue-Failure-Shokrieh-Taheri.for#L79
    Parameters
    ----------
        stress_max: float
            Applied cyclic stress amplitude (sigma_a) [MPa]
        stress_ratio: float
            Stress ratio (R) [-]
        transform: float
            Tsai-Hill transform factor
    Returns
    -------
        equivalent_stress: float
    """
    equivalent_stress = (1 - stress_ratio**2) * stress_max**2 * transform
    return equivalent_stress


def get_stress_max(
    aa: float, bb: float, cycles_to_failure, stress_ratio: float, transform: float
):
    """
    Invert the Shokrieh-Taheri master curve equivalent_stress(N) = aa * N^(-bb)
    back to a physical stress amplitude at a target angle/stress ratio (SPN in
    the original Fortran).
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/ST/Fatigue-Failure-Shokrieh-Taheri.for#L131
    Parameters
    ----------
        aa: float
        bb: float
        cycles_to_failure: float
        stress_ratio: float
        transform: float
    Returns
    -------
        stress_max: float
    """
    stress_max = np.sqrt(
        (aa * cycles_to_failure ** (-bb)) / ((1 - stress_ratio**2) * transform)
    )
    return stress_max


def execute(
    agg_input_csv_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    reference_angle: float,
    reference_stress_ratio: float,
    desirable_angle: float,
    target_stress_ratio: float,
    tensile_axial_strength: float,
    compressive_axial_strength: float,
    tensile_transverse_strength: float,
    compressive_transverse_strength: float,
    shear_strength: float,
) -> None:
    """
    Execute the Shokrieh-Taheri algorithm.

    The reference data (agg_input_csv_file) may contain several stress ratio
    groups (e.g. R=0.1, R=-1, ...), all measured at reference_angle. Each
    group is normalized with the SAME reference_stress_ratio (regime
    selection and (1-R^2) scaling), then fitted independently in log-log
    space, and each resulting curve is re-expanded at the target
    (desirable_angle, target_stress_ratio) - producing one predicted curve per
    reference stress ratio group, so results can be cross-checked across
    groups.
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
            Stress ratio (R) used to select the reference data's
            tensile/compressive strength regime and to normalize every group
            [-]
        desirable_angle: float
            Target/design angle [degrees]
        target_stress_ratio: float
            Target/design stress ratio (R) [-]
        tensile_axial_strength: float
        compressive_axial_strength: float
        tensile_transverse_strength: float
        compressive_transverse_strength: float
        shear_strength: float
    Returns
    -------
        None
    """
    agg_df = pd.read_csv(agg_input_csv_file)

    reference_angle_rad = np.radians(reference_angle)
    desirable_angle_rad = np.radians(desirable_angle)

    reference_transform = get_tsai_hill_transform(
        reference_angle_rad,
        reference_stress_ratio,
        tensile_axial_strength,
        compressive_axial_strength,
        tensile_transverse_strength,
        compressive_transverse_strength,
        shear_strength,
    )
    target_transform = get_tsai_hill_transform(
        desirable_angle_rad,
        target_stress_ratio,
        tensile_axial_strength,
        compressive_axial_strength,
        tensile_transverse_strength,
        compressive_transverse_strength,
        shear_strength,
    )

    agg_df["equivalent_stress"] = get_equivalent_stress(
        agg_df.stress_max, reference_stress_ratio, reference_transform
    )
    agg_df["log_equivalent_stress"] = np.log10(agg_df.equivalent_stress)
    agg_df["log_cycles_to_failure"] = np.log10(agg_df.cycles_to_failure)

    faf_csv_df = pd.DataFrame()
    faf_json_records = []

    for reference_group_stress_ratio, group_df in agg_df.groupby("stress_ratio"):
        regression = stats.linregress(
            group_df.log_equivalent_stress, group_df.log_cycles_to_failure
        )
        aa = 10 ** (-regression.intercept / regression.slope)
        bb = -1 / regression.slope

        curve_df = pd.DataFrame(LIST_CYCLES_TO_FAILURE, columns=["cycles_to_failure"])
        curve_df["stress_ratio"] = reference_group_stress_ratio
        curve_df["stress_max"] = get_stress_max(
            aa,
            bb,
            curve_df.cycles_to_failure,
            target_stress_ratio,
            target_transform,
        )
        faf_csv_df = pd.concat([faf_csv_df, curve_df])

        faf_json_records.append(
            {
                "stress_ratio": reference_group_stress_ratio,
                "target_stress_ratio": target_stress_ratio,
                "aa": aa,
                "bb": bb,
            }
        )

    faf_json_df = pd.DataFrame(faf_json_records)

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[["stress_ratio", "cycles_to_failure", "stress_max"]].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

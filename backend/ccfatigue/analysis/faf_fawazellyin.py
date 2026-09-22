#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf_fawazellyin.py
This code takes in a reference SNC curve and outputs FAF data

Fawaz-Ellyin rescales a single reference S-N curve (fitted at a reference
angle/stress ratio, with a known static strength) to a target angle/stress
ratio, using the ratio of static strengths (f) and a stress-ratio correction
factor (g).
Translated from a Fortran source (program EWE) provided by the user; no
public reference for this file was found in the current CCFatiguePlatform
repository.
"""

from itertools import chain
from math import log10
from typing import Optional

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.faf as faf

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(2000000, 20000001, 1000000),
    )
)


def get_static_strength_transform(
    sn_model: faf.FatigueModel,
    reference_static_strength: float,
    target_static_strength: float,
) -> float:
    """
    Static-strength transform factor (f in the original Fortran).
    Parameters
    ----------
        sn_model: faf.FatigueModel
            S-N curve used model [Lin-Log|Log-Log]
        reference_static_strength: float
            Static strength at the reference angle [MPa]
        target_static_strength: float
            Static strength at the target (desirable) angle [MPa]
    Returns
    -------
        transform: float
    """
    if sn_model == faf.FatigueModel.LIN_LOG:
        transform = target_static_strength / reference_static_strength
    else:
        transform = log10(target_static_strength) / log10(reference_static_strength)
    return transform


def get_stress_ratio_transform(
    reference_stress_ratio: float, target_stress_ratio: float
) -> float:
    """
    Stress-ratio correction factor (g in the original Fortran).
    Parameters
    ----------
        reference_stress_ratio: float
            Stress ratio (R) of the reference S-N curve [-]
        target_stress_ratio: float
            Target/design stress ratio (R) [-]
    Returns
    -------
        transform: float
    """
    if target_stress_ratio == reference_stress_ratio or target_stress_ratio <= 0:
        transform = 1
    elif target_stress_ratio == 1:
        transform = 0
    else:
        transform = abs((1 - target_stress_ratio) / (1 - reference_stress_ratio))
    return transform


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


def execute(
    snc_input_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    sn_model: faf.FatigueModel,
    reference_angle: float,
    reference_static_strength: float,
    desirable_angle: float,
    target_stress_ratio: float,
    target_static_strength: float,
) -> None:
    """
    Execute the Fawaz-Ellyin algorithm.

    The reference data (snc_input_json_file) is a single S-N curve (SNC
    json), fitted at a reference angle/stress ratio. It is rescaled to the
    target (desirable_angle, target_stress_ratio) using the ratio of static
    strengths at the reference and target angles.
    Parameters
    ----------
        snc_input_json_file: FilePath | ReadCsvBuffer
            Reference S-N curve (SNC json)
        faf_output_csv_file: FilePath | WriteBuffer
            output (FAF CSV)
        faf_output_json_file: FilePath | WriteBuffer
            output (FAF json)
        sn_model: faf.FatigueModel
            S-N curve used model [Lin-Log|Log-Log]
        reference_angle: float
            Off-axis angle of the reference (tested) data [degrees]
        reference_static_strength: float
            Static strength at the reference angle [MPa]
        desirable_angle: float
            Target/design angle [degrees]
        target_stress_ratio: float
            Target/design stress ratio (R) [-]
        target_static_strength: float
            Static strength at the target (desirable) angle [MPa]
    Returns
    -------
        None
    """
    snc_df = pd.read_json(snc_input_json_file, orient="records")

    reference_stress_ratio: float = snc_df.iloc[0].stress_ratio
    a_ref: float = snc_df.iloc[0].a
    b_ref: float = snc_df.iloc[0].b

    strength_transform = get_static_strength_transform(
        sn_model, reference_static_strength, target_static_strength
    )
    stress_ratio_transform = get_stress_ratio_transform(
        reference_stress_ratio, target_stress_ratio
    )

    if sn_model == faf.FatigueModel.LIN_LOG:
        a = strength_transform * a_ref
        get_stress = get_linlog_stress
    else:
        a = 10 ** (strength_transform * log10(a_ref))
        get_stress = get_loglog_stress
    b = strength_transform * stress_ratio_transform * b_ref

    faf_csv_df = pd.DataFrame(LIST_CYCLES_TO_FAILURE, columns=["cycles_to_failure"])
    faf_csv_df["stress_ratio"] = target_stress_ratio
    faf_csv_df["stress_max"] = get_stress(a, b, faf_csv_df.cycles_to_failure)

    faf_json_df = pd.DataFrame(
        {
            "stress_ratio": [target_stress_ratio],
            "reference_angle": [reference_angle],
            "reference_stress_ratio": [reference_stress_ratio],
            "desirable_angle": [desirable_angle],
            "a": [a],
            "b": [b],
        }
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[["stress_ratio", "cycles_to_failure", "stress_max"]].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

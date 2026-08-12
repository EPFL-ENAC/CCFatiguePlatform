#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf_simsbrogdon.py
This code takes in SNC and outputs FAF data

Sims-Brogdon combines longitudinal (X), transverse (Y) and shear/off-axis
(F) S-N curves into a single predicted S-N curve at a desirable angle,
using the same overall structure as FTPF (see faf_ftpf.py) but a different
combination formula (the mn cross term uses 1/X**2 instead of 1/(X*Y)).
Translated from a Fortran source (program SB) provided by the user; no
public reference for this file was found in the current CCFatiguePlatform
repository.
"""

from itertools import chain
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


def get_sn(x: float, y: float, nc: float, m: float, mn: float, s: float) -> float:
    """
    Combine the X/Y/S component stresses into a predicted stress at the
    desirable angle.
    Parameters
    ----------
        x: float
        y: float
        nc: float
        m: float
        mn: float
        s: float
    Returns
    -------
        sn: float
    """
    sn = ((nc / x**2) + (m / y**2) + (mn * ((1 / s**2) - (1 / x**2)))) ** (-0.5)
    return sn


def get_ssqr(x: float, y: float, tnc: float, tm: float, tmn: float, t: float) -> float:
    """
    Parameters
    ----------
        x: float
        y: float
        tnc: float
        tm: float
        tmn: float
        t: float
    Returns
    -------
        ssqr: float
    """
    ssqr = -((tnc / x**2) + (tm / y**2) - (tmn / x**2) - (1 / t**2)) / tmn
    return ssqr


def execute(
    snc_input_x_json_file: FilePath | ReadCsvBuffer,
    snc_input_y_json_file: FilePath | ReadCsvBuffer,
    snc_input_f_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    sn_model: faf.FatigueModel,
    desirable_angle: float,
    off_axis_angle: float,
) -> None:
    """
    Execute the Sims-Brogdon algorithm
    Parameters
    ----------
        snc_input_x_json_file: FilePath | ReadCsvBuffer
            X input (SNC json)
        snc_input_y_json_file: FilePath | ReadCsvBuffer
            Y input (SNC json)
        snc_input_f_json_file: FilePath | ReadCsvBuffer
            F input (SNC json)
        faf_output_csv_file: FilePath | WriteBuffer
            output (FAF CSV)
        faf_output_json_file: FilePath | WriteBuffer
            output (FAF json)
        sn_model: faf.FatigueModel
            S-N curve used model [Lin-Log|Log-Log]
        desirable_angle: float
            Desirable angle [degrees]
        off_axis_angle: float
            Off-axis angle of the F input [degrees]
    Returns
    -------
        None
    """
    # Import input files (SNC format)
    snc_x_df = pd.read_json(snc_input_x_json_file, orient="records")
    snc_y_df = pd.read_json(snc_input_y_json_file, orient="records")
    snc_f_df = pd.read_json(snc_input_f_json_file, orient="records")

    r_x: float = snc_x_df.iloc[0].stress_ratio
    r_y: float = snc_y_df.iloc[0].stress_ratio
    r_f: float = snc_f_df.iloc[0].stress_ratio

    a_x: float = snc_x_df.iloc[0].a
    a_y: float = snc_y_df.iloc[0].a
    a_f: float = snc_f_df.iloc[0].a

    b_x: float = snc_x_df.iloc[0].b
    b_y: float = snc_y_df.iloc[0].b
    b_f: float = snc_f_df.iloc[0].b

    theta = np.radians(desirable_angle)
    off_axis_rad = np.radians(off_axis_angle)

    tm = np.sin(off_axis_rad) ** 4
    tnc = np.cos(off_axis_rad) ** 4
    tmn = np.sin(off_axis_rad) ** 2 * np.cos(off_axis_rad) ** 2

    nc = np.cos(theta) ** 4
    m = np.sin(theta) ** 4
    mn = np.sin(theta) ** 2 * np.cos(theta) ** 2

    stress_ratio = float(r_x) if np.isclose((r_x + r_y + r_f) / 3, r_x) else 1
    confidence_interval = (
        float(snc_x_df.iloc[0].confidence_interval)
        if np.isclose(
            (
                snc_x_df.iloc[0].confidence_interval
                + snc_y_df.iloc[0].confidence_interval
                + snc_f_df.iloc[0].confidence_interval
            )
            / 3,
            snc_x_df.iloc[0].confidence_interval,
        )
        else 0
    )

    faf_json_df = pd.DataFrame(
        {"stress_ratio": [stress_ratio], "confidence_interval": [confidence_interval]}
    )

    faf_csv_df = pd.DataFrame(
        LIST_CYCLES_TO_FAILURE,
        columns=["cycles_to_failure"],
    )

    faf_csv_df["stress_ratio"] = stress_ratio

    if sn_model == faf.FatigueModel.LOG_LOG:
        get_stress = get_loglog_stress
    else:
        get_stress = get_linlog_stress

    faf_csv_df["x"] = get_stress(a_x, b_x, faf_csv_df.cycles_to_failure)
    faf_csv_df["y"] = get_stress(a_y, b_y, faf_csv_df.cycles_to_failure)

    if not np.isclose(off_axis_angle, 0):

        faf_csv_df["t"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)

        faf_csv_df["ssqr"] = faf_csv_df.apply(
            lambda z: get_ssqr(z.x, z.y, tnc, tm, tmn, z.t), axis=1
        )

        # Remove rows where ssqr < 0
        faf_csv_df.drop(faf_csv_df[faf_csv_df.ssqr < 0].index, inplace=True)
        faf_csv_df["s"] = 1 / np.sqrt(faf_csv_df.ssqr)

    else:
        faf_csv_df["s"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)

    if np.isclose(off_axis_angle, 22.5):
        faf_csv_df["s"] = faf_csv_df["t"] / 2.2

    faf_csv_df["stress_max"] = faf_csv_df.apply(
        lambda z: get_sn(z.x, z.y, nc, m, mn, z.s), axis=1
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[["stress_ratio", "cycles_to_failure", "stress_max", "s"]].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

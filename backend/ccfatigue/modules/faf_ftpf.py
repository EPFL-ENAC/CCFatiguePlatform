#!/usr/bin/env python
"""
CCFatigue - Module 4 - Fatigue Failure - faf-ftpf.py
This code takes in SNC and outputs FAF data

FTPF is described in Tassos red book, in p. 163 - the original papers are
[1] https://www.sciencedirect.com/science/article/pii/S0142112398000735?via%3Dihub
    (https://doi.org/10.1016/S0142-1123(98)00073-5)
[2] https://www.sciencedirect.com/science/article/pii/S014211230200004X?via%3Dihub
    (https://doi.org/10.1016/S0142-1123(02)00004-X)
"""

import math
from itertools import chain

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


def get_loglog_stress(a: float, b: float, cycles_to_failure) -> float:
    """
    Get stress according to a log log slope
    Parameters
    ----------
        a: float
            slope
        b: float
            intercept
        cycles_to_failure: float
            Number of cycles to failure (N) [-]
    Returns
    -------
        stress: float
    """
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L97
    stress = a * cycles_to_failure**-b
    return stress


def get_linlog_stress(a: float, b: float, cycles_to_failure) -> float:
    """
    Get stress according to a lin log slope
    Parameters
    ----------
        a: float
            slope
        b: float
            intercept
        cycles_to_failure: float
            Number of cycles to failure (N) [-]
    Returns
    -------
        stress: float
    """
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L183
    stress = a + b * cycles_to_failure
    return stress


def get_loglog_sn(
    x: float,
    y: float,
    nc: float,
    m: float,
    mn: float,
    s: float,
) -> float:
    """
    Get sn according to a log log slope
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
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L113
    sn = ((nc / x**2) + (m / y**2) + (mn * ((1 / s**2) - (1 / (x * y))))) ** (
        -0.5
    )
    return sn


def get_linlog_sn(
    x: float,
    y: float,
    nc: float,
    m: float,
    mn: float,
    s: float,
) -> float:
    """
    Get sn according to a log log slope
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
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L199
    sn = ((nc / x**2) + (m / y**2) + (mn * ((1 / s**2) - (1 / (x * y))))) ** (
        -0.5
    )
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
    """
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L101
    ssqr = -((tnc / x**2) + (tm / y**2) - (tmn / (x * y)) - (1 / t**2)) / tmn
    return ssqr


def execute(
    snc_input_x_json_file: FilePath | ReadCsvBuffer,
    snc_input_y_json_file: FilePath | ReadCsvBuffer,
    snc_input_f_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: FilePath | WriteBuffer,
    sn_model: str,
    desirable_angle: float,
    off_axis_angle: float,
) -> None:
    """
    Execute the CLD Harris algorithm
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
        sn_model: str
            S-N curve used model [Lin-Log|Log-Log]
        desirable_angle: float
            Desirable angle [degrees]
        off_axis_angle: float
            Off-axis angle [degrees]
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

    theta = math.radians(desirable_angle)
    off_axis_rad = math.radians(off_axis_angle)

    tm = math.sin(off_axis_rad) ** 4
    tnc = math.cos(off_axis_rad) ** 4
    tmn = math.sin(off_axis_rad) ** 2 * math.cos(off_axis_rad) ** 2

    nc = math.cos(theta) ** 4
    m = math.sin(theta) ** 4
    mn = math.sin(theta) ** 2 * math.cos(theta) ** 2

    stress_ratio = r_x if r_x == r_y == r_f else 0
    confidence_interval: float = (
        snc_x_df.iloc[0].confidence_interval
        if snc_x_df.iloc[0].confidence_interval
        == snc_y_df.iloc[0].confidence_interval
        == snc_f_df.iloc[0].confidence_interval
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

    if sn_model == "Log-Log":
        get_stress = get_loglog_stress
        get_sn = get_loglog_sn
    else:
        get_stress = get_linlog_stress
        get_sn = get_linlog_sn

    faf_csv_df["x"] = get_stress(a_x, b_x, faf_csv_df.cycles_to_failure)
    faf_csv_df["y"] = get_stress(a_y, b_y, faf_csv_df.cycles_to_failure)

    if not math.isclose(off_axis_angle, 0):

        faf_csv_df["t"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)

        faf_csv_df["ssqr"] = faf_csv_df.apply(
            lambda z: get_ssqr(z.x, z.y, tnc, tm, tmn, z.t), axis=1
        )

        # Remove rows where ssqr < 0
        faf_csv_df.drop(faf_csv_df[faf_csv_df.ssqr < 0].index, inplace=True)
        # output_df["s"] = output_df.apply(lambda z: 1 / math.sqrt(z.ssqr), axis=1)
        faf_csv_df["s"] = 1 / np.sqrt(faf_csv_df.ssqr)

    else:
        faf_csv_df["s"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)

    if math.isclose(off_axis_angle, 22.5):
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

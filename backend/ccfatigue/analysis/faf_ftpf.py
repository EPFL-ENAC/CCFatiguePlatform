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
    stress = a + b * np.log10(cycles_to_failure)
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


def get_f66_from_offaxis(
    f11: float,
    f22: float,
    f12: float,
    f1: float,
    f2: float,
    t: float,
    tnc: float,
    tm: float,
    tmn: float,
    t_cos2: float,
    t_sin2: float,
) -> float:
    """
    Back-calculate F66 (1/S**2) from a single off-axis S-N curve t(N), by
    solving the full Tsai-Wu quadratic (with the F1/F2 linear terms, valid
    for X' != X and Y' != Y) at the off-axis angle instead of the
    F1=F2=0 simplification used by get_ssqr().
    Parameters
    ----------
        f11: float
        f22: float
        f12: float
        f1: float
        f2: float
        t: float
            Off-axis stress to failure at this N
        tnc: float
            cos(off_axis_angle)**4
        tm: float
            sin(off_axis_angle)**4
        tmn: float
            sin(off_axis_angle)**2 * cos(off_axis_angle)**2
        t_cos2: float
            cos(off_axis_angle)**2
        t_sin2: float
            sin(off_axis_angle)**2
    Returns
    -------
        f66: float
    """
    a_partial = f11 * tnc + f22 * tm + 2 * f12 * tmn
    b_off = f1 * t_cos2 + f2 * t_sin2
    a_total = (1 - b_off * t) / t**2
    return (a_total - a_partial) / tmn


def get_ftpf_stress_max(
    f11: float,
    f22: float,
    f66: float,
    f12: float,
    f1: float,
    f2: float,
    nc: float,
    m: float,
    mn: float,
    cos2: float,
    sin2: float,
) -> float:
    """
    Predicted stress to failure at the desirable angle, solving the full
    (dissymmetric) Tsai-Wu quadratic A*stress**2 + B*stress - 1 = 0 -
    replaces get_loglog_sn()/get_linlog_sn(), which assume F1=F2=0
    (X' = X, Y' = Y).
    Parameters
    ----------
        f11: float
        f22: float
        f66: float
        f12: float
        f1: float
        f2: float
        nc: float
            cos(desirable_angle)**4
        m: float
            sin(desirable_angle)**4
        mn: float
            sin(desirable_angle)**2 * cos(desirable_angle)**2
        cos2: float
            cos(desirable_angle)**2
        sin2: float
            sin(desirable_angle)**2
    Returns
    -------
        stress_max: float
    """
    a = f11 * nc + f22 * m + mn * (2 * f12 + f66)
    b = f1 * cos2 + f2 * sin2
    return (-b + np.sqrt(b**2 + 4 * a)) / (2 * a)


def get_compression_curve(
    get_stress,
    cycles_to_failure,
    curve_file: Optional[FilePath | ReadCsvBuffer],
):
    """
    Evaluate the compression-side fatigue curve for the Tsai-Wu/FTPF tensor
    coefficients (F11, F22 need X', Y' - the compression-side counterparts
    of the tension curves X(N), Y(N)).

    Unlike shear (which has a documented back-calculation from an off-axis
    curve), the book (Tassos red book, §6.2.4, Eq. 6.10-6.18) has no
    approximation method for X'/Y' - a compression SNC curve is required.
    Parameters
    ----------
        get_stress: Callable
            get_loglog_stress or get_linlog_stress, matching sn_model
        cycles_to_failure: pd.Series
        curve_file: Optional[FilePath | ReadCsvBuffer]
            Compression SNC curve file
    Returns
    -------
        compression_stress: pd.Series
    """
    if curve_file is None:
        raise ValueError(
            "FTPF needs a compression SNC curve file for each direction "
            "(longitudinal and transverse) - Xc and Yc are required."
        )

    curve_df = pd.read_json(curve_file, orient="records")
    a_c = curve_df.iloc[0].a
    b_c = curve_df.iloc[0].b
    return get_stress(a_c, b_c, cycles_to_failure)


def execute(
    snc_input_x_json_file: FilePath | ReadCsvBuffer,
    snc_input_y_json_file: FilePath | ReadCsvBuffer,
    snc_input_f_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    sn_model: faf.FatigueModel,
    desirable_angle: float,
    off_axis_angle: float,
    snc_input_xc_json_file: Optional[FilePath | ReadCsvBuffer] = None,
    snc_input_yc_json_file: Optional[FilePath | ReadCsvBuffer] = None,
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
        sn_model: SnModel
            S-N curve used model [Lin-Log|Log-Log]
        desirable_angle: float
            Desirable angle [degrees]
        off_axis_angle: float
            Off-axis angle [degrees]
        snc_input_xc_json_file: Optional[FilePath | ReadCsvBuffer]
            Longitudinal compression SNC curve (X'(N)) - required
        snc_input_yc_json_file: Optional[FilePath | ReadCsvBuffer]
            Transverse compression SNC curve (Y'(N)) - required
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
    t_cos2 = np.cos(off_axis_rad) ** 2
    t_sin2 = np.sin(off_axis_rad) ** 2

    nc = np.cos(theta) ** 4
    m = np.sin(theta) ** 4
    mn = np.sin(theta) ** 2 * np.cos(theta) ** 2
    cos2 = np.cos(theta) ** 2
    sin2 = np.sin(theta) ** 2

    stress_ratio = r_x if r_x == r_y == r_f else 1
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

    if sn_model == faf.FatigueModel.LOG_LOG:
        get_stress = get_loglog_stress
    else:
        get_stress = get_linlog_stress

    faf_csv_df["x"] = get_stress(a_x, b_x, faf_csv_df.cycles_to_failure)
    faf_csv_df["y"] = get_stress(a_y, b_y, faf_csv_df.cycles_to_failure)

    # Tsai-Wu/FTPF tensor coefficients (F11, F22, F1, F2, F12) - computed
    # before stress_max/F66 below, since the dissymmetric (X' != X, Y' !=
    # Y) form of stress_max and of the off-axis F66 back-calculation both
    # need them.
    faf_csv_df["xc"] = get_compression_curve(
        get_stress,
        faf_csv_df.cycles_to_failure,
        snc_input_xc_json_file,
    )
    faf_csv_df["yc"] = get_compression_curve(
        get_stress,
        faf_csv_df.cycles_to_failure,
        snc_input_yc_json_file,
    )
    faf_csv_df["f11"] = 1 / (faf_csv_df["x"] * faf_csv_df["xc"])
    faf_csv_df["f22"] = 1 / (faf_csv_df["y"] * faf_csv_df["yc"])
    faf_csv_df["f1"] = 1 / faf_csv_df["x"] - 1 / faf_csv_df["xc"]
    faf_csv_df["f2"] = 1 / faf_csv_df["y"] - 1 / faf_csv_df["yc"]
    faf_csv_df["f12"] = -0.5 * np.sqrt(faf_csv_df["f11"] * faf_csv_df["f22"])

    if not np.isclose(off_axis_angle, 0):

        faf_csv_df["t"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)

        faf_csv_df["f66"] = faf_csv_df.apply(
            lambda z: get_f66_from_offaxis(
                z.f11, z.f22, z.f12, z.f1, z.f2, z.t, tnc, tm, tmn, t_cos2, t_sin2
            ),
            axis=1,
        )

        # Remove rows with no real solution for s (f66 <= 0)
        faf_csv_df.drop(faf_csv_df[faf_csv_df.f66 <= 0].index, inplace=True)
        faf_csv_df["s"] = 1 / np.sqrt(faf_csv_df.f66)

    else:
        faf_csv_df["s"] = get_stress(a_f, b_f, faf_csv_df.cycles_to_failure)
        faf_csv_df["f66"] = 1 / (faf_csv_df["s"] ** 2)

    faf_csv_df["stress_max"] = faf_csv_df.apply(
        lambda z: get_ftpf_stress_max(
            z.f11, z.f22, z.f66, z.f12, z.f1, z.f2, nc, m, mn, cos2, sin2
        ),
        axis=1,
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[
        [
            "stress_ratio",
            "cycles_to_failure",
            "stress_max",
            "x",
            "y",
            "s",
            "xc",
            "yc",
            "f11",
            "f22",
            "f66",
            "f1",
            "f2",
            "f12",
        ]
    ].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

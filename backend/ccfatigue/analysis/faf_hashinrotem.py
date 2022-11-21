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


def get_loglog_sn_case1(
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_s: float,
    b_s: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    theta: float,
) -> float:
    """
    Parameters
    ----------
        cycles_to_failure: int
        a_t: float
        b_t: float
        a_s: float
        b_s: float
        transverse_strength: float
        shear_strength: float
        strength_at_desirable_angle: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L120

    sigmat = a_t * cycles_to_failure ** (-b_t)
    Sigmas = a_s * cycles_to_failure ** (-b_s)

    ft = sigmat / transverse_strength
    fs = Sigmas / shear_strength
    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_loglog_sn_case2(
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_2: float,
    b_2: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    strength2: float,
    oangle2: float,
    theta: float,
) -> float:
    """
    Parameters
    ----------
        cycles_to_failure: int,
        a_t: float,
        b_t: float,
        a_2: float,
        b_2: float,
        transverse_strength: float,
        shear_strength: float,
        strength_at_desirable_angle: float,
        strength2: float,
        oangle2: float,
        theta: float,
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L140

    sigmat = a_t * cycles_to_failure ** (-b_t)

    ft = sigmat / transverse_strength
    const1 = ((shear_strength / transverse_strength) * np.tan(oangle2)) ** 2
    ff = ((a_2 * cycles_to_failure ** (-b_2)) / strength2) ** 2
    fs = np.sqrt((ff * ft**2) / ((1 + const1) * ft**2 - (ff * const1)))
    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_loglog_sn_case3(
    cycles_to_failure: int,
    a_1: float,
    b_1: float,
    a_2: float,
    b_2: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    strength1: float,
    strength2: float,
    oangle1: float,
    oangle2: float,
    theta: float,
) -> Optional[float]:
    """
    Parameters
    ----------
        cycles_to_failure: int,
        a_1: float,
        b_1: float,
        a_2: float,
        b_2: float,
        transverse_strength: float,
        shear_strength: float,
        strength_at_desirable_angle: float,
        strength1: float,
        strength2: float,
        oangle1: float,
        oangle2: float,
        theta: float,
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L180

    const1 = ((shear_strength / transverse_strength) * np.tan(oangle1)) ** 2
    const2 = ((shear_strength / transverse_strength) * np.tan(oangle2)) ** 2

    ff1 = ((a_1 * cycles_to_failure ** (-b_1)) / strength1) ** 2
    ff2 = ((a_2 * cycles_to_failure ** (-b_2)) / strength2) ** 2

    ft2 = (ff1 * ff2 * (const2 - const1)) / (ff1 * (1 + const2) - ff2 * (1 + const1))
    fs2 = (-ff1 * ft2) / (ff1 * const1 - ft2 * (1 + const1))
    if ft2 < 0:
        # go to 10 ! Ignore the current line
        return None
    else:

        ft = np.sqrt(ft2)
        fs = np.sqrt(fs2)

    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_loglog_sn(
    off_axis_angle1: float,
    off_axis_angle2: float,
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_s: float,
    b_s: float,
    a_1: float,
    b_1: float,
    a_2: float,
    b_2: float,
    tensile_transverse_strength: float,
    shear_strength: float,
    tensile_strength_at_desirable_angle: float,
    compressive_transverse_strength: float,
    compressive_strength_at_desirable_angle: float,
    tensile_strength1: float,
    tensile_strength2: float,
    compressive_strength1: float,
    compressive_strength2: float,
    oangle1: float,
    oangle2: float,
    stress_ratio: float,
    theta: float,
) -> float | None:
    """
    Get max stress, loglog version.
    Parameters
    ----------
        off_axis_angle1: float
        off_axis_angle2: float
        cycles_to_failure: int
        a_t: float
        b_t: float
        a_s: float
        b_s: float
        a_1: float
        b_1: float
        a_2: float
        b_2: float
        tensile_transverse_strength: float
        shear_strength: float
        tensile_strength_at_desirable_angle: float
        compressive_transverse_strength: float
        compressive_strength_at_desirable_angle: float
        tensile_strength1: float
        tensile_strength2: float
        compressive_strength1: float
        compressive_strength2: float
        oangle1: float
        oangle2: float
        stress_ratio: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    if stress_ratio >= 0 and stress_ratio < 1:
        transverse_strength = tensile_transverse_strength
        strength1 = tensile_strength1
        strength2 = tensile_strength2
        strength_at_desirable_angle = tensile_strength_at_desirable_angle

    elif stress_ratio > 1:
        transverse_strength = compressive_transverse_strength
        strength1 = compressive_strength1
        strength2 = compressive_strength2
        strength_at_desirable_angle = compressive_strength_at_desirable_angle

    else:  # stress_ratio == 1 or stress_ratio < 0
        raise NotImplementedError("stress_ratio = " + str(stress_ratio))

    if off_axis_angle1 == 0 and off_axis_angle2 == 0:
        stress_max = get_loglog_sn_case1(
            cycles_to_failure=cycles_to_failure,
            a_t=a_t,
            b_t=b_t,
            a_s=a_s,
            b_s=b_s,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            theta=theta,
        )

    elif off_axis_angle1 == 90:
        stress_max = get_loglog_sn_case2(
            cycles_to_failure=cycles_to_failure,
            a_t=a_t,
            b_t=b_t,
            a_2=a_2,
            b_2=b_2,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            strength2=strength2,
            oangle2=oangle2,
            theta=theta,
        )

    elif off_axis_angle1 != 0 and off_axis_angle2 != 0 and off_axis_angle1 != 90:

        stress_max = get_loglog_sn_case3(
            cycles_to_failure=cycles_to_failure,
            a_1=a_1,
            b_1=b_1,
            a_2=a_2,
            b_2=b_2,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            strength1=strength1,
            strength2=strength2,
            oangle1=oangle1,
            oangle2=oangle2,
            theta=theta,
        )

    else:  # Not implemented in Tassos algo
        # TODO for instance off_axis_angle1=0 and off_axis_angle2=90
        raise NotImplementedError(f"{off_axis_angle1=}, {off_axis_angle2=}")

    return stress_max


def get_linlog_sn_case1(
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_s: float,
    b_s: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    theta: float,
) -> float:
    """
    Parameters
    ----------
        cycles_to_failure: int
        a_t: float
        b_t: float
        a_s: float
        b_s: float
        transverse_strength: float
        shear_strength: float
        strength_at_desirable_angle: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L449

    sigmat = a_t + b_t * np.log10(cycles_to_failure)
    sigmas = a_s + b_s * np.log10(cycles_to_failure)

    ft = sigmat / transverse_strength
    fs = sigmas / shear_strength
    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_linlog_sn_case2(
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_2: float,
    b_2: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    strength2: float,
    oangle2: float,
    theta: float,
) -> float:
    """
    Parameters
    ----------
        cycles_to_failure: int
        a_t: float
        b_t: float
        a_2: float
        b_2: float
        transverse_strength: float
        shear_strength: float
        strength_at_desirable_angle: float
        strength2: float
        oangle2: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L474

    sigmat = a_t + b_t * np.log10(cycles_to_failure)

    ft = sigmat / transverse_strength
    const1 = ((shear_strength / transverse_strength) * np.tan(oangle2)) ** 2
    ff = ((a_2 + b_2 * np.log10(cycles_to_failure)) / strength2) ** 2
    fs = np.sqrt((ff * ft**2) / ((1 + const1) * ft**2 - (ff * const1)))
    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_linlog_sn_case3(
    cycles_to_failure: int,
    a_1: float,
    b_1: float,
    a_2: float,
    b_2: float,
    transverse_strength: float,
    shear_strength: float,
    strength_at_desirable_angle: float,
    strength1: float,
    strength2: float,
    oangle1: float,
    oangle2: float,
    theta: float,
) -> float | None:
    """
    Parameters
    ----------
        cycles_to_failure: int
        a_1: float
        b_1: float
        a_2: float
        b_2: float
        transverse_strength: float
        shear_strength: float
        strength_at_desirable_angle: float
        strength1: float
        strength2: float
        oangle1: float
        oangle2: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L514

    const1 = ((shear_strength / transverse_strength) * np.tan(oangle1)) ** 2
    const2 = ((shear_strength / transverse_strength) * np.tan(oangle2)) ** 2

    ff1 = ((a_1 + b_1 * np.log10(cycles_to_failure)) / strength1) ** 2
    ff2 = ((a_2 + b_2 * np.log10(cycles_to_failure)) / strength2) ** 2

    ft2 = (ff1 * ff2 * (const2 - const1)) / (ff1 * (1 + const2) - ff2 * (1 + const1))
    fs2 = (-ff1 * ft2) / (ff1 * const1 - ft2 * (1 + const1))

    if ft2 <= 0:
        # go to 50
        return None

    else:
        ft = np.sqrt(ft2)
        fs = np.sqrt(fs2)

    stress_max = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_max


def get_linlog_sn(
    off_axis_angle1: float,
    off_axis_angle2: float,
    cycles_to_failure: int,
    a_t: float,
    b_t: float,
    a_s: float,
    b_s: float,
    a_1: float,
    b_1: float,
    a_2: float,
    b_2: float,
    tensile_transverse_strength: float,
    shear_strength: float,
    tensile_strength_at_desirable_angle: float,
    compressive_transverse_strength: float,
    compressive_strength_at_desirable_angle: float,
    tensile_strength1: float,
    tensile_strength2: float,
    compressive_strength1: float,
    compressive_strength2: float,
    oangle1: float,
    oangle2: float,
    stress_ratio: float,
    theta: float,
) -> float | None:
    """
    Get max stress, lin-log version.
    Parameters
    ----------
        off_axis_angle1: float
        off_axis_angle2: float
        cycles_to_failure: int
        a_t: float
        b_t: float
        a_s: float
        b_s: float
        a_1: float
        b_1: float
        a_2: float
        b_2: float
        tensile_transverse_strength: float
        shear_strength: float
        tensile_strength_at_desirable_angle: float
        compressive_transverse_strength: float
        compressive_strength_at_desirable_angle: float
        tensile_strength1: float
        tensile_strength2: float
        compressive_strength1: float
        compressive_strength2: float
        oangle1: float
        oangle2: float
        stress_ratio: float
        theta: float
    Returns
    -------
        stress_max
            Max stress (sigma_max) [MPa]
    """

    if stress_ratio >= 0 and stress_ratio < 1:
        transverse_strength = tensile_transverse_strength
        strength1 = tensile_strength1
        strength2 = tensile_strength2
        strength_at_desirable_angle = tensile_strength_at_desirable_angle

    elif stress_ratio > 1 or stress_ratio == -1:
        transverse_strength = compressive_transverse_strength
        strength1 = compressive_strength1
        strength2 = compressive_strength2
        strength_at_desirable_angle = compressive_strength_at_desirable_angle

    else:  # stress_ratio == 1 or stress_ratio < 0
        raise NotImplementedError("stress_ratio = " + str(stress_ratio))

    if off_axis_angle1 == 0 and off_axis_angle2 == 0:

        stress_max = get_linlog_sn_case1(
            cycles_to_failure=cycles_to_failure,
            a_t=a_t,
            b_t=b_t,
            a_s=a_s,
            b_s=b_s,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            theta=theta,
        )

    elif off_axis_angle1 == 90:

        stress_max = get_linlog_sn_case2(
            cycles_to_failure=cycles_to_failure,
            a_t=a_t,
            b_t=b_t,
            a_2=a_2,
            b_2=b_2,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            strength2=strength2,
            oangle2=oangle2,
            theta=theta,
        )

    elif off_axis_angle1 != 0 and off_axis_angle2 != 0 and off_axis_angle1 != 90:

        stress_max = get_linlog_sn_case3(
            cycles_to_failure=cycles_to_failure,
            a_1=a_1,
            b_1=b_1,
            a_2=a_2,
            b_2=b_2,
            transverse_strength=transverse_strength,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            strength1=strength1,
            strength2=strength2,
            oangle1=oangle1,
            oangle2=oangle2,
            theta=theta,
        )

    else:  # Not implemented in Tassos algo
        # TODO for instance off_axis_angle1=0 and off_axis_angle2=90
        raise NotImplementedError(f"{off_axis_angle1=}, {off_axis_angle2=}")

    return stress_max


def execute(
    snc_input_x_json_file: FilePath | ReadCsvBuffer,
    snc_input_y_json_file: FilePath | ReadCsvBuffer,
    snc_input_f_json_file: FilePath | ReadCsvBuffer,
    faf_output_csv_file: FilePath | WriteBuffer,
    faf_output_json_file: Optional[FilePath | WriteBuffer],
    fatigue_model: faf.FatigueModel,
    desirable_angle: float,
    off_axis_angle1: float,
    off_axis_angle2: float,
    tensile_transverse_strength: float,
    compressive_transverse_strength: float,
    shear_strength: float,
    tensile_strength1: float,
    compressive_strength1: float,
    tensile_strength2: float,
    compressive_strength2: float,
    tensile_strength_at_desirable_angle: float,
    compressive_strength_at_desirable_angle: float,
) -> None:
    """
    Execute the CLD Hashin-Rotem algorithm
    Parameters
    ----------
        snc_input_x_json_file: FilePath | ReadCsvBuffer
        snc_input_y_json_file: FilePath | ReadCsvBuffer
        snc_input_f_json_file: FilePath | ReadCsvBuffer
        faf_output_csv_file: FilePath | WriteBuffer
        faf_output_json_file: Optional[FilePath | WriteBuffer]
        fatigue_model: faf.FatigueModel
        desirable_angle: float
        off_axis_angle1: float
        off_axis_angle2: float
        tensile_transverse_strength: float
        compressive_transverse_strength: float
        shear_strength: float
        tensile_strength1: float
        compressive_strength1: float
        tensile_strength2: float
        compressive_strength2: float
        tensile_strength_at_desirable_angle: float
        compressive_strength_at_desirable_angle: float
    Returns
    -------
        None
    """

    # Import input files (SNC format)
    snc_1_df = pd.read_json(snc_input_x_json_file, orient="records")
    snc_2_df = pd.read_json(snc_input_y_json_file, orient="records")
    snc_a_df = pd.read_json(snc_input_f_json_file, orient="records")

    r1 = snc_1_df.iloc[0].stress_ratio
    r2 = snc_2_df.iloc[0].stress_ratio
    ra = snc_a_df.iloc[0].stress_ratio

    at = 0
    bt = 0
    a1 = 0
    b1 = 0
    a2 = 0
    b2 = 0
    a_s = 0
    b_s = 0

    if off_axis_angle1 == 0 and off_axis_angle2 == 0:
        at = snc_1_df.iloc[0].a
        bt = snc_1_df.iloc[0].b
        a_s = snc_2_df.iloc[0].a
        b_s = snc_2_df.iloc[0].b

    if off_axis_angle1 == 90:
        at = snc_1_df.iloc[0].a
        bt = snc_1_df.iloc[0].b
        a2 = snc_2_df.iloc[0].a
        b2 = snc_2_df.iloc[0].b

    if off_axis_angle1 != 0 and off_axis_angle2 != 0 and off_axis_angle1 != 90:
        a1 = snc_1_df.iloc[0].a
        b1 = snc_1_df.iloc[0].b
        a2 = snc_2_df.iloc[0].a
        b2 = snc_2_df.iloc[0].b

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

    # Select approriate SN function to solve equation
    if fatigue_model == "Log-Log":
        get_sn = get_loglog_sn
    else:
        get_sn = get_linlog_sn

    faf_csv_df["stress_max"] = faf_csv_df.apply(
        lambda z: get_sn(
            off_axis_angle1=off_axis_angle1,
            off_axis_angle2=off_axis_angle2,
            cycles_to_failure=z.cycles_to_failure,  # type: ignore
            a_t=at,
            b_t=bt,
            a_s=a_s,
            b_s=b_s,
            a_1=a1,
            b_1=b1,
            a_2=a2,
            b_2=b2,
            tensile_transverse_strength=tensile_transverse_strength,
            shear_strength=shear_strength,
            tensile_strength_at_desirable_angle=tensile_strength_at_desirable_angle,
            compressive_transverse_strength=compressive_transverse_strength,
            compressive_strength_at_desirable_angle=compressive_strength_at_desirable_angle,  # noqa: E501
            tensile_strength1=tensile_strength1,
            tensile_strength2=tensile_strength2,
            compressive_strength1=compressive_strength1,
            compressive_strength2=compressive_strength2,
            oangle1=oangle1,
            oangle2=oangle2,
            stress_ratio=stress_ratio,
            theta=theta,
        ),
        axis=1,
    )

    faf_json_df = pd.DataFrame(
        {
            "stress_ratio": [stress_ratio],
            "confidence_interval": [confidence_interval],
            "a1": a1,
            "b1": b1,
            "a2": a2,
            "b2": b2,
            "off_axis_angle1": oangle1,
            "off_axis_angle2": oangle2,
        }
    )

    # Create output files
    faf_json_df.to_json(faf_output_json_file, orient="records")  # type: ignore
    faf_csv_df[["stress_ratio", "cycles_to_failure", "stress_max"]].to_csv(
        faf_output_csv_file,  # type: ignore
        index=False,
    )

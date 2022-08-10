#!/usr/bin/env python
""" CCFatigue - Module 4 - Fatigue-Failure-Hashin-Rotem.py

H-R is described in the red book, in p. 159 - the original paper is
https://journals.sagepub.com/doi/10.1177/002199837300700404
https://drive.google.com/drive/u/1/folders/1SVank5TEcCxHG5vIgcmxaR9zAov8qj2l?ths=true
"""

import os
import math
import pandas as pd
from itertools import chain

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_1_JSON_FILENAME = "SN1.json"
INPUT_2_JSON_FILENAME = "SN2.json"
INPUT_A_JSON_FILENAME = "SNA.json"
INPUT_REFDATA_FILENAME = "refdata.json"
INPUT_1_JSON_FILE = os.path.join(SRC_DIR, INPUT_1_JSON_FILENAME)
INPUT_2_JSON_FILE = os.path.join(SRC_DIR, INPUT_2_JSON_FILENAME)
INPUT_A_JSON_FILE = os.path.join(SRC_DIR, INPUT_A_JSON_FILENAME)
INPUT_REFDATA_FILE = os.path.join(SRC_DIR, INPUT_REFDATA_FILENAME)

# OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
# OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)


def hashin_equation_23(
    f_tau: float,
    tau_s: float,
    theta: float,
    sigma_s_t: float,
    f_t: float,
):
    """Equation 23, p455
    f_tau =
    tau_s = shear loading alone
    theta =
    sigma_s_t = failure stresses in transverse loading alone
    f_t =
    returns f = fatigue function
    TODO"""
    f = f_tau * math.sqrt(
        (1 + ((tau_s / sigma_s_t) * math.tan(theta)) ** 2)
        / (1 + ((tau_s / sigma_s_t) * (f_tau / f_t) * math.tan(theta)) ** 2)
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
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L120
    """

    sigmat = a_t * cycles_to_failure ** (-b_t)
    Sigmas = a_s * cycles_to_failure ** (-b_s)

    ft = sigmat / transverse_strength
    fs = Sigmas / shear_strength
    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L140
    """

    sigmat = a_t * cycles_to_failure ** (-b_t)

    ft = sigmat / transverse_strength
    const1 = ((shear_strength / transverse_strength) * math.tan(oangle2)) ** 2
    ff = ((a_2 * cycles_to_failure ** (-b_2)) / strength2) ** 2
    fs = math.sqrt((ff * ft**2) / ((1 + const1) * ft**2 - (ff * const1)))
    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L180
    """

    const1 = ((shear_strength / transverse_strength) * math.tan(oangle1)) ** 2
    const2 = ((shear_strength / transverse_strength) * math.tan(oangle2)) ** 2

    ff1 = ((a_1 * cycles_to_failure ** (-b_1)) / strength1) ** 2
    ff2 = ((a_2 * cycles_to_failure ** (-b_2)) / strength2) ** 2

    ft2 = (ff1 * ff2 * (const2 - const1)) / (ff1 * (1 + const2) - ff2 * (1 + const1))
    fs2 = (-ff1 * ft2) / (ff1 * const1 - ft2 * (1 + const1))
    if ft2 < 0:
        # go to 10 ! Ignore the current line
        return None
    else:

        ft = math.sqrt(ft2)
        fs = math.sqrt(fs2)

    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
) -> float:
    """
    Get stress parameter, loglog version.
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
        stress_parameter = get_loglog_sn_case1(
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
        stress_parameter = get_loglog_sn_case2(
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

        stress_parameter = get_loglog_sn_case3(
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

    return stress_parameter


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
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L449
    """

    sigmat = a_t + b_t * math.log10(cycles_to_failure)
    sigmas = a_s + b_s * math.log10(cycles_to_failure)

    ft = sigmat / transverse_strength
    fs = sigmas / shear_strength
    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L474
    """

    sigmat = a_t + b_t * math.log10(cycles_to_failure)

    ft = sigmat / transverse_strength
    const1 = ((shear_strength / transverse_strength) * math.tan(oangle2)) ** 2
    ff = ((a_2 + b_2 * math.log10(cycles_to_failure)) / strength2) ** 2
    fs = math.sqrt((ff * ft**2) / ((1 + const1) * ft**2 - (ff * const1)))
    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/HR/Fatigue-Failure-Hashin-Rotem.for#L514
    """

    const1 = ((shear_strength / transverse_strength) * math.tan(oangle1)) ** 2
    const2 = ((shear_strength / transverse_strength) * math.tan(oangle2)) ** 2

    ff1 = ((a_1 + b_1 * math.log10(cycles_to_failure)) / strength1) ** 2
    ff2 = ((a_2 + b_2 * math.log10(cycles_to_failure)) / strength2) ** 2

    ft2 = (ff1 * ff2 * (const2 - const1)) / (ff1 * (1 + const2) - ff2 * (1 + const1))
    fs2 = (-ff1 * ft2) / (ff1 * const1 - ft2 * (1 + const1))

    if ft2 <= 0:
        # go to 50
        return None

    else:
        ft = math.sqrt(ft2)
        fs = math.sqrt(fs2)

    stress_parameter = strength_at_desirable_angle * hashin_equation_23(
        f_tau=fs,
        tau_s=shear_strength,
        theta=theta,
        sigma_s_t=transverse_strength,
        f_t=ft,
    )

    return stress_parameter


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
) -> float:
    """
    Get stress parameter
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

        stress_parameter = get_linlog_sn_case1(
            cycles_to_failure=cycles_to_failure,
            a_t=a_t,
            b_t=b_t,
            a_s=a_s,
            b_s=b_s,
            shear_strength=shear_strength,
            strength_at_desirable_angle=strength_at_desirable_angle,
            theta=theta,
        )

    elif off_axis_angle1 == 90:

        stress_parameter = get_linlog_sn_case2(
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

        stress_parameter = get_linlog_sn_case3(
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

    return stress_parameter


if __name__ == "__main__":

    # Import param files
    SNC_refdata_df = pd.read_json(INPUT_REFDATA_FILE, orient="index")

    sn_model = SNC_refdata_df.loc["sn_model"][0]
    desirable_angle = SNC_refdata_df.loc["desirable_angle"][0]
    off_axis_angle1 = SNC_refdata_df.loc["off_axis_angle1"][0]
    off_axis_angle2 = SNC_refdata_df.loc["off_axis_angle2"][0]
    tensile_transverse_strength = SNC_refdata_df.loc["tensile_transverse_strength"][0]
    compressive_transverse_strength = SNC_refdata_df.loc[
        "compressive_transverse_strength"
    ][0]
    shear_strength = SNC_refdata_df.loc["shear_strength"][0]
    tensile_strength1 = SNC_refdata_df.loc["tensile_strength1"][0]
    compressive_strength1 = SNC_refdata_df.loc["compressive_strength1"][0]
    tensile_strength2 = SNC_refdata_df.loc["tensile_strength2"][0]
    compressive_strength2 = SNC_refdata_df.loc["compressive_strength2"][0]
    tensile_strength_at_desirable_angle = SNC_refdata_df.loc[
        "tensile_strength_at_desirable_angle"
    ][0]
    compressive_strength_at_desirable_angle = SNC_refdata_df.loc[
        "compressive_strength_at_desirable_angle"
    ][0]

    # Import input files (SNC format)
    SNC_1_df = pd.read_json(INPUT_1_JSON_FILE, orient="records")
    SNC_2_df = pd.read_json(INPUT_2_JSON_FILE, orient="records")
    SNC_a_df = pd.read_json(INPUT_A_JSON_FILE, orient="records")

    r1 = SNC_1_df.iloc[0].stress_ratio
    r2 = SNC_2_df.iloc[0].stress_ratio
    ra = SNC_a_df.iloc[0].stress_ratio

    aa = SNC_a_df.iloc[0].A
    ba = SNC_a_df.iloc[0].B

    at = 0
    bt = 0
    a1 = 0
    b1 = 0
    a2 = 0
    b2 = 0
    a_s = 0
    b_s = 0

    if off_axis_angle1 == 0 and off_axis_angle2 == 0:
        at = SNC_1_df.iloc[0].A
        bt = SNC_1_df.iloc[0].B
        a_s = SNC_2_df.iloc[0].A
        b_s = SNC_2_df.iloc[0].B

    if off_axis_angle1 == 90:
        at = SNC_1_df.iloc[0].A
        bt = SNC_1_df.iloc[0].B
        a2 = SNC_2_df.iloc[0].A
        b2 = SNC_2_df.iloc[0].B

    if off_axis_angle1 != 0 and off_axis_angle2 != 0 and off_axis_angle1 != 90:
        a1 = SNC_1_df.iloc[0].A
        b1 = SNC_1_df.iloc[0].B
        a2 = SNC_2_df.iloc[0].A
        b2 = SNC_2_df.iloc[0].B

    theta = math.radians(desirable_angle)
    oangle1 = math.radians(off_axis_angle1)
    oangle2 = math.radians(off_axis_angle2)

    stress_ratio = (
        float(r1)
        if math.isclose(
            (r1 + r2 + ra) / 3,
            r1,
        )
        else 1
    )

    reliability = (
        float(SNC_1_df.iloc[0].RSQL)
        if math.isclose(
            (SNC_1_df.iloc[0].RSQL + SNC_2_df.iloc[0].RSQL + SNC_a_df.iloc[0].RSQL) / 3,
            SNC_1_df.iloc[0].RSQL,
        )
        else 0
    )

    output_df = pd.DataFrame(
        chain(
            range(1, 1000, 50),
            range(1000, 1001),
            range(10000, 2000000, 10000),
            range(2000000, 20000001, 1000000),
        ),
        columns=["cycles_to_failure"],
    )

    output_df["stress_ratio"] = stress_ratio

    # Select approriate SN function to solve equation
    if sn_model == "Log-Log":
        get_sn = get_loglog_sn
    else:
        get_sn = get_linlog_sn

    output_df["stress_parameter"] = output_df.apply(
        lambda z: get_sn(
            off_axis_angle1=off_axis_angle1,
            off_axis_angle2=off_axis_angle2,
            cycles_to_failure=z.cycles_to_failure,
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
            compressive_strength_at_desirable_angle=compressive_strength_at_desirable_angle,
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

    pass

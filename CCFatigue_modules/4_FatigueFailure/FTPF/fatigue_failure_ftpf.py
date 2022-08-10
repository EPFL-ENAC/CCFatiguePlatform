#!/usr/bin/env python
""" CCFatigue - Module 4 - Fatigue-Failure-FTPF.py

FTPF is described in Tassos red book, in p. 163 - the original papers are 
https://www.sciencedirect.com/science/article/pii/S0142112398000735?via%3Dihub (https://doi.org/10.1016/S0142-1123(98)00073-5)
https://www.sciencedirect.com/science/article/pii/S014211230200004X?via%3Dihub (https://doi.org/10.1016/S0142-1123(02)00004-X)
"""

import os
import math
import pandas as pd
from itertools import chain

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_X_JSON_FILENAME = "SNC_inputX.json"
INPUT_Y_JSON_FILENAME = "SNC_inputY.json"
INPUT_F_JSON_FILENAME = "SNC_inputF.json"
INPUT_REFDATA_FILENAME = "SNC_refdata.json"
INPUT_X_JSON_FILE = os.path.join(DATA_DIR, INPUT_X_JSON_FILENAME)
INPUT_Y_JSON_FILE = os.path.join(DATA_DIR, INPUT_Y_JSON_FILENAME)
INPUT_F_JSON_FILE = os.path.join(DATA_DIR, INPUT_F_JSON_FILENAME)
INPUT_REFDATA_FILE = os.path.join(DATA_DIR, INPUT_REFDATA_FILENAME)

# OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
# OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)


def get_loglog_stress(a: float, b: float, cycles_to_failure: float) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L97
    Inputs:
    - a, b: slope parameter
    - cycles_to_failure
    Outputs:
    - Stress
    """
    return a * cycles_to_failure**-b


def get_linlog_stress(a: float, b: float, cycles_to_failure: float) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L183
    Inputs:
    - a, b: slope parameter
    - cycles_to_failure
    Outputs:
    - Stress
    """
    return a + b * cycles_to_failure


def get_loglog_sn(
    x: float,
    y: float,
    nc: float,
    m: float,
    mn: float,
    s: float,
) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L113
    """
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
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L199
    """
    sn = ((nc / x**2) + (m / y**2) + (mn * ((1 / s**2) - (1 / (x * y))))) ** (
        -0.5
    )
    return sn


def get_ssqr(x: float, y: float, tnc: float, tm: float, tmn: float, t: float) -> float:
    """
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/4_FatigueFailure/FTPF/Fatigue-Failure-FTPF.for#L101
    """
    ssqr = -((tnc / x**2) + (tm / y**2) - (tmn / (x * y)) - (1 / t**2)) / tmn
    return ssqr


if __name__ == "__main__":

    # Import param files
    SNC_refdata_df = pd.read_json(INPUT_REFDATA_FILE, orient="index")

    # Import input files (SNC format)
    SNC_x_df = pd.read_json(INPUT_X_JSON_FILE, orient="records")
    SNC_y_df = pd.read_json(INPUT_Y_JSON_FILE, orient="records")
    SNC_f_df = pd.read_json(INPUT_F_JSON_FILE, orient="records")

    r_x = SNC_x_df.iloc[0].stress_ratio
    r_y = SNC_y_df.iloc[0].stress_ratio
    r_f = SNC_f_df.iloc[0].stress_ratio

    a_x = SNC_x_df.iloc[0].A
    a_y = SNC_y_df.iloc[0].A
    a_f = SNC_f_df.iloc[0].A

    b_x = SNC_x_df.iloc[0].B
    b_y = SNC_y_df.iloc[0].B
    b_f = SNC_f_df.iloc[0].B

    theta = math.radians(
        SNC_refdata_df.loc["desirable_angle"][0]
    )  # (DANGLE / 180) * 3.14159265
    tempangle = math.radians(
        SNC_refdata_df.loc["off_axis_angle"][0]
    )  # (THIRDSN / 180) * 3.14159265

    tm = math.sin(tempangle) ** 4
    tnc = math.cos(tempangle) ** 4
    tmn = math.sin(tempangle) ** 2 * math.cos(tempangle) ** 2

    nc = math.cos(theta) ** 4
    m = math.sin(theta) ** 4
    mn = math.sin(theta) ** 2 * math.cos(theta) ** 2

    stress_ratio = (
        float(r_x)
        if math.isclose(
            (r_x + r_y + r_f) / 3,
            r_x,
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

    if SNC_refdata_df.loc["sn_model"][0] == "Log-Log":
        get_stress = get_loglog_stress
        get_sn = get_loglog_sn
    else:
        get_stress = get_linlog_stress
        get_sn = get_linlog_sn

    output_df["x"] = get_stress(a_x, b_x, output_df.cycles_to_failure)
    output_df["y"] = get_stress(a_y, b_y, output_df.cycles_to_failure)

    if SNC_refdata_df.loc["off_axis_angle"][0] != 0:

        output_df["t"] = get_stress(a_f, b_f, output_df.cycles_to_failure)

        output_df["ssqr"] = output_df.apply(
            lambda z: get_ssqr(z.x, z.y, tnc, tm, tmn, z.t), axis=1
        )

        # Remove rows where ssqr >= 0
        output_df.drop(output_df[output_df.ssqr < 0].index, inplace=True)
        output_df["s"] = output_df.apply(lambda z: 1 / math.sqrt(z.ssqr), axis=1)

    else:
        output_df["s"] = get_stress(a_f, b_f, output_df.cycles_to_failure)

    if SNC_refdata_df.loc["off_axis_angle"][0] == 22.5:
        output_df["s"] = output_df["t"] / 2.2

    output_df["stress_parameter"] = output_df.apply(
        lambda z: get_sn(z.x, z.y, nc, m, mn, z.s), axis=1
    )

    pass

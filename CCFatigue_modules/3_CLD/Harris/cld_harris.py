#!/usr/bin/env python
""" CCFatigue - Module 3 - CLD-Harris.py

Harris diagram is described in Tassos red book p. 108 - the original papers are:
[1] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[2] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_FILENAME = "SNC_input.csv"
INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_CSV_FILENAME = "CLD_Harris.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
UCS = 27.1
UTS = 27.7

BOUNDS_MARGIN = 0.99

# Cycles for the isolines (the lines of the CLD)
CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def calculate_stress_amplitude(stress_ratio: float, max_stress: float) -> float:
    """
    Calculate stress_amplitude (sigma_a) for given stress ratio (R) and max stress
    Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L68
    - lines 68-74
    Inputs:
    - stress_ratio (R)
    - max_stress (sigma_max)
    Output:
    - stess amplitude sigma_a
    """
    if abs(stress_ratio) > 1:
        stress_amplitude = (1 - (1 / stress_ratio)) * max_stress / 2
    else:
        stress_amplitude = (1 - stress_ratio) * max_stress / 2
    return stress_amplitude


def calculate_stress_mean(stress_ratio: float, max_stress: float) -> float:
    """
    Calculate mean stress (sigma_mean) for given stress ratio (R) and max stress
    Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L68
    - lines 68-74
    Inputs:
    - stress_ratio (R)
    - max_stress (sigma_max)
    - uts
    - ucs
    Output:
    - mean stress sigma_mean
    """
    if abs(stress_ratio) > 1:
        sigma_mean = (1 - (1 / stress_ratio)) * max_stress / 2
    else:
        sigma_mean = (1 + stress_ratio) * max_stress / 2

    return sigma_mean


def bounds_stress_mean(sigma_mean, uts: float, ucs: float, margin: float) -> float:
    """
    Apply bounds (uts, ucs) to mean stress (sigma_mean)
    Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L86
    - lines 86-95
    Inputs:
    - sigma_mean
    - uts (upper bound)
    - ucs (lower bound)
    - margin
    Output:
    - mean stress sigma_mean
    """
    if sigma_mean > uts:
        sigma_mean = margin * uts
    if sigma_mean < -ucs:
        sigma_mean = margin * -ucs

    return sigma_mean


def calculate_fuv(x1_array, x2_array, y_array):
    """
    Calculate f, u and v
    Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L109
    - lines 109-135
    Inputs:
    - x1_array: np array of x1, one per stress ratio
    - x2_array: np array of x2, one per stress ratio
    - y_array: np array of y, one per stress ratio
    Outputs:
    - (f, u, v)
    """

    # Prepare matrix
    # x_matrix = np.vstack(x1_array)
    x_matrix = np.concatenate([np.vstack(x1_array), np.vstack(x2_array)], axis=1)
    x_matrix = np.insert(x_matrix, 0, 1, axis=1)

    y_matrix = np.vstack(y_array)

    # Computes the transpose product of a matrix
    xtx = np.dot(x_matrix.T, x_matrix)

    # Computes the generalized inverse of a real matrix
    invxtx = np.linalg.pinv(xtx)

    # Multiplies two real rectangular matrices, AB.
    xty = np.dot(x_matrix.T, y_matrix)
    bet = np.dot(invxtx, xty)

    f = 10 ** bet[0, 0]
    u = bet[1, 0]
    v = bet[2, 0]

    return (f, u, v)


if __name__ == "__main__":

    # Import input files (SNC format)
    SNC_df = pd.read_csv(INPUT_FILE)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    SNC_df["stress_ratio_id"] = (
        SNC_df.stress_ratio != SNC_df.stress_ratio.shift()
    ).cumsum()

    # Average by stress_ratio
    stress_ratios_df = (
        SNC_df[
            [
                "stress_ratio_id",
                "stress_ratio",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
    )

    # Error message if less than 3 R-ratios
    if len(stress_ratios_df) < 3:
        raise Exception("Input data is not enough to apply Haris' method.")

    # Calculate sigma amplitude
    SNC_df["stress_amplitude"] = SNC_df.apply(
        lambda x: calculate_stress_amplitude(x.stress_ratio, x.stress_parameter), axis=1
    )

    # Calculate mean sigma
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: calculate_stress_mean(x.stress_ratio, x.stress_parameter),
        axis=1,
    )

    # Apply bounds to mean sigma (applied to module 3, not to module 5)
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: bounds_stress_mean(x.stress_mean, UTS, UCS, BOUNDS_MARGIN),
        axis=1,
    )

    SNC_df["y"] = SNC_df.apply(lambda x: math.log10(x.stress_amplitude / UTS), axis=1)
    SNC_df["x1"] = SNC_df.apply(lambda x: math.log10(1 - (x.stress_mean / UTS)), axis=1)
    SNC_df["x2"] = SNC_df.apply(
        lambda x: math.log10((UCS / UTS) + (x.stress_mean / UTS)), axis=1
    )

    cycles_df = pd.DataFrame(
        SNC_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

    # Log10(N)
    cycles_df["log10_cycles_to_failure"] = np.log10(cycles_df.cycles_to_failure)

    for cycles_count in SNC_df.cycles_to_failure.unique():

        cycle_df = SNC_df.loc[SNC_df.cycles_to_failure == cycles_count]

        f, u, v = calculate_fuv(
            cycle_df.x1.to_numpy(), cycle_df.x2.to_numpy(), cycle_df.y.to_numpy()
        )

        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "f"] = f
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "u"] = u
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "v"] = v

    # Linear Regression to calculate A and B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)

    CLD_df = pd.DataFrame()

    for onc in CYCLES_COUNT:

        # Eq 8 p530 ref[1]
        ff = linregress_f.slope * math.log10(onc) + linregress_f.intercept
        uu = linregress_u.slope * math.log10(onc) + linregress_u.intercept
        vv = linregress_v.slope * math.log10(onc) + linregress_v.intercept

        for sm in np.arange(-0.90 * UCS, 0.90 * UTS, 0.90 * (UTS + UCS) / 40):
            # row = stress_ratios_df.copy()

            C2 = ff * (1 - sm / UTS) ** uu
            C3 = ((UCS / UTS) + (sm / UTS)) ** vv

            stress_amplitude = C2 * C3 * UTS
            stress_mean = sm

            row = pd.DataFrame(
                {
                    "cycles_to_failure": [onc],
                    "stress_amplitude": [stress_amplitude],
                    "stress_mean": [stress_mean],
                }
            )

            CLD_df = pd.concat(
                [
                    CLD_df,
                    row,
                ],
                ignore_index=True,
            )

    pass

    # Generate output files
    CLD_df.to_csv(OUTPUT_CSV_FILE, index=False)

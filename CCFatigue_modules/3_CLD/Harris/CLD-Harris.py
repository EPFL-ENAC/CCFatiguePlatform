#!/usr/bin/env python
""" CCFatigue - Module 3 - CLD-Harris.py

Harris diagram is described in Tassos red book p. 108 - the original papers are 
https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub (https://doi.org/10.1016/0142-1123(94)90478-2)
https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub (https://doi.org/10.1016/S0266-3538(97)00121-8)
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
OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
UCS = 27.1
UTS = 27.7
CRITICAL_STRESS_RATIO = 0.1

# Cycles for the isolines (the lines of the CLD)
CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


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
                # "log10_stress_parameter",
                # "log10_number_of_cycles",
                # "number_of_cycles",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
    )

    # Error message if number of R-ratios were less than 3
    if len(stress_ratios_df) < 3:
        print("Input data is not enough to apply Haris' method.")
        exit()

    # For each samples with abs(stress_ratio) > 1,
    #     calculate the amplitude and mean stress
    SNC_df.loc[abs(SNC_df.stress_ratio) > 1, "stress_amplitude"] = (
        (1 - (1 / SNC_df.stress_ratio)) * SNC_df.stress_parameter / 2
    )
    SNC_df.loc[abs(SNC_df.stress_ratio) > 1, "mean_stress"] = (
        -(1 + (1 / SNC_df.stress_ratio)) * SNC_df.stress_parameter / 2
    )

    # For each samples with abs(stress_ratio) <= 1,
    #     calculate the amplitude and mean stress
    SNC_df.loc[abs(SNC_df.stress_ratio) <= 1, "stress_amplitude"] = (
        (1 - SNC_df.stress_ratio) * SNC_df.stress_parameter / 2
    )
    SNC_df.loc[abs(SNC_df.stress_ratio) <= 1, "mean_stress"] = (
        (1 + SNC_df.stress_ratio) * SNC_df.stress_parameter / 2
    )

    # If mean_stress > UTS => mean_stress = 0.99 UTS
    SNC_df.loc[SNC_df.mean_stress > UTS, "mean_stress"] = 0.99 * UTS

    # If mean_stress < -UCS => mean_stress = 0.99 * -UCS
    SNC_df.loc[SNC_df.mean_stress < -UCS, "mean_stress"] = 0.99 * -UCS

    SNC_df["y"] = SNC_df.apply(lambda x: math.log10(x.stress_amplitude / UTS), axis=1)
    SNC_df["x1"] = SNC_df.apply(lambda x: math.log10(1 - (x.mean_stress / UTS)), axis=1)
    SNC_df["x2"] = SNC_df.apply(
        lambda x: math.log10((UCS / UTS) + (x.mean_stress / UTS)), axis=1
    )

    cycles_df = pd.DataFrame(
        SNC_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

    # Log10(N)
    cycles_df["log10_cycles_to_failure"] = np.log10(cycles_df.cycles_to_failure)

    for cycles_count in SNC_df.cycles_to_failure.unique():

        cycle_df = SNC_df.loc[SNC_df.cycles_to_failure == cycles_count]

        # Prepare matrix
        x_matrix = cycle_df[["x1", "x2"]].to_numpy()
        x_matrix = np.insert(x_matrix, 0, 1, axis=1)

        y_matrix = cycle_df[["y"]].to_numpy()

        # Computes the transpose product of a matrix
        xtx = np.dot(x_matrix.T, x_matrix)

        # Computes the generalized inverse of a real matrix
        invxtx = np.linalg.pinv(xtx)

        # Multiplies two real rectangular matrices, AB.
        xty = np.dot(x_matrix.T, y_matrix)
        bet = np.dot(invxtx, xty)

        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "f"] = (
            10 ** bet[0, 0]
        )
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "u"] = bet[1, 0]
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "v"] = bet[2, 0]

    # Linear Regression to calculate A and B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)

    CLD_df = pd.DataFrame()

    for onc in CYCLES_COUNT:

        ff = linregress_f.slope * math.log10(onc) + linregress_f.intercept
        uu = linregress_u.slope * math.log10(onc) + linregress_u.intercept
        vv = linregress_v.slope * math.log10(onc) + linregress_v.intercept

        for sm in np.arange(-0.90 * UCS, 0.90 * UTS, 0.90 * (UTS + UCS) / 40):
            # row = stress_ratios_df.copy()

            C2 = ff * (1 - sm / UTS) ** uu
            C3 = ((UCS / UTS) + (sm / UTS)) ** vv

            stress_amplitude = C2 * C3 * UTS
            mean_stress = sm

            row = pd.DataFrame(
                {
                    "onc": [onc],
                    "stress_amplitude": [stress_amplitude],
                    "mean_stress": [mean_stress],
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
    # TODO generate output files

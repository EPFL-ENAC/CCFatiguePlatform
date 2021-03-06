#!/usr/bin/env python
"""
Implementation of the Piecewise Linear Methods as described in:
Anastasios P. Vassilopoulos, Behzad D. Manshadi, Thomas Keller,
Influence of the constant life diagram formulation on the fatigue life prediction of
composite materials,
International Journal of Fatigue,
Volume 32, Issue 4, 2010, Pages 659-669, ISSN 0142-1123,
https://doi.org/10.1016/j.ijfatigue.2009.09.008
(Section 2.2)
"""

import math
import os
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

# Variables def
#
# sigma prime a = ???
known_R_ratios = ### Here make a list of known R ratios from the SNC json input.

def calculate_stress_amplitude(R, known_R_ratios):
   """
   Calculates stress amplitude corresponsing to given R, according to method
   Inputs :
    - R for which we want to estimate stress amplitude
    - known_R_ratios
    - corresponding_stress_amplitudes_for_known_R_ratios = ????

    Outputs :
    - stress amplitude (sigma_a') corresponding to given R
    """

#     Step 1 - Identifiy between which R ratios R' is located.

    if abs(R) > 1:
        stress_amplitude = CC_sector_equation()
    else: #elif R < 0:
        stress_amplitude = TT_sector_equation()
    elif R == 0 :
        ?? ## What happens when R == 0 or 1 ?
    elif R == 1 :
        ?? ## What happens when R == 0 or 1 ?

#     R_1_TT = SNC_df.R_ratios[0]

#     if abs(R) > 1:
#         sigma_prime_a = CC_sector_equation()
#     else: #elif R < 0:
#         sigma_prime_a = TT_sector_equation()
#     elif R == 0 :
#         ?? ## What happens when R == 0 or 1 ?
#     elif R == 1 :
#         ?? ## What happens when R == 0 or 1 ?
#     else:
#         ## Locate whic Rmin, Rmax known R_ratios R is located between:  TODO - TEST BEHAVIOR !
#         for Rmin in R_ratios:
#             if R > Rmin:
#                 pass
#         for Rmax in R_ratios:
#             if R < Rmax:
#                 continue

#         sigma_prime_a = CT_sector_equation(Rmin, Rmax)


# def TT_sector_equation(R, UTS, R1, sigmaA_R1):
#     """
#     Equation 3
#     """
#     sigma_prime_a = UTS / ( (UTS/sigmaA1) + R - R1)

#     return sigma_prime_a


# def TT_sector_equation(R, Rmin, Rmax, sigmaA_Rmin, sigmaA_Rmax):
#     """
#     Equation 4
#     """
#     sigma_prime_a = TODO

#     return sigma_prime_a


# def CC_sector_equation(R, UCS, R1, sigmaA1):
#     """
#     Equation 5
#     """
#     sigma_prime_a = TODO

#     return sigma_prime_a


def get_asigma(stress_ratio: float, stress_parameter: float) -> float:
    """TODO
    Function also used by module 5
    """
    asigma = 0
    if abs(stress_ratio) > 1:
        asigma = (1 - (1 / stress_ratio)) * stress_parameter / 2
    else:
        asigma = (1 - stress_ratio) * stress_parameter / 2

    return asigma


def stress_amplitude_equation(
    slope: float, intercept: float, cycles_count: int
) -> float:
    """TODO"""
    stress_amplitude = 10 ** (
        -(slope / intercept) + (1 / intercept) * math.log10(cycles_count)
    )
    return stress_amplitude


if __name__ == "__main__":

    # Import input files (SNC format)
    SNC_df = pd.read_csv(INPUT_FILE)

    # for N in .... :  TODO
    #     sigma_a calculate_sigma_prime_a(R, R_ratios)
    #     ## write results in CLD_df

    # CLD_df = pd.to_csv(OUTPUT_FILE)

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

    # # For each samples with abs(stress_ratio) > 1,
    # #     calculate the amplitude and mean stress
    # SNC_df.loc[abs(SNC_df.stress_ratio) > 1, "asigma"] = (
    #     (1 - (1 / SNC_df.stress_ratio)) * SNC_df.stress_parameter / 2
    # )

    # # For each samples with abs(stress_ratio) <= 1,
    # #     calculate the amplitude and mean stress
    # SNC_df.loc[abs(SNC_df.stress_ratio) <= 1, "asigma"] = (
    #     (1 - SNC_df.stress_ratio) * SNC_df.stress_parameter / 2
    # )
    SNC_df["asigma"] = SNC_df.apply(
        lambda x: get_asigma(x.stress_ratio, x.stress_parameter), axis=1
    )

    # Add columns with log10(stress_parameter) and log10(number_of_cycles)
    SNC_df["log10_asigma"] = np.log10(SNC_df.asigma)
    SNC_df["log10_cycles_to_failure"] = np.log10(SNC_df.cycles_to_failure)

    # Calculate A and B (linear regression)
    linregress = SNC_df.groupby("stress_ratio_id").apply(
        lambda x: stats.linregress(x.log10_asigma, x.log10_cycles_to_failure)
    )

    # Add A and B in avg_by_stress_ratio
    # Slope = A, Intercept = B
    stress_ratios_df["slope"] = linregress.apply(lambda x: x[1])
    stress_ratios_df["intercept"] = linregress.apply(lambda x: x[0])

    # Sort the stress ratios
    SNC_df.sort_values(
        by=["stress_ratio", "stress_ratio_id", "cycles_to_failure"], inplace=True
    )

    CLD_df = pd.DataFrame()

    for onc in CYCLES_COUNT:

        row = stress_ratios_df.copy()
        row["onc"] = onc
        row["stress_amplitude"] = row.apply(
            lambda x: stress_amplitude_equation(x.slope, x.intercept, onc), axis=1
        )

        # Eq 1
        row["mean_stress"] = (
            (1 + row.stress_ratio) * row.stress_amplitude / (1 - row.stress_ratio)
        )

        CLD_df = pd.concat([CLD_df, row], ignore_index=True)
    pass  # debug purpose
    # TODO generate output files

#!/usr/bin/env python
""" CCFatigue - Module 5 - Miner-Piecewise-linear.py

There is no documentation specifically for this one. A description is given in the red book,
after p. 206
ref:
[1] https://www.sciencedirect.com/science/article/pii/S1359835X04000466?via%3Dihub
    (https://doi.org/10.1016/j.compositesa.2004.02.009)
[2] https://doi.org/10.1016/j.ijfatigue.2009.09.008

"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_SNC_FILENAME = "SNC_input.csv"
INPUT_SNC_FILE = os.path.join(SRC_DIR, INPUT_SNC_FILENAME)
INPUT_CYC_FILENAME = "CYC_ccinput.csv"
INPUT_CYC_FILE = os.path.join(SRC_DIR, INPUT_CYC_FILENAME)

OUTPUT_CSV_FILENAME = "DAS_PiecewiseLinear.csv"
OUTPUT_CSV_FILE = os.path.join(SRC_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
UCS = 367.2
UTS = 416.5
CRITICAL_STRESS_RATIO = -1

# Cycles for the isolines (the lines of the CLD)
CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9

# Factor loop
FACTOR_START = 1.0
FACTOR_STOP = 0.9
FACTOR_STEP = -1.0


def tt_sector_equation(r_prime, uts, r_1tt, sigma_a_1tt) -> float:
    """
    Equation 3 p661 ref[2]
    """
    sigma_prime_a = uts / ((uts / sigma_a_1tt) + r_prime - r_1tt)

    return sigma_prime_a


def between_known_stress_ratios_equation(
    sigma_a1: float, sigma_a2: float, r1: float, r2: float, r_prime
) -> float:
    """
    Equation 4 p661 ref[2]
    """
    sigma_prime_a = (
        sigma_a1 * (r1 - r2) / ((r1 - r_prime) * sigma_a1 / sigma_a2 + (r_prime - r2))
    )

    return sigma_prime_a


def cc_sector_equation(
    r_prime: float, ucs: float, r_1cc: float, sigma_a_1cc: float
) -> float:
    """
    Equation 5 p661 ref[2]
    """
    sigma_prime_a = UTS / ((ucs / sigma_a_1cc) - r_prime + r_1cc)

    return sigma_prime_a


def minus_r_equation(stress_ratio: float) -> float:
    """
    Part of equation 3 p661 ref[2]
    """
    r = (1 + stress_ratio) / (1 - stress_ratio)
    return r


def get_sector(stress_ratio: float) -> int:
    """
    Get sector corresponding to given stress_ratio according to fig 1, p660, ref [2]
    sector 0 = TT (0 < R < 1)
    sector 1 = TC (R < 0)
    sector 2 = CC (R > 1)
    Inputs :
    - stress_ratio (R)

    Outputs :
    - sector
    """
    if stress_ratio == 1:
        raise NotImplementedError(
            "R=1 is not treated since this is not considered "
            "to be a fatigue case, but is called creep"
        )

    if stress_ratio < 1 and stress_ratio >= 0:
        return 0
    elif stress_ratio < 0:
        return 1
    else:
        return 2


def r_is_bigger(r1: float, r2: float) -> bool:
    """
    Compare two stress ratios according to sectors (see fig 1, p660, ref [2]).
    Return True if r1 is bigger than r2
    Inputs:
    - r1: stress ratio
    - r2: stress ratio

    Outputs:
    - bool: r1 > r2
    """
    s1 = get_sector(r1)
    s2 = get_sector(r2)
    return s1 < s2 or (s1 == s2 and r1 > r2)


def calculate_sigma_a_prime(stress_ratio, known_stress_ratios_df, uts, ucs) -> float:
    """
    Calculates stress amplitude sigma_a_prime corresponsing to given R,
    according to method PiecewiseLinear CLD, p661, ref [2]
    Inputs :
    - stress_ratio (R) for which we want to estimate stress amplitude
    - known_stress_ratios_df, sorted by sectors then by descending stress
    - uts
    - ucs

    Outputs :
    - stress amplitude (sigma_a') corresponding to given R
    """

    known_stress_ratios = known_stress_ratios_df.stress_ratio.unique()

    # If R' > all known R (bigger according to sectors)
    if r_is_bigger(stress_ratio, known_stress_ratios[0]):
        stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == known_stress_ratios[0].stress_ratio
        ].apply(
            lambda x: tt_sector_equation(
                minus_r_equation(stress_ratio),
                uts,
                minus_r_equation(known_stress_ratios[0]),
                x.stress_amplitude,
            ),
            axis=1,
        )

    # If R' < all known R
    elif r_is_bigger(known_stress_ratios[-1], stress_ratio):
        stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == known_stress_ratios[-1].stress_ratio
        ].apply(
            lambda x: cc_sector_equation(
                minus_r_equation(stress_ratio),
                ucs,
                minus_r_equation(known_stress_ratios[-1]),
                x.stress_amplitude,
            ),
            axis=1,
        )

    # R' between known R
    else:

        # Identifiy between which R ratios R' is located.
        smaller = known_stress_ratios[0]
        for r in known_stress_ratios:
            if r_is_bigger(stress_ratio, r):
                smaller = r
                break
            else:
                bigger = r

        # Get stress amplitudes for each cycles_to_failure for bigger and smaller R
        bigger_stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == bigger
        ].stress_amplitude.reset_index(drop=True)
        smaller_stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == smaller
        ].stress_amplitude.reset_index(drop=True)

        known_stress_amplitudes = pd.DataFrame(
            {
                "sigma_a1": bigger_stress_amplitudes,
                "sigma_a2": smaller_stress_amplitudes,
            }
        )

        # Compute minus r for R1, R2 and R'
        r1 = minus_r_equation(bigger)
        r2 = minus_r_equation(smaller)
        r_prime = minus_r_equation(stress_ratio)

        stress_amplitudes = known_stress_amplitudes.apply(
            lambda x: between_known_stress_ratios_equation(
                x.sigma_a1, x.sigma_a2, r1, r2, r_prime
            ),
            axis=1,
        )

    return stress_amplitudes


def calculate_stress_amplitude(stress_ratio: float, stress_parameter: float) -> float:
    """
    Calculate stress amplitude corresponsing to given R and sigma_max
    Inputs :
    - stress_ratio (R)
    - stress_parameter (sigma_max)

    Outputs :
    - stress amplitude (sigma_a)
    """
    stress_amplitude = 0
    if abs(stress_ratio) > 1:
        stress_amplitude = (1 - (1 / stress_ratio)) * stress_parameter / 2
    else:
        stress_amplitude = (1 - stress_ratio) * stress_parameter / 2

    return stress_amplitude


if __name__ == "__main__":

    SNC_df = pd.read_csv(INPUT_SNC_FILE)  # Known stress ratios
    CYC_df = pd.read_csv(INPUT_CYC_FILE)

    # Calculate stress amplitude (sigma_a) for each known stress ratios
    SNC_df["stress_amplitude"] = SNC_df.apply(
        lambda x: calculate_stress_amplitude(x.stress_ratio, x.stress_parameter), axis=1
    )

    # Sort know stress ratios by sector counterclockwise according to fig 1, p660, ref [2]
    # - sector 0 = TT (0 < R < 1)
    SNC_df.loc[(SNC_df.stress_ratio < 1) & (SNC_df.stress_ratio >= 0), "sector"] = 0
    # - sector 1 = TC (R < 0)
    SNC_df.loc[SNC_df.stress_ratio < 0, "sector"] = 1
    # - sector 2 = CC (R > 1)
    SNC_df.loc[SNC_df.stress_ratio > 1, "sector"] = 2
    SNC_df.sort_values(
        by=["sector", "stress_ratio"],
        ascending=[True, False],
        kind="mergesort",
        inplace=True,
    )

    stress_ratio = SNC_df.iloc[0].stress_ratio
    log10_cycles_to_failure = np.log10(
        SNC_df.loc[SNC_df.stress_ratio == stress_ratio].cycles_to_failure
    )

    # For each stress ratio in CCinput
    for stress_ratio in CYC_df.stress_ratio.unique():

        # Calculate stress amplitudes prime (sigma_a')
        stress_amplitude_prime = calculate_sigma_a_prime(stress_ratio, SNC_df, UTS, UCS)

        # Calculate slope A and intercept B
        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L265
        linregress = stats.linregress(
            np.log10(stress_amplitude_prime), log10_cycles_to_failure
        )
        CYC_df.loc[CYC_df.stress_ratio == stress_ratio, "slope"] = linregress[1]
        CYC_df.loc[CYC_df.stress_ratio == stress_ratio, "intercept"] = linregress[0]

    factors = pd.DataFrame(
        np.arange(FACTOR_START, FACTOR_STOP, FACTOR_STEP), columns=["factor"]
    )
    for factor in factors.factor:

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L236
        CYC_df["sa"] = CYC_df.stress_amplitude / 2 * factor

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L267
        CYC_df["an"] = 10 ** (
            CYC_df.slope + CYC_df.intercept * math.log10(CYC_df.half_stress_range)
        )

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L230
        CYC_df["stress"] = (CYC_df.stress_mean + CYC_df.stress_amplitude / 2) * factor

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L268
        damage = CYC_df.n_cycles / CYC_df.an

        factors.loc[factors.factor == factor, "damage"] = 1 / damage.sum()
        factors.loc[factors.factor == factor, "max_stress"] = CYC_df.stress.max()

    # Generate output file
    factors[["max_stress", "damage"]].to_csv(
        OUTPUT_CSV_FILE,
        index=False,
    )

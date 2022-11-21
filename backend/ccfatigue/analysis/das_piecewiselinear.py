#!/usr/bin/env python
""" CCFatigue - Module 5 - Miner-Piecewise-linear.py

There is no documentation specifically for this one.
A description is given in the red book, after p. 206
ref:
[1] https://www.sciencedirect.com/science/article/pii/S1359835X04000466?via%3Dihub
    (https://doi.org/10.1016/j.compositesa.2004.02.009)
[2] https://doi.org/10.1016/j.ijfatigue.2009.09.008
[3] https://link.springer.com/content/pdf/10.1007/978-1-84996-181-3.pdf
    (DOI 10.1007/978-1-84996-181-3)
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.analysis.utils.piecewiselinear as piecewiselinear

DEFAULT_UCS = 367.2
DEFAULT_UTS = 416.5

# Factor loop
DEFAULT_FACTOR_START = 1.0
DEFAULT_FACTOR_STOP = 0.9
DEFAULT_FACTOR_STEP = -1.0

CRITICAL_STRESS_RATIO = -1

# Cycles for the isolines (the lines of the CLD)
CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def get_lowercase_r(stress_ratio: float) -> float:
    """
    Get r = (1 + R) / (1 - R)
    Part of eq 3 p661 ref[2]
    Parameters
    ----------
        stress_ratio
            Stress ratio (R) [-]
    Returns
    -------
        r
    """
    r = (1 + stress_ratio) / (1 - stress_ratio)
    return r


def get_stress_amplitude_case1(r_prime, uts, r_1tt, sigma_a_1tt) -> float:
    """
    Get stress amplitude for given r' when r' > all known r
    Eq 3 p661 ref[2]
    Parameters
    ----------
        r_prime
            r' = (1 + R') / (1 - R')
        uts
            Ultimate tensile stress (UTS)
        r_1tt
            r_1TT = (1 + R_1TT) / (1 - R_1TT)
        sigma_a_1tt
            Stress amplitude for biggest known r
    Returns
    -------
        sigma_prime_a
    """
    sigma_prime_a = uts / ((uts / sigma_a_1tt) + r_prime - r_1tt)
    return sigma_prime_a


def get_stress_amplitude_case2(
    sigma_a1: float, sigma_a2: float, r1: float, r2: float, r_prime
) -> float:
    """
    Get stress amplitude for given r' when r' between two known r
    Eq 4 p661 ref[2]
    Parameters
    ----------
        sigma_a1
            Stress amplitude for bigger known r
        sigma_a2
            Stress amplitude for smaller known r
        r1
            bigger known r (r_i)
        r2
            smaller known r (r_i+1)
        r_prime
            r' = (1 + R') / (1 - R')
    Returns
    -------
        sigma_prime_a
    """
    sigma_prime_a = (
        sigma_a1 * (r1 - r2) / ((r1 - r_prime) * sigma_a1 / sigma_a2 + (r_prime - r2))
    )
    return sigma_prime_a


def get_stress_amplitude_case3(
    r_prime: float, uts: float, r_1cc: float, sigma_a_1cc: float
) -> float:
    """
    Get stress amplitude for given r' when r' < all known r
    Equation 5 p661 ref[2]
    Parameters
    ----------
        r_prime
            r' = (1 + R') / (1 - R')
        uts
            Ultimate tensile stress (UTS)
        r_1cc
            r_1CC = (1 + R_1CC) / (1 - R_1CC)
        sigma_a_1cc
            Stress amplitude for smallest known r
    Returns
    -------
        sigma_prime_a
    """
    sigma_prime_a = uts / ((uts / sigma_a_1cc) - r_prime + r_1cc)
    return sigma_prime_a


def get_sector(stress_ratio: float) -> int:
    """
    Get sector corresponding to given stress_ratio according to fig 1, p660, ref [2]
    sector 0 = TT (0 < R < 1)
    sector 1 = TC (R < 0)
    sector 2 = CC (R > 1)
    Parameters
    ----------
        stress_ratio
            Stress ratio (R)
    Returns
    -------
        sector
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
    Return True if R1 is bigger than R2
    Parameters
    ----------
        r1
            Stress ratio (R)
        r2
            Stress ratio (R)
    Returns
    -------
        True if R1 is bigger than R2 else False
            True if R
    """
    s1 = get_sector(r1)
    s2 = get_sector(r2)
    return s1 < s2 or (s1 == s2 and r1 > r2)


def calculate_sigma_a_prime(stress_ratio, known_stress_ratios_df, uts, ucs):
    """
    Calculates stress amplitude sigma_a_prime corresponsing to given R,
    according to method PiecewiseLinear CLD, p661, ref [2]
    Parameters
    ----------
        stress_ratio
            Stress ratio (R) for which we want to estimate stress amplitude
        known_stress_ratios_df
            Known stress ratios, sorted by sectors then by descending stress
        uts
            Ultimate tensile stress (UTS)
        ucs
            Ultimate compressive stress (UCS)
    Returns
    -------
        stress_amplitude
            Stress amplitude (sigma_a') corresponding to given stress_ratio (R)
    """

    known_stress_ratios = known_stress_ratios_df.stress_ratio.unique()

    # If R' > all known R (bigger according to sectors rule)
    if r_is_bigger(stress_ratio, known_stress_ratios[0]):
        stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == known_stress_ratios[0].stress_ratio
        ].apply(
            lambda x: get_stress_amplitude_case1(
                get_lowercase_r(stress_ratio),
                uts,
                get_lowercase_r(known_stress_ratios[0]),
                x.stress_amplitude,
            ),
            axis=1,
        )

    # If R' < all known R
    elif r_is_bigger(known_stress_ratios[-1], stress_ratio):
        stress_amplitudes = known_stress_ratios_df.loc[
            known_stress_ratios_df.stress_ratio == known_stress_ratios[-1].stress_ratio
        ].apply(
            lambda x: get_stress_amplitude_case3(
                get_lowercase_r(stress_ratio),
                ucs,
                uts,
                get_lowercase_r(known_stress_ratios[-1]),
                x.stress_amplitude,  # type: ignore
            ),
            axis=1,
        )

    # R' between known R
    else:

        # Identifiy between which R ratios R' is located.
        smaller = known_stress_ratios[0]
        bigger = known_stress_ratios[-1]
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
        r1 = get_lowercase_r(bigger)
        r2 = get_lowercase_r(smaller)
        r_prime = get_lowercase_r(stress_ratio)

        stress_amplitudes = known_stress_amplitudes.apply(
            lambda x: get_stress_amplitude_case2(
                x.sigma_a1, x.sigma_a2, r1, r2, r_prime
            ),
            axis=1,
        )

    return stress_amplitudes


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cyc_csv_input_file: FilePath | ReadCsvBuffer,
    das_csv_output_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
    factor_start: float = DEFAULT_FACTOR_START,
    factor_stop: float = DEFAULT_FACTOR_STOP,
    factor_step: float = DEFAULT_FACTOR_STEP,
) -> None:
    """
    Execute the CLD Harris algorithm
    Parameters
    ----------
        snc_csv_input_file
            SNC input file
        cyc_csv_input_file
            CYC input file
        das_csv_output_file
            DAS output file
        ucs
            Ultimate compressive stress
        uts
            Ultimate tensile stress
        factor_start
            load cycle estimate loop factor start
        factor_stop
            load cycle estimate loop factor stop
        factor_step
            load cycle estimate loop factor step
    Returns
    -------
        None
    """

    snc_df = pd.read_csv(snc_csv_input_file)  # Known stress ratios
    cyc_df = pd.read_csv(cyc_csv_input_file)

    # Calculate stress amplitude (sigma_a) for each known stress ratios
    snc_df["stress_amplitude"] = snc_df.apply(
        lambda x: piecewiselinear.calculate_stress_amplitude(
            x.stress_ratio, x.stress_max
        ),
        axis=1,
    )

    # Sort know stress ratios by sector counterclockwise
    # according to fig 1, p660, ref [2]
    piecewiselinear.sort_by_stress_ratios(snc_df)

    stress_ratio = snc_df.iloc[0].stress_ratio
    log10_cycles_to_failure = np.log10(
        snc_df.loc[snc_df.stress_ratio == stress_ratio].cycles_to_failure
    )

    # For each stress ratio in CCinput
    for stress_ratio in cyc_df.stress_ratio.unique():

        # Calculate stress amplitudes prime (sigma_a')
        stress_amplitude_prime = calculate_sigma_a_prime(stress_ratio, snc_df, uts, ucs)

        # Calculate slope A and intercept B
        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L265
        linregress = stats.linregress(
            np.log10(stress_amplitude_prime), log10_cycles_to_failure
        )
        cyc_df.loc[cyc_df.stress_ratio == stress_ratio, "slope"] = linregress[1]
        cyc_df.loc[cyc_df.stress_ratio == stress_ratio, "intercept"] = linregress[0]

    factors = pd.DataFrame(
        np.arange(factor_start, factor_stop, factor_step), columns=["factor"]
    )
    for factor in factors.factor:

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L236
        cyc_df["stress_amplitude"] = cyc_df.stress_range / 2 * factor

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L267
        cyc_df["cycles_to_failure"] = 10 ** (
            cyc_df.slope + cyc_df.intercept * np.log10(cyc_df.stress_amplitude)
        )

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L230
        cyc_df["stress_max"] = (cyc_df.stress_mean + cyc_df.stress_range / 2) * factor

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Piecewise-Linear/Miner-Piecewise-linear.for#L268
        damage = cyc_df.n_cycles / cyc_df.cycles_to_failure

        factors.loc[factors.factor == factor, "damage"] = 1 / damage.sum()
        factors.loc[factors.factor == factor, "max_stress"] = cyc_df.stress_max.max()

    # Generate output file
    factors[["max_stress", "damage"]].to_csv(
        das_csv_output_file,
        index=False,
    )

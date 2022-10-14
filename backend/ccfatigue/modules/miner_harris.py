""" CCFatigue - Module 5 - miner_harris.py
This code takes in SNC + CYC data and output DAS data

Harris diagram is described in Tassos red book p. 108 - the original papers are:
[1] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[2] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

# TODO: bounds_stress_mean should not be used according to fortran but
# without it the code crash when calculating x1 (log(negative value))
# Issue #121 asks Tassos for EXE and input files to compare results

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.modules.harris_utils as harris_utils


def calculate_normalized_alternating_stress(
    f: float, m: float, c: float, u: float, v: float
):
    """
    Calculate the normalized alternating stress component
    Equation 1 p9 Ref [2]
    Parameters
    ----------
        f
            function of the laminate tensile strength
        m
            normalized mean stress
        c
            normalized compression stress
        u
            shape of the right (predominatly tensile) wings of bell-shaped curve
        v
            shape of the left (predominatly compressive) wings of bell-shaped curve
    Returns
    -------
        a
            normalized alternating stress component
    """
    a = f * (1 - m) ** u * (c + m) ** v
    return a


def calculate_damage(
    stress_mean,
    stress_ratio,
    n_cycles,
    factor: float,
    linregress_f,
    linregress_u,
    linregress_v,
):
    """
    Calculate damage for a specific damage
    Parameters
    ----------
        stress_mean: Series
            CYC input Dataframe
        factor: float
        linregress_f: LinregressResult
            Slope and intercept for f (function of the laminate tensile strength)
        linregress_u
            Slope and intercept for u (shape of the right (predominatly tensile) wings
            of bell-shaped curve)
        linregress_v
            Slope and intercept for v (shape of the left (predominatly compressive)
            wings of bell-shaped curve)
    Returns
    -------
        damage: Series[]
            damage for each row of CYC input file
    """
    # Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Harris/Miner-Harris.for#L177  # noqa
    #      lines 177-249

    loop_params = [
        (1000, 1),
        (10e5, 10),
        (10e8, 10000),
        (10e12, 10e7),
        (10e16, 10e10),
        (10e25, 10e12),
    ]

    samp = stress_mean * factor + (1 - stress_ratio) / (1 + stress_ratio)

    a = 10000
    jj = 1

    for (max, inc) in loop_params:
        while jj < max and a >= samp:

            f = linregress_f.slope * np.log10(jj) + linregress_f.intercept
            u = linregress_u.slope * np.log10(jj) + linregress_u.intercept
            v = linregress_v.slope * np.log10(jj) + linregress_v.intercept
            a = calculate_normalized_alternating_stress(
                f,
                stress_mean * factor / harris_utils.UTS,
                harris_utils.UCS / harris_utils.UTS,
                u,
                v,
            )
            jj += inc

    damage = n_cycles / jj
    return damage


def execute(
    input_snc_file: FilePath | ReadCsvBuffer,
    input_cyc_file: FilePath | ReadCsvBuffer,
    output_csv_file: FilePath | WriteBuffer,
    factor_start=1.0,
    factor_stop=0.9,
    factor_step=-1.0,
) -> None:
    """
    Execute the CLD Harris algorithm
    Parameters
    ----------
        input_snc_file: FilePath | ReadCsvBuffer,
            SNC input file
        input_cyc_file: FilePath | ReadCsvBuffer,
            CYC input file
        output_csv_file: FilePath | WriteBuffer,
            DAS utput file
    Returns
    -------
        None
    """

    ucs = harris_utils.UCS
    uts = harris_utils.UTS

    SNC_df = pd.read_csv(input_snc_file)  # Known stress ratios
    CYC_df = pd.read_csv(input_cyc_file)

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
        lambda x: harris_utils.calculate_stress_amplitude(x.stress_ratio, x.stress_max),
        axis=1,
    )

    # Calculate mean sigma
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: harris_utils.calculate_stress_mean(x.stress_ratio, x.stress_max),
        axis=1,
    )

    # TODO should not be applied to module 5 according to code
    # but cause error if not applied: log10(x < 0)
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: harris_utils.bounds_stress_mean(
            x.stress_mean,
            uts,
            ucs,
            harris_utils.BOUNDS_MARGIN,
        ),
        axis=1,
    )

    # SNC_df["y"] = SNC_df.apply(lambda x: np.log10(x.stress_amplitude / uts), axis=1)
    # SNC_df["x1"] = SNC_df.apply(lambda x: np.log10(1 - (x.stress_mean / uts)), axis=1)
    # SNC_df["x2"] = np.log10(ucs / uts + SNC_df.stress_mean / uts)
    SNC_df["y"] = np.log10(SNC_df.stress_amplitude / uts)
    SNC_df["x1"] = np.log10(1 - (SNC_df.stress_mean / uts))
    SNC_df["x2"] = np.log10(ucs / uts + SNC_df.stress_mean / uts)

    cycles_df = pd.DataFrame(
        SNC_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

    # Log10(N)
    cycles_df["log10_cycles_to_failure"] = np.log10(cycles_df.cycles_to_failure)

    for cycles_count in SNC_df.cycles_to_failure.unique():

        cycle_df = SNC_df.loc[SNC_df.cycles_to_failure == cycles_count]

        f, u, v = harris_utils.calculate_fuv(
            cycle_df.x1.to_numpy(), cycle_df.x2.to_numpy(), cycle_df.y.to_numpy()
        )

        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "f"] = f
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "u"] = u
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "v"] = v

    # Linear Regression to calculate slope A and intercept B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)
    # af, bf = linregress_f.slope, linregress_f.intercept
    # au, bu = linregress_u.slope, linregress_u.intercept
    # av, bv = linregress_v.slope, linregress_v.intercept

    factors = pd.DataFrame(
        np.arange(factor_start, factor_stop, factor_step),
        columns=["factor"],
    )
    for factor in factors.factor:

        CYC_df["damage"] = CYC_df.apply(
            lambda x: calculate_damage(
                x.stress_mean,
                x.stress_ratio,
                x.n_cycles,
                factor,
                linregress_f,
                linregress_u,
                linregress_v,
            ),
            axis=1,
        )

        CYC_df["stress"] = (CYC_df.stress_mean + CYC_df.stress_range / 2) * factor

        factors.loc[factors.factor == factor, "damage"] = 1 / CYC_df.damage.sum()
        factors.loc[factors.factor == factor, "stress_max"] = CYC_df.stress.max()

    # Generate output file
    factors[["stress_max", "damage"]].to_csv(
        output_csv_file,  # type: ignore
        index=False,
    )

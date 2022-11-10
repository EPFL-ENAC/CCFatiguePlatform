""" CCFatigue - Module 5 - miner_harris.py
This code takes in SNC + CYC data and output DAS data

Harris diagram is described in Tassos red book p. 108
[1] https://link.springer.com/content/pdf/10.1007/978-1-84996-181-3.pdf
The original papers are:
[2] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[3] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import optimize, stats

import ccfatigue.analysis.harris_utils as harris_utils

DEFAULT_UCS = 367.2
DEFAULT_UTS = 416.5
DEFAULT_FACTOR_START = 1.4
DEFAULT_FACTOR_STOP = 0.4
DEFAULT_FACTOR_STEP = -0.1


def get_normalized_stress_amplitude(m: float, c: float, f: float, u: float, v: float):
    """
    Calculate the normalized stress amplitude component
    Eq 4.10 p108 Ref [1]
    Parameters
    ----------
        m
            normalized mean stress (=sigma_a / UTS)
        c
            normalized compression strength (UCS / UTS)
        f
            function of the laminate tensile strength
        u
            shape of the right (predominatly tensile) wings of bell-shaped curve
        v
            shape of the left (predominatly compressive) wings of bell-shaped curve
    Returns
    -------
        a
            normalized stress amplitude
    """
    a = f * (1 - m) ** u * (c + m) ** v
    return a


def get_normalized_stress_amplitude_for_n(
    cycles_to_failure: float,
    a1: float,
    b1: float,
    a2: float,
    b2: float,
    a3: float,
    b3: float,
    m: float,
    c: float,
    uts: float,
) -> float:
    """
    Get the normalized alternating stress component for given N
    Parameters
    ----------
        cycles_to_failure
            Number of cycles to failure (N)
        a1
            f slope
        b1
            f intercept
        a2
            u slope
        b2
            u intercept
        a3
            v slope
        b3
            v intercept
        m
            normalized mean stress (=sigma_a / UTS)
        c
            normalized compression strength (UCS / UTS)
        uts
            Ultimate tensile stress
    Returns
    -------
        a
            normalized stress amplitude
    """
    # Eq 4.11 p109 [1]
    f = a1 * np.log10(cycles_to_failure) + b1
    # Eq 4.12 p109 [1]
    u = a2 * np.log10(cycles_to_failure) + b2
    # Eq 4.13 p109 [1]
    v = a3 * np.log10(cycles_to_failure) + b3

    a = (
        get_normalized_stress_amplitude(
            m,
            c,
            f,
            u,
            v,
        )
        * uts
    )

    return a


def calculate_damage(
    stress_mean,
    stress_ratio,
    n_cycles,
    factor: float,
    a1: float,
    b1: float,
    a2: float,
    b2: float,
    a3: float,
    b3: float,
    ucs: float,
    uts: float,
) -> float:
    """
    Calculate damage
    Parameters
    ----------
        stress_mean: Series
            CYC input Dataframe
        stress_ratio: Series
            Stress ratio (R)
        n_cycles: Series
            Number of cycles (n)
        factor
            load cycle estimate factor
        a1
            f slope
        b1
            f intercept
        a2
            u slope
        b2
            u intercept
        a3
            v slope
        b3
            v intercept
        ucs
            Ultimate compressive stress
        uts
            Ultimate tensile stress
    Returns
    -------
        damage: Series[]
            damage for each row of CYC input file
    """
    stress_amplitude = stress_mean * factor * (1 - stress_ratio) / (1 + stress_ratio)

    m = stress_mean * factor / uts
    c = ucs / uts

    # Find the number of cycles to failure N in a defined range (a < N < b)
    # so as to obtain a normalized stress amplitude very close to stress_amplitude
    cycles_to_failure = optimize.bisect(
        lambda x: get_normalized_stress_amplitude_for_n(
            x,
            a1,
            b1,
            a2,
            b2,
            a3,
            b3,
            m,
            c,
            uts,
        )
        - stress_amplitude,
        a=1,
        b=10e25,
        rtol=0.0001,  # type: ignore
    )
    damage = n_cycles / cycles_to_failure
    return damage


def execute(
    input_snc_file: FilePath | ReadCsvBuffer,
    input_cyc_file: FilePath | ReadCsvBuffer,
    output_csv_file: FilePath | WriteBuffer,
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
        input_snc_file
            SNC input file
        input_cyc_file
            CYC input file
        output_csv_file
            DAS utput file
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

    SNC_df["y"] = np.log10(SNC_df.stress_amplitude / uts)
    SNC_df["x1"] = np.log10(1 - (SNC_df.stress_mean / uts))
    SNC_df["x2"] = np.log10(ucs / uts + SNC_df.stress_mean / uts)

    cycles_df = pd.DataFrame(
        SNC_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

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
                linregress_f.slope,  # type: ignore
                linregress_f.intercept,  # type: ignore
                linregress_u.slope,  # type: ignore
                linregress_u.intercept,  # type: ignore
                linregress_v.slope,  # type: ignore
                linregress_v.intercept,  # type: ignore
                ucs,
                uts,
            ),
            axis=1,
        )

        CYC_df["stress_max"] = (CYC_df.stress_mean + CYC_df.stress_range / 2) * factor

        factors.loc[factors.factor == factor, "damage"] = 1 / CYC_df.damage.sum()
        factors.loc[factors.factor == factor, "stress_max"] = CYC_df.stress_max.max()

    # Generate output file
    factors[["stress_max", "damage"]].to_csv(
        output_csv_file,  # type: ignore
        index=False,
    )

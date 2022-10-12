#!/usr/bin/env python
""" CCFatigue - Module 3 - CLD-Harris.py
This code takes in SNC data and outputs CLD data

Harris diagram is described in Tassos red book p. 108 - the original paper is:

[1] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[2] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.modules.harris_utils as harris_utils

# staticvalue.txt => constants
# ucs = 27.1
# UTS = 27.7

# BOUNDS_MARGIN = 0.99

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def cld_add_row(
    cld_df, cycles_to_failure: float, stress_amplitude: float, stress_mean: float
) -> pd.DataFrame:
    """
    Add row to CLD DataFrame
    Parameters
    ----------
        cld_df
            CLD DataFrame
        cycles_to_failure: float
        stress_amplitude: float
        stress_mean: float
    Returns
    -------
        CLD DataFrame
    """
    row = pd.DataFrame(
        {
            "cycles_to_failure": [cycles_to_failure],
            "stress_amplitude": [stress_amplitude],
            "stress_mean": [stress_mean],
        }
    )

    cld_df = pd.concat(
        [
            cld_df,
            row,
        ],
        ignore_index=True,
    )
    return cld_df


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_csv_file: FilePath | WriteBuffer,
) -> None:
    """
    Execute the CLD Harris algorithm
    Parameters
    ----------
        input_file: FilePath | ReadBuffer
            SNC input file
        output_csv_file: FilePath | WriteBuffer
            CLD csv file
    Returns
    -------
        None
    """

    ucs = harris_utils.UCS
    uts = harris_utils.UTS
    bounds_margin = harris_utils.BOUNDS_MARGIN

    # Import input files (SNC format)
    snc_df = pd.read_csv(input_file)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    snc_df["stress_ratio_id"] = (
        snc_df.stress_ratio != snc_df.stress_ratio.shift()
    ).cumsum()

    # Average by stress_ratio
    stress_ratios_df = (
        snc_df[
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
    snc_df["stress_amplitude"] = snc_df.apply(
        lambda x: harris_utils.calculate_stress_amplitude(x.stress_ratio, x.stress_max),
        axis=1,
    )

    # Calculate mean sigma
    snc_df["stress_mean"] = snc_df.apply(
        lambda x: harris_utils.calculate_stress_mean(x.stress_ratio, x.stress_max),
        axis=1,
    )

    # Apply bounds to mean sigma (applied to module 3, not to module 5)
    snc_df["stress_mean"] = snc_df.apply(
        lambda x: harris_utils.bounds_stress_mean(
            x.stress_mean,
            uts,
            ucs,
            bounds_margin,
        ),
        axis=1,
    )

    snc_df["y"] = np.log10(snc_df.stress_amplitude / uts)
    snc_df["x1"] = np.log10(1 - (snc_df.stress_mean / uts))
    snc_df["x2"] = np.log10(ucs / uts + snc_df.stress_mean / uts)

    cycles_df = pd.DataFrame(
        snc_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

    # Log10(N)
    cycles_df["log10_cycles_to_failure"] = np.log10(cycles_df.cycles_to_failure)

    for cycles_count in snc_df.cycles_to_failure.unique():

        cycle_df = snc_df.loc[snc_df.cycles_to_failure == cycles_count]

        f, u, v = harris_utils.calculate_fuv(
            cycle_df.x1.to_numpy(), cycle_df.x2.to_numpy(), cycle_df.y.to_numpy()
        )

        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "f"] = f
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "u"] = u
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "v"] = v

    # Linear Regression to calculate A and B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)

    cld_df = pd.DataFrame()

    for cycles_to_failure in CLD_CYCLES_COUNT:

        # Eq 8 p530 ref[1]
        ff = linregress_f.slope * np.log10(cycles_to_failure) + linregress_f.intercept  # type: ignore # noqa
        uu = linregress_u.slope * np.log10(cycles_to_failure) + linregress_u.intercept  # type: ignore # noqa
        vv = linregress_v.slope * np.log10(cycles_to_failure) + linregress_v.intercept  # type: ignore # noqa

        cld_df = cld_add_row(
            cld_df=cld_df,
            cycles_to_failure=cycles_to_failure,
            stress_amplitude=0,
            stress_mean=-ucs,
        )

        increment = 0.90 * (uts + ucs) / 40
        start = -0.90 * ucs
        stop = 0.90 * uts + increment
        for sm in np.arange(
            start,
            stop,
            increment,
        ):

            # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L192
            c2 = ff * (1 - sm / uts) ** uu
            c3 = ((ucs / uts) + (sm / uts)) ** vv

            stress_amplitude = c2 * c3 * uts
            stress_mean = sm

            cld_df = cld_add_row(
                cld_df=cld_df,
                cycles_to_failure=cycles_to_failure,
                stress_amplitude=stress_amplitude,
                stress_mean=stress_mean,
            )

        cld_df = cld_add_row(
            cld_df=cld_df,
            cycles_to_failure=cycles_to_failure,
            stress_amplitude=0,
            stress_mean=uts,
        )

    # Generate output files
    cld_df.to_csv(path_or_buf=output_csv_file, index=False)  # type: ignore

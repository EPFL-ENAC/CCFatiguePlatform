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
from pandas._typing import ReadCsvBuffer, WriteBuffer, FilePath
from scipy import stats
import ccfatigue.modules.harris_utils as harris_utils


# staticvalue.txt => constants
# ucs = 27.1
# UTS = 27.7

# BOUNDS_MARGIN = 0.99

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


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
    SNC_df = pd.read_csv(input_file)

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

    # Apply bounds to mean sigma (applied to module 3, not to module 5)
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: harris_utils.bounds_stress_mean(
            x.stress_mean,
            uts,
            ucs,
            bounds_margin,
        ),
        axis=1,
    )

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

    # Linear Regression to calculate A and B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)

    CLD_df = pd.DataFrame()

    for onc in CLD_CYCLES_COUNT:

        # Eq 8 p530 ref[1]
        ff = linregress_f.slope * np.log10(onc) + linregress_f.intercept  # type: ignore # noqa
        uu = linregress_u.slope * np.log10(onc) + linregress_u.intercept  # type: ignore # noqa
        vv = linregress_v.slope * np.log10(onc) + linregress_v.intercept  # type: ignore # noqa

        for sm in np.arange(
            -0.90 * ucs,
            0.90 * uts,
            0.90 * (uts + ucs) / 40,
        ):

            C2 = ff * (1 - sm / uts) ** uu
            C3 = ((ucs / uts) + (sm / uts)) ** vv

            stress_amplitude = C2 * C3 * uts
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

    # Generate output files
    CLD_df.to_csv(path_or_buf=output_csv_file, index=False)  # type: ignore

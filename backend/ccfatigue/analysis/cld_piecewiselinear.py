#!/usr/bin/env python
"""
Implementation of the Piecewise Linear Methods as described in:
Anastasios P. Vassilopoulos, Behzad D. Manshadi, Thomas Keller,
Influence of the constant life diagram formulation on the fatigue life prediction of
composite materials,
International Journal of Fatigue,
Volume 32, Issue 4, 2010, Pages 659-669, ISSN 0142-1123,
[1] https://doi.org/10.1016/j.ijfatigue.2009.09.008
    (Section 2.2)
[2] Fatigue of Fiber-reinforced Composites
    4.3.1.2 Piecewise Linear CLD (pp 107-108)
    DOI 10.1007/978-1-84996-181-3
    https://link.springer.com/content/pdf/10.1007/978-1-84996-181-3.pdf
"""

import math
import os

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.analysis.cld_utils as cld_utils
import ccfatigue.analysis.piecewiselinear_utils as piecewiselinear_utils

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_FILENAME = "SNC_input.csv"
SNC_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7
CRITICAL_STRESS_RATIO = 0.1

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    """
    Execute the CLD Harris algorithm
    Parameters
    ----------
        snc_csv_input_file
            SNC csv input file
        cld_csv_output_file
            CLD csv output file
        ucs
            Ultimate compressive stress
        uts
            Ultimate tensile stress
    Returns
    -------
        None
    """

    # Import input files (SNC format)
    snc_df = pd.read_csv(snc_csv_input_file)

    # Calculate stress amplitude (sigma_a) for each known stress ratios
    snc_df["stress_amplitude"] = snc_df.apply(
        lambda x: piecewiselinear_utils.calculate_stress_amplitude(
            x.stress_ratio, x.stress_max
        ),
        axis=1,
    )

    # Get stress ratio (R)
    stress_ratios_df = pd.DataFrame({"stress_ratio": snc_df.stress_ratio.unique()})

    # Sort R according to sectors, counterclockwise
    piecewiselinear_utils.sort_by_stress_ratios(stress_ratios_df)

    # Calculate slope A and intercept B (linear regression) for each group of R
    linregress = snc_df.groupby("stress_ratio").apply(
        lambda x: stats.linregress(
            np.log10(x.stress_amplitude),  # type: ignore
            np.log10(x.cycles_to_failure),  # type: ignore
        )
    )

    # Set stress_ratio as index for linregress groupby import
    stress_ratios_df.set_index(["stress_ratio"], inplace=True)
    stress_ratios_df["slope"] = linregress.apply(lambda x: x[1])
    stress_ratios_df["intercept"] = linregress.apply(lambda x: x[0])

    # Remove index for stress_ratios_df.stress_ratio usage
    stress_ratios_df.reset_index(["stress_ratio"], inplace=True)

    # Create output df
    cld_df = pd.DataFrame()

    for cycles_to_failure in CLD_CYCLES_COUNT:

        cld_df = cld_utils.cld_add_row(
            cld_df,
            cycles_to_failure,
            0,
            uts,
        )

        # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/cde13599121bceb95d579adfe3e56056ba622d60/CCFatigue_modules/3_CLD/Piecewise-Linear/CLD-Piecewise-Linear.for#L241
        # Eq not found in doc
        stress_ratios_df["stress_amplitude"] = 10 ** (
            -(stress_ratios_df.slope / stress_ratios_df.intercept)
            + 1 / stress_ratios_df.intercept * math.log10(cycles_to_failure)
        )

        # Eq 1 p661
        stress_ratios_df["stress_mean"] = (
            (1 + stress_ratios_df.stress_ratio)
            * stress_ratios_df.stress_amplitude
            / (1 - stress_ratios_df.stress_ratio)
        )

        for index, stress_ratio in stress_ratios_df.iterrows():

            cld_df = cld_utils.cld_add_row(
                cld_df,
                cycles_to_failure,
                stress_ratio.stress_amplitude,
                stress_ratio.stress_mean,
            )

        cld_df = cld_utils.cld_add_row(
            cld_df,
            cycles_to_failure,
            0,
            -ucs,
        )

    # Generate output files
    cld_df.to_csv(path_or_buf=cld_csv_output_file, index=False)  # type: ignore

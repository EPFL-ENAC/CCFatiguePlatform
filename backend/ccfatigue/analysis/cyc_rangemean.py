#!/usr/bin/env python
"""
CCFatigue - Module 1 - cyc_range_mean.py
This file is a translation from fortran Cycle-Counting-Range-Mean.for
"""

import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cyc as cyc

MATRIX_SIZE = 64


def execute(
    lds_input_csv_file: FilePath | ReadCsvBuffer,
    cyc_output_csv_file: FilePath | WriteBuffer,
) -> None:
    """
    Execute the LinLog algorithm
    Parameters
    ----------
        lds_input_csv_file: FilePath | ReadBuffer
            LDS input CSV file
        cyc_output_csv_file: FilePath | WriteBuffer
            CYC output CSV file
    Returns
    -------
        None
    """

    peak = []
    ranges = []

    lds_df = pd.read_csv(lds_input_csv_file)

    # Simple Range Counting
    # ##########################################################################

    for stress_max in lds_df.stress_max.to_list():
        peak.append(stress_max)
        if len(peak) < 2:
            continue

        y = abs(peak[-1] - peak[-2])
        _mean = (peak[-2] + peak[-1]) / 2
        _range = y
        ranges.append(cyc.CycRangeRow(_range, _mean, peak[-2], peak[-1], 1))

    # Create Markov Matrix then CYC dataframe
    cyc_df = pd.DataFrame(cyc.ranges2cyc(MATRIX_SIZE, ranges))

    # Generate output files
    cyc_df.to_csv(path_or_buf=cyc_output_csv_file, index=False)  # type: ignore
    pass

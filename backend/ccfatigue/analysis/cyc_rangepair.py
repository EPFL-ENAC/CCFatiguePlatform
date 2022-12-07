#!/usr/bin/env python
"""
Cycle counting range pair algorithm
This file is a translation from fortran Cycle-Counting-Range-Pair.for
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
    Execute the Cycle Counting Range pair algorithm
    Parameters
    ----------
        lds_input_csv_file: FilePath | ReadCsvBuffer
            LDS input CSV file
        cyc_output_csv_file: FilePath | WriteBuffer
            CYC output CSV file
    Returns
    -------
        None
    """
    peaks = []
    ranges = []

    lds_df = pd.read_csv(lds_input_csv_file)

    # Range-Pair Counting
    # ##########################################################################

    for stress_max in lds_df.stress_max.to_list():
        peaks.append(stress_max)
        while len(peaks) >= 3:

            x = abs(peaks[-1] - peaks[-2])
            y = abs(peaks[-2] - peaks[-3])
            if x < y:
                break  # exit while

            _mean = (peaks[-2] + peaks[-3]) / 2
            _range = y
            ranges.append(cyc.CycRangeRow(_range, _mean, peaks[-3], peaks[-2], 2))
            peaks.pop(-2)
            peaks.pop(-2)

    # Create Markov Matrix then CYC dataframe
    cyc_df = pd.DataFrame(cyc.ranges2cyc(MATRIX_SIZE, ranges))

    # Generate output files
    cyc_df.to_csv(path_or_buf=cyc_output_csv_file, index=False)  # type: ignore
    pass

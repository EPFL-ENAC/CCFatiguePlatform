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
    
    # ---------------------------------------------------------------------------
    # End-of-data residual handling (custom rule)
    # If 3 points remain, keep the smaller adjacent range as a full cycle.
    # If 2 points remain, count it as a half-cycle.
    # ---------------------------------------------------------------------------
    if len(peaks) == 3:
        p0, p1, p2 = peaks[0], peaks[1], peaks[2]
        y = abs(p1 - p0)  # older adjacent range
        x = abs(p2 - p1)  # last adjacent range

        # keep the smaller one
        if x <= y:
            r_range = x
            r_mean = (p1 + p2) / 2
            peak1, peak2 = p1, p2
        else:
            r_range = y
            r_mean = (p0 + p1) / 2
            peak1, peak2 = p0, p1

        # full cycle => n_cycles=2 (will become 1.0 after /2)
        ranges.append(cyc.CycRangeRow(r_range, r_mean, peak1, peak2, 2))

    elif len(peaks) == 2:
        p0, p1 = peaks[0], peaks[1]
        r_range = abs(p1 - p0)
        r_mean = (p0 + p1) / 2

        # half cycle => n_cycles=1 (will become 0.5 after /2)
        ranges.append(cyc.CycRangeRow(r_range, r_mean, p0, p1, 1))


    # Create Markov Matrix then CYC dataframe
    cyc_df = pd.DataFrame(cyc.ranges2cyc(MATRIX_SIZE, ranges))

    # Generate output files
    cyc_df.to_csv(path_or_buf=cyc_output_csv_file, index=False)  # type: ignore
    pass

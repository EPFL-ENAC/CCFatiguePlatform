#!/usr/bin/env python
"""
Simplified Rainflow Cycle Counting
This module takes in LDS and outputs CYC
Translation from fortran Cycle-Counting-Simplified-Rainflow.for
Algorithm described in "Simple rainflow counting algorithms"
[1] https://www.sciencedirect.com/science/article/pii/0142112382900184
    https://doi.org/10.1016/0142-1123(82)90018-4
 """

import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cyc as cyc

MATRIX_SIZE = 64


def rearrange_peaks(peaks):
    """
    Rearrange peaks and valleys to begin and end with maximum peak
    Parameters
    ----------
        peaks
            List of peaks/valleys
    Returns
    -------
        rearranged_peaks
    """
    # If first and last peaks are equals, remove last
    if peaks.iloc[0] == peaks.iloc[-1]:
        peaks = peaks[:-1]

    # Find maximum peak position
    pos_max_peak = peaks.idxmax()

    rearranged_peaks = pd.concat(
        [peaks.iloc[pos_max_peak:], peaks.iloc[: pos_max_peak + 1]]
    )

    return rearranged_peaks


def execute(
    lds_input_csv_file: FilePath | ReadCsvBuffer,
    cyc_csv_output_file: FilePath | WriteBuffer,
) -> None:
    """
    Execute the Simplified Rainflow Cycle Counting algorithm
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

    lds_df = pd.read_csv(lds_input_csv_file)

    rearranged_peaks = rearrange_peaks(lds_df.stress_max)

    # RAINFLOW ALGORITHM I as describe in ref [1] p32
    # 1 - Read the next peak or valley
    #     (if out of data, STOP)
    # 2 - Form ranges X and Y
    #     (if the vector contains less than 3 points, go to Step 1)
    # 3 - Compare ranges X and Y
    #     a. If X < Y, goto step 1
    #     b. If X >= Y, goto step 4
    # 4 - Count range Y
    #     Discard the peak and valley of Y
    #     Go to Step 2

    peaks = []  # Stack of peaks and valleys
    ranges = []  # List of ranges

    for p in rearranged_peaks:
        peaks.append(p)

        while len(peaks) >= 3:
            x = abs(peaks[-1] - peaks[-2])
            y = abs(peaks[-2] - peaks[-3])
            if x < y:
                break

            _mean = (peaks[-2] + peaks[-3]) / 2
            _range = y
            ranges.append(cyc.CycRangeRow(_range, _mean, peaks[-3], peaks[-2], 2))
            peaks.pop(-2)
            peaks.pop(-2)

    # Create Markov Matrix then CYC dataframe
    cyc_df = pd.DataFrame(cyc.ranges2cyc(MATRIX_SIZE, ranges))

    # Generate output files
    cyc_df.to_csv(path_or_buf=cyc_csv_output_file, index=False)  # type: ignore
    pass

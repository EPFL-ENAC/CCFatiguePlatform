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

from ccfatigue.analysis.utils.cyc import FlowRow


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

    peaks = []
    flows = []

    lds_df = pd.read_csv(lds_input_csv_file)

    rearranged_peaks = rearrange_peaks(lds_df.stress_max)

    peaks = []  # Stack of peaks and valleys

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

    for p in rearranged_peaks:
        peaks.append(p)

        while len(peaks) >= 3:
            x = abs(peaks[-1] - peaks[-2])
            y = abs(peaks[-2] - peaks[-3])
            if x < y:
                break

            _mean = (peaks[-2] + peaks[-3]) / 2
            _range = y
            flows.append(FlowRow(_range, _mean, peaks[-3], peaks[-2], 2))
            peaks.pop(-2)
            peaks.pop(-2)

    # Creating 64*64 markov matrix
    # ##########################################################################

    maxrange = max(flows, key=lambda p: p.range).range
    minrange = min(flows, key=lambda p: p.range).range
    maxmean = max(flows, key=lambda p: p.mean).mean
    minmean = min(flows, key=lambda p: p.mean).mean

    matrixsize = 64
    deltarange = (maxrange - minrange) / matrixsize
    deltamean = (maxmean - minmean) / matrixsize
    sumcum = 0.0
    cum = 0

    # init cc[matrixsize * matrixsize] filled with 0
    n_cycles = [[0 for i in range(matrixsize)] for j in range(matrixsize)]

    for flow in flows:

        i = min(int((flow.range - minrange) / deltarange), matrixsize - 1)
        j = min(int((flow.mean - minmean) / deltamean), matrixsize - 1)
        n_cycles[i][j] += flow.n_cycles

    cyc_df = pd.DataFrame()

    for i in range(matrixsize):
        for j in range(matrixsize):
            sumcum += n_cycles[i][j]

    for i in range(matrixsize):
        for j in range(matrixsize):
            if n_cycles[i][j] != 0:
                downra = minrange + i * deltarange
                upra = minrange + (i + 1) * deltarange
                stress_range = (upra + downra) / 2
                downme = minmean + j * deltamean
                upme = minmean + (j + 1) * deltamean
                stress_mean = (upme + downme) / 2
                stress_ratio = -1 + (4 * stress_mean) / (2 * stress_mean + stress_range)
                cum += n_cycles[i][j]
                cum_n_cycles = abs(sumcum - cum) * 100 / sumcum

                row = pd.DataFrame(
                    {
                        "stress_range": [stress_range],
                        "stress_mean": [stress_mean],
                        "stress_ratio": [stress_ratio],
                        "n_cycles": [n_cycles[i][j] / 2],
                        "cum_n_cycles": [cum_n_cycles],
                    }
                )

                cyc_df = pd.concat(
                    [
                        cyc_df,
                        row,
                    ],
                    ignore_index=True,
                )

    # Generate output files
    cyc_df.to_csv(path_or_buf=cyc_csv_output_file, index=False)  # type: ignore
    pass

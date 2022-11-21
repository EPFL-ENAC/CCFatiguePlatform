#!/usr/bin/env python
"""
CCFatigue - Module 1 - cyc_range_mean.py
This file is a translation from fortran Cycle-Counting-Range-Mean.for
"""

import pandas as pd
from pandas._typing import FilePath, ReadBuffer, WriteBuffer


class FlowRow:
    """
    Row of a Flow table
    """

    def __init__(
        self, _range: float, _mean: float, peak1: float, peak2: float, n_cycles: float
    ):
        self.range = _range
        self.mean = _mean
        self.peak1 = peak1
        self.peak2 = peak2
        self.n_cycles = n_cycles


def execute(
    lds_input_csv_file: FilePath | ReadBuffer,
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
    flows = []

    lds_df = pd.read_csv(lds_input_csv_file)  # type: ignore

    # Simple Range Counting
    # ##########################################################################

    for stress_max in lds_df.stress_max.to_list():
        peak.append(stress_max)
        if len(peak) < 2:
            continue

        y = abs(peak[-1] - peak[-2])
        _mean = (peak[-2] + peak[-1]) / 2
        _range = y
        flows.append(FlowRow(_range, _mean, peak[-2], peak[-1], 1))

    # Sorting output file
    # ##########################################################################
    flows.sort(key=lambda x: x.range)

    maxrange = max(flows, key=lambda p: p.range).range
    minrange = min(flows, key=lambda p: p.range).range
    maxmean = max(flows, key=lambda p: p.mean).mean
    minmean = min(flows, key=lambda p: p.mean).mean

    matrixsize = 64
    deltarange = abs(maxrange - minrange) / matrixsize
    deltamean = abs(maxmean - minmean) / matrixsize
    sumcum = 0.0
    cum = 0

    # init cc[matrixsize * matrixsize] filled with 0
    n_cycles = [[0 for i in range(matrixsize)] for j in range(matrixsize)]

    for k in range(len(flows)):
        for i in range(matrixsize):

            if flows[k].range >= minrange + i * deltarange - deltarange / 100:
                if flows[k].range <= minrange + (i + 1) * deltarange + deltarange / 100:

                    for j in range(matrixsize):
                        if flows[k].mean >= minmean + j * deltamean - deltamean / 100:
                            if (
                                flows[k].mean
                                <= minmean + (j + 1) * deltamean + deltamean / 100
                            ):

                                n_cycles[i][j] += flows[k].n_cycles
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
    cyc_df.to_csv(path_or_buf=cyc_output_csv_file, index=False)  # type: ignore
    pass

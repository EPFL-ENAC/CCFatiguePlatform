#!/usr/bin/env python
"""
CCFatigue - Module 1 - Cycle counting - Common utils
"""

import ccfatigue.analysis.utils.cyc as cyc


class CycRangeRow:
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


def ranges2cyc(matrix_size: int, ranges: list[cyc.CycRangeRow]):
    """
    Fill a markov matrix with all ranges then outputs CYC
    Parameters
    ----------
        matrix_size: int
            Markov matrix size (usually 64x64)
        ranges
            List of ranges (range = peak - valley)
    Returns
    -------
        cyc: list[{
                "stress_range": stress_range,
                "stress_mean": stress_mean,
                "stress_ratio": stress_ratio,
                "n_cycles": n_cycles,
                "cum_n_cycles": cum_n_cycles,
            }]
    """

    maxrange = max(ranges, key=lambda p: p.range).range
    minrange = min(ranges, key=lambda p: p.range).range
    maxmean = max(ranges, key=lambda p: p.mean).mean
    minmean = min(ranges, key=lambda p: p.mean).mean

    deltarange = (maxrange - minrange) / matrix_size
    deltamean = (maxmean - minmean) / matrix_size
    total_n_cycles = 0.0
    cumulated_n_cycles = 0

    # Creating 64*64 markov matrix
    n_cycles: list[list[float]] = [
        [0 for i in range(matrix_size)] for j in range(matrix_size)
    ]

    # Fill matrix with ranges (and count sum(n_cycles))
    for _range in ranges:
        i = min(int((_range.range - minrange) / deltarange), matrix_size - 1)
        j = min(int((_range.mean - minmean) / deltamean), matrix_size - 1)
        n_cycles[i][j] += _range.n_cycles
        total_n_cycles += _range.n_cycles

    cyc = []

    # Add each matrix cell with data into CYC list
    for i in range(matrix_size):
        for j in range(matrix_size):
            if n_cycles[i][j] != 0:
                downra = minrange + i * deltarange
                upra = minrange + (i + 1) * deltarange
                stress_range = (upra + downra) / 2
                downme = minmean + j * deltamean
                upme = minmean + (j + 1) * deltamean
                stress_mean = (upme + downme) / 2
                stress_ratio = -1 + (4 * stress_mean) / (2 * stress_mean + stress_range)
                cumulated_n_cycles += n_cycles[i][j]
                cum_n_cycles = (
                    abs(total_n_cycles - cumulated_n_cycles) * 100 / total_n_cycles
                )

                cyc.append(
                    {
                        "stress_range": stress_range,
                        "stress_mean": stress_mean,
                        "stress_ratio": stress_ratio,
                        "n_cycles": n_cycles[i][j] / 2,
                        "cum_n_cycles": cum_n_cycles,
                    }
                )
    return cyc

#!/usr/bin/env python
"""
CCFatigue - Module 1 - Cycle counting - Common utils
"""


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

    def __str__(self):
        return "{:10.4f} {:10.4f} {:10.4f} {:10.4f} {}".format(
            self.range, self.mean, self.peak1, self.peak2, self.n_cycles
        )

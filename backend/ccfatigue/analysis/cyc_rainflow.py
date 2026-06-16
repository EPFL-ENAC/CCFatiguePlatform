#!/usr/bin/env python
"""
ASTM E1049 Section 5.4.4 Rainflow Cycle Counting
This module takes in LDS and outputs CYC
"""

import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cyc as cyc

MATRIX_SIZE = 64


def _rainflow(peaks: list[float]) -> list[cyc.CycRangeRow]:
    """
    ASTM E1049-85 Section 5.4.4 Rainflow Counting Algorithm.

    Rules (S = starting point of current history):
    1. Read next peak or valley. If out of data, go to Step 6.
    2. If < 3 points, go to Step 1. Form ranges X and Y from 3 most recent points.
    3. Compare |X| and |Y|:
       (a) X < Y  → go to Step 1.
       (b) X ≥ Y  → go to Step 4.
    4. If Y contains S → Step 5. Otherwise count Y as 1 full cycle, discard
       both endpoints of Y, go to Step 2.
    5. Count Y as 1 half-cycle; discard first point of Y; advance S; go to Step 2.
    6. Count each remaining adjacent range as 1 half-cycle.

    n_cycles encoding for ranges2cyc (which divides by 2):
      full cycle  → n_cycles = 2  (output 1.0)
      half cycle  → n_cycles = 1  (output 0.5)
    """
    stack: list[float] = []
    ranges: list[cyc.CycRangeRow] = []

    for p in peaks:
        stack.append(p)  # Step 1

        while True:
            if len(stack) < 3:
                break  # Step 1

            X = abs(stack[-1] - stack[-2])
            Y = abs(stack[-2] - stack[-3])

            if X < Y:  # Step 3(a)
                break  # Step 1

            # Step 3(b): X >= Y
            y_range = Y
            y_mean = (stack[-3] + stack[-2]) / 2

            if len(stack) == 3:
                # Step 5: Y contains S — half cycle, advance S
                ranges.append(cyc.CycRangeRow(y_range, y_mean, stack[-3], stack[-2], 1))
                stack.pop(0)
            else:
                # Step 4: full cycle, discard both endpoints of Y
                ranges.append(cyc.CycRangeRow(y_range, y_mean, stack[-3], stack[-2], 2))
                del stack[-3:-1]

    # Step 6: remaining adjacent pairs → half cycles
    for k in range(len(stack) - 1):
        r = abs(stack[k + 1] - stack[k])
        m = (stack[k] + stack[k + 1]) / 2
        ranges.append(cyc.CycRangeRow(r, m, stack[k], stack[k + 1], 1))

    return ranges


def execute(
    lds_input_csv_file: FilePath | ReadCsvBuffer,
    cyc_csv_output_file: FilePath | WriteBuffer,
) -> None:
    lds_df = pd.read_csv(lds_input_csv_file)
    ranges = _rainflow(lds_df.stress_max.to_list())
    cyc_df = pd.DataFrame(cyc.ranges2cyc(MATRIX_SIZE, ranges))
    cyc_df.to_csv(path_or_buf=cyc_csv_output_file, index=False)

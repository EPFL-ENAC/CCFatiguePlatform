#!/usr/bin/env python
"""
Compare output files (different formats but same values) from fortran and python module
 """

__author__ = "Nicolas Dubois"

import sys
import pandas as pd
import numpy as np

filename1 = sys.argv[1]
filename2 = sys.argv[2]

df1 = pd.read_table(filename1, sep="\s+", header=None)
df2 = pd.read_table(filename2, sep="\s+", header=None)

if len(df1.columns) != len(df2.columns):
    print("Not same columns")
    exit()

if np.allclose(df1, df2):
    print("Files are same (or very close)")

close = np.isclose(df1, df2)

for (i, cols) in enumerate(close):
    if not all(cols):
        print(f"Different at line {i + 1}, isclose={cols}")

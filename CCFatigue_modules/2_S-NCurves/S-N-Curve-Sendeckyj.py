#!/usr/bin/env python
"""
G. P. Sendeckyj, Fitting Models to Composite Materials Fatigue Data.
ASTM STP 734, ed. By C.C.Chamis, American Society for Testing and Materials,
West Conshohocken, 1981, pp 245-260. 
DOI:10.1520/STP29314S

sigma_a: maximum applied cyclic stress (see ref, p 248)
sigma_r: residual strength (see ref, p 248)
sigma_e: equivalent static strength (see ref, p 248)
k: number of censored data in current stress ratio
m: number of data in current stress ratio
n: number of cycles (see ref, p 248)
s: absolute value of the asymptotic slope at long life on log-log plot
   of the S-N curve (see ref, p 248)
c: measure of the extent of the "flat" region on the S-N curve at high
   applied cyclic stress levels (see ref, p 248)
alpha: Weibull shape (see ref, p 248)
beta: Weibull scale (see ref, p 248)
g: ?? TODO

"""

import os

# import json
import numpy as np
import pandas as pd
from scipy import stats
import math


SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "data")
PARAM_DIR = os.path.join(SRC_DIR, "..", "ExternalDataSources")

INPUT_FILENAME = "AGG_input.csv"
INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_JSON_FILENAME = "SNC_Sendeckyj.json"
OUTPUT_JSON_FILE = os.path.join(DATA_DIR, OUTPUT_JSON_FILENAME)
OUTPUT_CSV_FILENAME = "SNC_Sendeckyj.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# C is a measure of the extent of the "flat" region on the S-N curve at high
#  applied cyclic stress levels (see ref, p 248)
C_MIN = 0.000001
C_MAX = 10
C_STEP = 0.000001

# S is the absolute value of the asymptotic slope at long life on log-log plot of the
# S-N curve (see ref, p 248)
S_MIN = 0.001
S_MAX = 0.5
S_STEP = 0.0005


def sendeckyj_equation_1(sigma_a, sigma_r, n, c, s):
    """Returns sigma_e = the equivalent static strength
    Eq 1 (see ref, p248)
    """
    sigma_e = sigma_a * ((sigma_r / sigma_a) ** (1 / s) + (n - 1) * c) ** s
    return sigma_e


def sendeckyj_equation_17(m, k, serie_of_sigme_e):
    """Returns G TODO G = ??

    Eq 17 (see ref, p259)
    """
    return math.exp(1 / (m - k)) * serie_of_sigme_e.apply(lambda x: math.log(x).sum())


# Import input file (SNC format)
df = pd.read_csv(INPUT_FILE)

for c in np.arange(C_MIN, C_MAX, C_STEP):
    for s in np.arange(S_MIN, S_MAX, S_STEP):

        # results = df.groupby("stress_ratio")
        # results[].co

        # Eq 1
        df["sigma_e"] = df.apply(
            lambda x: sendeckyj_equation_1(
                x.stress_parameter,
                x.residual_strength,
                x.number_of_cycles,
                c,
                s,
            ),
            axis=1,
        )

        # Eq 17
        # g = df.apply(lambda x: sendeckyj_equation_17(
        #     x))

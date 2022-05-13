#!/usr/bin/env python
""" CCFatigue - Module 2 - S-N-Curve-LogLog.py
This code takes in AGG data and output SNC data
It applies a linear regression on log(number of cycles) and log(stress ratios)
as described in https://www.astm.org/stp313-eb.html pp. 20-23
DOI: 10.1520/STP313-EB
TODO
"""

import math
import os
from re import A
from statistics import variance
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

import Astm


CONFIDENCE = 95  # TODO check with Tassos if this should be a parameter (95, 99)
RELIABILITY_LEVEL = 50

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
# PARAM_DIR = os.path.join(SRC_DIR, "..", "ExternalDataSources")

# PARAM_ASTM_FILENAME = "astm.csv" # Used only in Tassos commented code
# PARAM_ASTM_RELIABILITY_LEVEL_FILENAME = f"astm{CONFIDENCE}.csv"

astm = Astm.Astm(CONFIDENCE)

INPUT_FILENAME = "AGG_input.csv"
INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_JSON_FILENAME = "SNC_LogLog.json"
OUTPUT_JSON_FILE = os.path.join(DATA_DIR, OUTPUT_JSON_FILENAME)
OUTPUT_CSV_FILENAME = "SNC_LogLog.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)


def equation_ycar(slope, intercept, log10_stress_parameter):
    """TODO"""
    ycar = slope + intercept * log10_stress_parameter
    return ycar


def stress_at_failure(aa, bb, nn):
    """TODO"""
    spn = aa * nn ** (-bb)
    return spn


def get_stress_at_failure(nod, q, a, b, nn, pp, xb):
    """Get Stress at failure (sigma_max)
    returns (lower bound, upper bound)"""

    termi = (nod * q * a * b) - (nod * q * math.log10(nn) * b) + (pp * nod * xb)
    termii = math.sqrt(
        nod
        * pp
        * q
        * (
            2 * nod * a * b * xb
            - 2 * nod * math.log10(nn) * b * xb
            + q * b**2
            + nod * b**2 * xb**2
            + nod * math.log10(nn) ** 2
            - 2 * nod * math.log10(nn) * a
            - pp
            + nod * a**2
        )
    )
    termiii = nod * (b**2 * q - pp)

    spnu = 10 ** (-(termi + termii) / termiii)
    spnl = 10 ** (-(termi - termii) / termiii)

    return (spnl, spnu)


# RROUT = 1e7

# Import ASTM files
# astm_df = pd.read_csv(os.path.join(PARAM_DIR, PARAM_ASTM_FILENAME))
# astm_rl_df = pd.read_csv(os.path.join(PARAM_DIR, PARAM_ASTM_RELIABILITY_LEVEL_FILENAME))

# Import input file (SNC format)
df = pd.read_csv(INPUT_FILE)

# Add columns with log10(stress_parameter) and log10(number_of_cycles)
df["log10_stress_parameter"] = np.log10(df.stress_parameter)
df["log10_number_of_cycles"] = np.log10(df.number_of_cycles)


# TODO: Check with Tassos if input file can have different section with same
# stress_ratio
df["stress_ratio_id"] = (df.stress_ratio != df.stress_ratio.shift()).cumsum()

# TODO: Check with Tassos if input file can have different section with same
# stress_level
df["stress_level_id"] = (df.stress_level != df.stress_level.shift()).cumsum()

# Average by stress_ratio
results = (
    df[
        [
            "stress_ratio_id",
            "stress_ratio",
            "log10_stress_parameter",
            "log10_number_of_cycles",
            "number_of_cycles",
        ]
    ]
    .groupby(["stress_ratio_id"])
    .mean()
)

cycles_to_failure = pd.DataFrame(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(3000000, 20000000, 1000000),
        range(30000000, 1400000000, 100000000),
    ),
    columns=["cycles_to_failure"],
)

snc_output_csv_df = pd.DataFrame(
    columns=[
        "stress_ratio",
        "cycles_to_failure",
        "stress_parameter",
        "stress_lowerbound",
        "stress_upperbound",
    ]
)

# Calculate A (Eq 4) and B (Eq 5)
linregress = df.groupby("stress_ratio_id").apply(
    lambda x: stats.linregress(x.log10_stress_parameter, x.log10_number_of_cycles)
)

# Add A and B in avg_by_stress_ratio
# Slope = A, Intercept = B
results["slope"] = linregress.apply(lambda x: x[1])
results["intercept"] = linregress.apply(lambda x: x[0])
# for stress_ratio_id in df.stress_ratio_id.unique():
#     df.loc[df.stress_ratio_id == stress_ratio_id, "slope"] = linregress[
#         stress_ratio_id
#     ][1]
#     df.loc[df.stress_ratio_id == stress_ratio_id, "intercept"] = linregress[
#         stress_ratio_id
#     ][0]

# Add AA and BB TODO reference for these equations
results["stress_parameter_aa"] = 10 ** (-results.slope / results.intercept)
results["stress_parameter_bb"] = -1 / results.intercept

df["ycar"] = df.apply(
    lambda x: equation_ycar(
        results.loc[x.stress_ratio_id].slope,
        results.loc[x.stress_ratio_id].intercept,
        x.log10_stress_parameter,
    ),
    axis=1,
)

# LNb
results["log10_number_of_cycles"] = (
    df[["stress_ratio_id", "log10_number_of_cycles"]].groupby("stress_ratio_id").mean()
)

# LSST
df["lsst"] = df.apply(
    lambda x: (
        x.log10_stress_parameter - results.loc[x.stress_ratio_id].log10_number_of_cycles
    )
    ** 2,
    axis=1,
)
results["lsst"] = df[["stress_ratio_id", "lsst"]].groupby("stress_ratio_id").sum()

# LSSE
df["lsse"] = (df.log10_number_of_cycles - df.ycar) ** 2
results["lsse"] = df[["stress_ratio_id", "lsse"]].groupby("stress_ratio_id").sum()

# LRSQ
results["lrsq"] = 1 - results.lsse / results.lsst

# Q
results["xb"] = (
    df[["stress_ratio_id", "log10_stress_parameter"]].groupby("stress_ratio_id").mean()
)
df["q"] = df.apply(
    lambda x: (x.log10_stress_parameter - results.loc[x.stress_ratio_id].xb) ** 2,
    axis=1,
)
results["q"] = df[["stress_ratio_id", "q"]].groupby("stress_ratio_id").sum()

# NOD
results["nod"] = (
    df[["stress_ratio_id", "stress_ratio"]].groupby("stress_ratio_id").count()
)

# level
results["level"] = (
    df[["stress_ratio_id", "stress_level"]].groupby("stress_ratio_id").nunique()
)


# Variance
results["variance"] = results.apply(lambda x: math.sqrt(x.lsse / (x.nod - 2)), axis=1)


# Fp
results["fp"] = results.apply(
    # lambda x: astm_rl_df.loc[astm_rl_df.n == x.nod - x.level][str(int(x.level - 2))],
    lambda x: astm.get_astm_val(x.level - 2, x.nod - x.level),
    axis=1,
)

# PP
results["pp"] = 2 * results.fp * results.variance**2


# Prepare SNC output list of cycles to failure

snc_output_json_df = pd.DataFrame(
    columns=[
        "stress_ratio",
        "RSQL",
        "A",
        "B",
        "Fp",
        "Linearity",
        "Sstar",
        "Cstar",
    ],
)

snc_output_csv_df = pd.DataFrame(
    columns=[
        "stress_ratio",
        "cycles_to_failure",
        "stress_parameter",
        "stress_lowerbound",
        "stress_upperbound",
    ]
)

# For each group of stress_ratio
for stress_ratio_id in df["stress_ratio_id"].unique():

    stress_ratio_df = df.loc[df.stress_ratio_id == stress_ratio_id]
    stress_ratio = stress_ratio_df.iloc[0].stress_ratio
    result = results.loc[stress_ratio_id]

    stress_parameter = cycles_to_failure.copy()
    stress_parameter["stress_parameter"] = stress_parameter.apply(
        lambda x: stress_at_failure(
            result.stress_parameter_aa, result.stress_parameter_bb, x
        )
    )

    stress_bounds = stress_parameter.apply(
        lambda x: get_stress_at_failure(
            result.nod,
            result.q,
            result.slope,
            result.intercept,
            x.cycles_to_failure,
            result.pp,
            result.xb,
        ),
        axis=1,
    )
    stress_parameter["spnu"] = stress_bounds.apply(lambda x: x[1])
    stress_parameter["spnl"] = stress_bounds.apply(lambda x: x[0])

    stress_parameter["stress_ratio"] = stress_ratio

    snc_output_csv_df = pd.concat([snc_output_csv_df, stress_parameter])

#     json_df = pd.DataFrame(
#         {
#             "stress_ratio": stress_ratio,
#             "RSQL": RELIABILITY_LEVEL,
#             "A": results.stress_parameter_aa,
#             "B": results.stress_parameter_bb,
#             "Sstar": results[stress_ratio_id].lrsq,
#             # "Cstar": c_star,
#         },
#         index=[0],
#     )
#     snc_output_json_df = pd.concat([snc_output_json_df, json_df], ignore_index=True)
results["RSQL"] = RELIABILITY_LEVEL
# results[["stress_ratio", "RSQL", "stress_parameter_aa"]].to_json(orient="records", colu)


pass
# LRSQ=1-LSSE/LSST
# Fp
# Linearcheck = (FN / (level(f) - 2)) / (FD / (NOD(f) - level(f)))
# RMSE = sqrt(SSE / NOD(f))
# SSE = Sum((10**log10_number_of_cycles - 10**(slope + intercept * log10_stress_parameter))**2)
# merge = pd.merge(df, results, on=["stress_ratio"])
# merge["sse"]
# sse = []
# for stress_ratio in df.stress_ratio.unique():

#     sse[stress_ratio] = ((
#         10**df.set_index("stress_ratio")["log10_number_of_cycles"][stress_ratio]
#         - 10 ** (results..slope + merge.intercept * merge.log10_stress_parameter_x)
#     ) ** 2)
# results["sse"] = merge[["stress_ratio", "sse"]].groupby("stress_ratio").sum()["sse"]

# .groupby("stress_ratio").sum()


# SST
# RSQ

# # For each data, calculate
# #   P1 = (log10_stress_parameter - mean(log10_stress_parameter))
# #      * (log10_number_of_cycles - mean(log10_number_of_cycles))
# df["diff_stress_param"] = df.log10_stress_parameter - df.groupby(
#     ["stress_ratio"]
# ).log10_stress_parameter.transform("mean")


# df["diff_nb_cycles"] = df.log10_number_of_cycles - df.groupby(
#     ["stress_ratio"]
# ).log10_number_of_cycles.transform("mean")
# df["XY"] = df.diff_stress_param * df.diff_nb_cycles


# # avg_by_stress_ratio["P"] = df.groupby(["stress_ratio"]).XY.sum()
# avg_by_stress_ratio["P"] = df.groupby(["stress_ratio"]).XY.sum()

# df["diff_stress_param**2"] = (
#     df.diff_stress_param**2
# )  # ** 2.groupby(["stress_ratio"]).diff_stress_param ** 2.sum()

# avg_by_stress_ratio["Q"] = df.groupby(["stress_ratio"])["diff_stress_param**2"].sum()

# avg_by_stress_ratio["B"] = avg_by_stress_ratio.P / avg_by_stress_ratio.Q

# avg_by_stress_ratio["A"] = (
#     avg_by_stress_ratio.log10_number_of_cycles
#     - avg_by_stress_ratio.B * avg_by_stress_ratio.log10_stress_parameter
# )

# df["Ycar"] = df.log10_stress_parameter * avg_by_stress_ratio.B + avg_by_stress_ratio.A

# avg_by_stress_ratio["AA"] = 10 ** (-avg_by_stress_ratio.A / avg_by_stress_ratio.B)

# avg_by_stress_ratio["BB"] = -1 / avg_by_stress_ratio.B


# Create JSON Output file
# R(f) = avg_by_stress_ratio.stress_ratio
# 50 constant ???
# AA
# BB
# LRSQ = 1 - LSSE / LSST
# Fp
# Linearcheck
# RMSE
# SSE
# SST
# RSQ

# with open(OUTPUT_JSON_FILE, "w") as f:
#     json.dump(avg_by_stress_ratio.to_dict(orient="list", axis=1), f)
# results.to_dict(orient="list", axis=1)

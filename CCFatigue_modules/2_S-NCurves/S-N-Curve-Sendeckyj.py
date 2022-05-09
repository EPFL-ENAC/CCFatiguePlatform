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
import pandas as pd
import math
import sendeckyj
from itertools import chain


SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
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
C_INITIAL_INCREMENT = 0.000001

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


def sendeckyj_equation_16(sigma_e, g):
    """Returns X
    Eq 16 (see ref, p259)
    """
    x = sigma_e / g
    return x


def sendeckyj_equation_17(m, k, sigma_e):
    """Returns G TODO G = ??
    Eq 17 (see ref, p259)
    """
    sum_of_log_sigma_e = sigma_e.apply(lambda x: math.log(x)).sum()
    # TODO ask Tassos if exp(1/(m-k)) or exp((1/(m-k)) * sum...)
    g = math.exp((1 / (m - k)) * sum_of_log_sigma_e)
    return g


def sendeckyj_equation_19(g, m, k, xs, alpha):
    """Returns beta TODO beta = ??
    Eq 19 (see ref, p259)
    """
    beta = g * (1 / (m - k) * xs.apply(lambda x: x**alpha).sum()) ** (1 / alpha)
    return beta


def tassos_equation(reliability_level, alpha, c, s, nn, a, beta):
    """TODO"""
    k1 = (-math.log(reliability_level / 100.0)) ** (1.0 / alpha)
    k2 = 1 / ((nn - a) * c)
    k3 = k2**s
    k4 = k1 * k3
    spn = beta * k4
    return spn


# Import input file (AGG format)
df = pd.read_csv(INPUT_FILE)

# Data are grouped by stress_ratio but one experiment
# can have two separate groups with same stress_ratio so we need to identify
# the groups
df["grp_id"] = (df.stress_ratio != df.stress_ratio.shift()).cumsum()


# Prepare SNC output list of cycles to failure

snc_output_json_df = pd.DataFrame(
    columns=[
        "stress_ratio",
        "RSQL",
        "A",
        "B",
        "LRSQ",
        "Fp",
        "Linearity",
        "RMSE",
        "SSE",
        "SST",
        "RSQ",
        "Sstar",
        "Cstar",
    ],
)
# snc_output_json_df["stress_ratio"] = df["stress_ratio"].unique()
# snc_output_json_df.set_index("stress_ratio", inplace=True)

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

c_mean = 0
s_mean = 0
alpha_mean = 0


# For each group of stress_ratio
for grp_id in df["grp_id"].unique():

    stress_ratio_df = df.loc[df.grp_id == grp_id]
    stress_ratio = stress_ratio_df.iloc[0].stress_ratio

    # Find the optimum solution
    alpha_max = 0
    p = 1
    q = 1
    alpha_old = 0
    c = C_MIN
    c_increment = C_INITIAL_INCREMENT
    c_old = 0
    c_c_old = 0  # TODO ??
    s_old = 0
    alpha_c_old = 0  # TODO ??

    data_count = len(stress_ratio_df)
    censored_data_count = len(
        stress_ratio_df[
            (stress_ratio_df.stress_parameter != stress_ratio_df.residual_strength)
        ]
    )

    residual_strength = stress_ratio_df.residual_strength
    stress_parameter = stress_ratio_df.stress_parameter
    number_of_cycles = stress_ratio_df.number_of_cycles

    counter = 0

    while c < C_MAX:

        s = S_MIN
        while s < S_MAX:

            s_done = False

            alpha = -sendeckyj.maximum_likelihood_estimators(
                s,
                c,
                residual_strength,
                stress_parameter,
                number_of_cycles,
                0,
            )

            if alpha > alpha_max:
                alpha_max = alpha
                p_star = p
                q_star = q
                s_star = s
                c_star = c

            if p > 1:
                # if (alpha - alpha_old) / (s - s_old) >= 0:
                if alpha >= alpha_old:
                    alpha_old = alpha
                    s_old = s
                    c_old = c
                else:
                    break

            p += 1
            s += S_STEP

        if q > 1:
            # if (alpha_old - alpha_c_old) / (c - c_c_old) >= 0:
            if alpha_old >= alpha_c_old:
                if alpha_old - alpha_c_old < 0.01 * alpha_max:
                    c_increment = 0.000001
                if alpha_old - alpha_c_old < 0.001 * alpha_max:
                    c_increment = 0.00001
                if alpha_old - alpha_c_old < 0.0001 * alpha_max:
                    c_increment = 0.0001
                if alpha_old - alpha_c_old < 0.00001 * alpha_max:
                    c_increment = 0.001
                if alpha_old - alpha_c_old < 0.000001 * alpha_max:
                    c_increment = 0.01
                if alpha_old - alpha_c_old < 0.0000001 * alpha_max:
                    c_increment = 0.1
                counter = 0
            else:
                counter += 1
                if counter > 500:
                    break
        p = 1
        q += 1
        alpha_c_old = alpha_max
        c_c_old = c
        alpha_old = 0
        s_old = 0
        c_old = 0
        c += c_increment

    # Eq 1
    sigmas_e = stress_ratio_df.apply(
        lambda x: sendeckyj_equation_1(
            x.stress_parameter,
            x.residual_strength,
            x.number_of_cycles,
            c_star,
            s_star,
        ),
        axis=1,
    )

    # Eq 17
    g = sendeckyj_equation_17(data_count, censored_data_count, sigmas_e)

    # Eq 16
    xs = sigmas_e.apply(lambda x: sendeckyj_equation_16(x, g))

    # Eq 19
    beta = sendeckyj_equation_19(g, data_count, censored_data_count, xs, alpha)

    reliability_level = stress_ratio_df.iloc[0].reliability_level

    # Prepare output data
    json_df = pd.DataFrame(
        [[stress_ratio, reliability_level, alpha_max, beta, s_star, c_star]],
        columns=["stress_ratio", "RSQL", "A", "B", "Sstar", "Cstar"],
    )
    snc_output_json_df = pd.concat([snc_output_json_df, json_df], ignore_index=True)

    a = -(1 - c_star) / c_star

    stress_parameter = cycles_to_failure.copy()
    stress_parameter["stress_parameter"] = stress_parameter.apply(
        lambda x: tassos_equation(reliability_level, alpha, c_star, s_star, x, a, beta)
    )

    stress_parameter["stress_ratio"] = stress_ratio

    snc_output_csv_df = pd.concat([snc_output_csv_df, stress_parameter])

# Export dataframes to files
snc_output_json_df.to_json(OUTPUT_JSON_FILE, orient="records")
snc_output_csv_df.to_csv(
    OUTPUT_CSV_FILE,
    index=False,
)

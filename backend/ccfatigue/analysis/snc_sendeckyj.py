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

import math
from itertools import chain

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.analysis.fortran.sendeckyj as sendeckyj  # Fortran optimized functions
import ccfatigue.analysis.utils.snc as snc
from ccfatigue.analysis.utils.astm import Astm

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

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(3000000, 20000000, 1000000),
        range(30000000, 1400000000, 100000000),
    )
)


def sendeckyj_equation_1(
    sigma_a: float, sigma_r: float, n: int, c: float, s: float
) -> float:
    """Returns sigma_e = the equivalent static strength
    Eq 1 (see ref, p248)
    """
    sigma_e = sigma_a * ((sigma_r / sigma_a) ** (1 / s) + (n - 1) * c) ** s
    return sigma_e


def sendeckyj_equation_16(sigma_e: float, g: float) -> float:
    """Returns X
    Eq 16 (see ref, p259)
    """
    x = sigma_e / g
    return x


def sendeckyj_equation_17(m: int, k: int, sigma_e) -> float:
    """Returns G TODO G = ??
    Eq 17 (see ref, p259)
    """
    sum_of_log_sigma_e = sigma_e.apply(lambda x: math.log(x)).sum()
    # TODO ask Tassos if exp(1/(m-k)) or exp((1/(m-k)) * sum...)
    g = math.exp((1 / (m - k)) * sum_of_log_sigma_e)
    return g


def sendeckyj_equation_19(
    g: float, m: int, k: int, xs: pd.Series, alpha: float
) -> float:
    """Returns beta TODO beta = ??
    Eq 19 (see ref, p259)
    """
    beta = g * (1 / (m - k) * float(xs.apply(lambda x: x**alpha).sum())) ** (
        1 / alpha
    )
    return beta


def tassos_equation(
    reliability_level: float,
    alpha: float,
    c: float,
    s: float,
    nn: float,
    a: float,
    beta: float,
) -> float:
    """TODO"""
    k1 = (-math.log(reliability_level / 100.0)) ** (1.0 / alpha)
    k2 = 1 / ((nn - a) * c)
    k3 = k2**s
    k4 = k1 * k3
    spn = beta * k4
    return spn


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_json_file: FilePath | WriteBuffer | None,
    output_csv_file: FilePath | WriteBuffer | None,
    confidence_interval: float = 50,
    confidence: int = 95,
):

    astm = Astm(confidence)

    # Import input file (AGG format)
    samples = pd.read_csv(input_file)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    # the groups
    samples["stress_ratio_id"] = (
        samples.stress_ratio != samples.stress_ratio.shift()
    ).cumsum()

    # Prepare SNC output list of cycles to failure

    snc_output_json_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "confidence_interval",
            "a",
            "b",
            "sstar",
            "cstar",
        ],
    )
    # snc_output_json_df["stress_ratio"] = df["stress_ratio"].unique()
    # snc_output_json_df.set_index("stress_ratio", inplace=True)

    snc_output_csv_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "cycles_to_failure",
            "stress_max",
            "stress_lowerbound",
            "stress_upperbound",
        ]
    )

    # Average by stress_ratio
    stress_ratios_df = (
        samples[
            [
                "stress_ratio_id",
                "stress_ratio",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
    )

    # Add columns with log10(stress_max) and log10(cycles_to_failure)
    samples["log10_stress_max"] = np.log10(samples.stress_max)
    samples["log10_cycles_to_failure"] = np.log10(samples.cycles_to_failure)

    # Q
    stress_ratios_df["xb"] = (
        samples[["stress_ratio_id", "log10_stress_max"]]
        .groupby("stress_ratio_id")
        .mean()
    )
    samples["q"] = samples.apply(
        lambda x: (x.log10_stress_max - stress_ratios_df.loc[x.stress_ratio_id].xb)
        ** 2,
        axis=1,
    )
    stress_ratios_df["q"] = (
        samples[["stress_ratio_id", "q"]].groupby("stress_ratio_id").sum()
    )

    # Calculate A (Eq 4) and B (Eq 5)
    linregress = samples.groupby("stress_ratio_id").apply(
        lambda x: stats.linregress(
            x.log10_stress_max,  # type: ignore
            x.log10_cycles_to_failure,  # type: ignore
        )
    )

    # Eq 4 and 5
    # Add A and B in avg_by_stress_ratio
    # Slope = A, Intercept = B
    stress_ratios_df["slope"] = linregress.apply(lambda x: x[1])
    stress_ratios_df["intercept"] = linregress.apply(lambda x: x[0])

    samples["ycar"] = samples.apply(
        lambda x: snc.equation_ycar(
            stress_ratios_df.loc[x.stress_ratio_id].slope,  # type: ignore
            stress_ratios_df.loc[x.stress_ratio_id].intercept,  # type: ignore
            x.log10_stress_max,
        ),
        axis=1,
    )

    # LSSE
    samples["lsse"] = (samples.log10_cycles_to_failure - samples.ycar) ** 2
    stress_ratios_df["lsse"] = (
        samples[["stress_ratio_id", "lsse"]].groupby("stress_ratio_id").sum()
    )

    # NOD
    stress_ratios_df["sample_count"] = (
        samples[["stress_ratio_id", "stress_ratio"]].groupby("stress_ratio_id").count()
    )

    # level
    stress_ratios_df["level"] = (
        samples[["stress_ratio_id", "stress_cluster_number"]]
        .groupby("stress_ratio_id")
        .nunique()
    )

    # Variance
    stress_ratios_df["variance"] = stress_ratios_df.apply(
        lambda x: math.sqrt(x.lsse / (x.sample_count - 2)), axis=1
    )

    # Fp
    stress_ratios_df["fp"] = stress_ratios_df.apply(
        lambda x: astm.get_astm_val(x.level - 2, x.sample_count - x.level),
        axis=1,
    )

    # PP
    stress_ratios_df["pp"] = 2 * stress_ratios_df.fp * stress_ratios_df.variance**2

    # For each group of stress_ratio
    for (stress_ratio_id, stress_ratio_df) in stress_ratios_df.iterrows():

        stress_ratio_sample_df = samples.loc[samples.stress_ratio_id == stress_ratio_id]
        stress_ratio = stress_ratio_sample_df.iloc[0].stress_ratio

        # Find the optimum solution
        alpha = 0
        alpha_max = 0
        p = 1
        q = 1
        alpha_old = 0
        c = C_MIN
        c_increment = C_INITIAL_INCREMENT
        c_star = 0
        s_star = 0
        alpha_c_old = 0  # TODO ??

        data_count = len(stress_ratio_sample_df)
        censored_data_count = len(
            stress_ratio_sample_df[
                (
                    stress_ratio_sample_df.stress_max
                    != stress_ratio_sample_df.residual_strength
                )
            ]
        )

        residual_strength = stress_ratio_sample_df.residual_strength
        stress_max = stress_ratio_sample_df.stress_max
        cycles_to_failure = stress_ratio_sample_df.cycles_to_failure

        counter = 0

        while c < C_MAX:

            s = S_MIN
            while s < S_MAX:

                alpha = -sendeckyj.maximum_likelihood_estimators(
                    s,
                    c,
                    residual_strength,
                    stress_max,
                    cycles_to_failure,
                    0,
                )

                if alpha > alpha_max:
                    alpha_max = alpha
                    s_star = s
                    c_star = c

                if p > 1:
                    if alpha >= alpha_old:
                        alpha_old = alpha
                    else:
                        break

                p += 1
                s += S_STEP

            if q > 1:
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
            alpha_old = 0
            c += c_increment

        # Eq 1
        sigmas_e = stress_ratio_sample_df.apply(
            lambda x: sendeckyj_equation_1(
                x.stress_max,
                x.residual_strength,
                x.cycles_to_failure,
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

        # Prepare output data
        json_df = pd.DataFrame(
            {
                "stress_ratio": stress_ratio,
                "confidence_interval": confidence_interval,
                "a": alpha_max,
                "b": beta,
                "sstar": s_star,
                "cstar": c_star,
            },
            index=[0],
        )
        snc_output_json_df = pd.concat([snc_output_json_df, json_df], ignore_index=True)

        a = -(1 - c_star) / c_star

        stress_max = pd.DataFrame(LIST_CYCLES_TO_FAILURE, columns=["cycles_to_failure"])

        stress_max["stress_max"] = stress_max.apply(
            lambda x: tassos_equation(
                confidence_interval, alpha, c_star, s_star, x, a, beta
            )
        )

        stress_bounds = stress_max.apply(
            lambda x: snc.stress_at_failure_bounds(
                stress_ratio_df.sample_count,
                stress_ratio_df.q,
                stress_ratio_df.slope,
                stress_ratio_df.intercept,
                x.cycles_to_failure,
                stress_ratio_df.pp,
                stress_ratio_df.xb,
            ),  # type: ignore
            axis=1,
        )
        stress_max["stress_lowerbound"] = stress_bounds.apply(lambda x: x[0])
        stress_max["stress_upperbound"] = stress_bounds.apply(lambda x: x[1])

        stress_max["stress_ratio"] = stress_ratio

        snc_output_csv_df = pd.concat([snc_output_csv_df, stress_max])

    # Export dataframes to files
    snc_output_json_df.to_json(output_json_file, orient="records")
    snc_output_csv_df.to_csv(
        output_csv_file,
        index=False,
    )

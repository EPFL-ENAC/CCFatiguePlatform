#!/usr/bin/env python

"""
Whitney pooling scheme for S-N curves with censored fatigue data.

Run-out convention:
- failure  : residual_strength is empty
- run-out  : residual_strength is provided

Implemented formulas:
- local MLE with run-outs: Eq. (3.22)
- local characteristic life: Eq. (3.23)
- pooled MLE: Eq. (3.24)
- pooled scale: Eq. (3.25)
- final S-N curve: Eq. (3.21)
"""

import math
from itertools import chain

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats
from scipy.optimize import brentq

LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(3000000, 20000000, 1000000),
        range(30000000, 1400000000, 100000000),
    )
)

STRESS_BIN = 10.0  # used only if stress_cluster_number is not present


def clamp_probability(p: float) -> float:
    return min(max(float(p), 1e-12), 1.0 - 1e-12)


def survival_probability(confidence_interval: float) -> float:
    p = float(confidence_interval)
    if p > 1.0:
        p /= 100.0
    return clamp_probability(p)


def build_stress_level_id(df: pd.DataFrame) -> pd.Series:
    """Use stress_cluster_number if available, otherwise cluster close stresses."""
    if "stress_cluster_number" in df.columns:
        return pd.to_numeric(df["stress_cluster_number"], errors="coerce")
    return (df["stress_max"] / STRESS_BIN).round().astype(int)


def solve_positive_root(func, x_min=1e-3, x_max=300.0, n=4000) -> float:
    """Solve f(x)=0 on a positive interval using bracketing."""
    xs = np.geomspace(x_min, x_max, n)
    ys = [func(x) for x in xs]

    for i in range(len(xs) - 1):
        y1, y2 = ys[i], ys[i + 1]
        if not np.isfinite(y1) or not np.isfinite(y2):
            continue
        if y1 == 0:
            return float(xs[i])
        if y2 == 0:
            return float(xs[i + 1])
        if y1 * y2 < 0:
            return float(brentq(func, xs[i], xs[i + 1], maxiter=200))

    raise ValueError("No positive root found.")


def local_mle_equation(alpha: float, q_fail: np.ndarray, q_run: np.ndarray) -> float:
    """
    Eq. (3.22) in normalized form.

    q_fail = N_ij / mean_failure_life
    q_run  = N_si / mean_failure_life
    """
    r_i = len(q_fail)
    if r_i <= 0 or alpha <= 0:
        raise ValueError("Invalid local MLE inputs.")

    num = np.sum((q_fail ** alpha) * np.log(q_fail))
    den = np.sum(q_fail ** alpha)

    if len(q_run) > 0:
        num += np.sum((q_run ** alpha) * np.log(q_run))
        den += np.sum(q_run ** alpha)

    return float(num / den - (1.0 / r_i) * np.sum(np.log(q_fail)) - 1.0 / alpha)


def local_characteristic_life(alpha: float, q_fail: np.ndarray, q_run: np.ndarray) -> float:
    """
    Eq. (3.23) in normalized form.
    """
    r_i = len(q_fail)
    if r_i <= 0:
        raise ValueError("No failures at this stress level.")

    total = np.sum(q_fail ** alpha)
    if len(q_run) > 0:
        total += np.sum(q_run ** alpha)

    return float((total / r_i) ** (1.0 / alpha))


def pooled_mle_equation(
    alpha_f: float,
    pooled_failures: list[np.ndarray],
    pooled_runouts: list[np.ndarray],
    failed_counts: list[int],
) -> float:
    """
    Eq. (3.24).
    """
    if alpha_f <= 0:
        raise ValueError("alpha_f must be positive.")

    r_T = int(np.sum(failed_counts))
    if r_T <= 0:
        raise ValueError("No failures in pooled data.")

    num = 0.0
    den = 0.0
    log_term = 0.0

    for q_fail, q_run in zip(pooled_failures, pooled_runouts):
        num += float(np.sum((q_fail ** alpha_f) * np.log(q_fail)))
        den += float(np.sum(q_fail ** alpha_f))
        log_term += float(np.sum(np.log(q_fail)))

        if len(q_run) > 0:
            num += float(np.sum((q_run ** alpha_f) * np.log(q_run)))
            den += float(np.sum(q_run ** alpha_f))

    return num / den - (1.0 / r_T) * log_term - 1.0 / alpha_f


def pooled_scale(
    alpha_f: float,
    pooled_failures: list[np.ndarray],
    pooled_runouts: list[np.ndarray],
    failed_counts: list[int],
) -> float:
    """
    Eq. (3.25).
    """
    r_T = int(np.sum(failed_counts))
    if r_T <= 0:
        raise ValueError("No failures in pooled data.")

    total = 0.0
    for q_fail, q_run in zip(pooled_failures, pooled_runouts):
        total += float(np.sum(q_fail ** alpha_f))
        if len(q_run) > 0:
            total += float(np.sum(q_run ** alpha_f))

    return float((total / r_T) ** (1.0 / alpha_f))


def whitney_sigma(
    n: float,
    sigma0: float,
    power: float,
    alpha_f: float,
    p_survival: float,
) -> float:
    """
    Eq. (3.21):
    sigma = sigma0 * (-ln(Ps))^(power/alpha_f) * N^(-power)
    """
    p = clamp_probability(p_survival)
    n = max(float(n), 1.0)
    return sigma0 * ((-math.log(p)) ** (power / alpha_f)) * (n ** (-power))


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_json_file: FilePath | WriteBuffer | None,
    output_csv_file: FilePath | WriteBuffer | None,
    confidence_interval: float = 50,
    confidence: int = 95,
):
    # Read AGG
    samples = pd.read_csv(input_file)

    # Numeric columns
    samples["stress_ratio"] = pd.to_numeric(samples["stress_ratio"], errors="coerce")
    samples["stress_max"] = pd.to_numeric(samples["stress_max"], errors="coerce")
    samples["cycles_to_failure"] = pd.to_numeric(
        samples["cycles_to_failure"], errors="coerce"
    )

    if "residual_strength" not in samples.columns:
        samples["residual_strength"] = np.nan
    samples["residual_strength"] = pd.to_numeric(
        samples["residual_strength"], errors="coerce"
    )

    # Keep valid rows
    samples = samples.dropna(subset=["stress_ratio", "stress_max", "cycles_to_failure"])
    samples = samples[
        (samples["stress_max"] > 0) & (samples["cycles_to_failure"] > 0)
    ].copy()

    # Failure / run-out
    samples["is_runout"] = samples["residual_strength"].notna()

    # Grouping
    samples["stress_ratio_id"] = samples["stress_ratio"].astype("category").cat.codes
    samples["stress_level_id"] = build_stress_level_id(samples)

    json_rows = []
    csv_rows = []

    for _, group_df in samples.groupby("stress_ratio_id"):
        stress_ratio = float(group_df.iloc[0]["stress_ratio"])

        # Survival probabilities
        p_mid = survival_probability(confidence_interval)
        p_low = 0.95
        p_high = 0.05

        level_records = []

        # ----- Local fits: Eqs. (3.22) and (3.23)
        for _, level_df in group_df.groupby("stress_level_id"):
            stress_mean = float(level_df["stress_max"].mean())

            failures = level_df.loc[
                ~level_df["is_runout"], "cycles_to_failure"
            ].to_numpy(dtype=float)

            runouts = level_df.loc[
                level_df["is_runout"], "cycles_to_failure"
            ].to_numpy(dtype=float)

            r_i = len(failures)

            # Need at least 2 failures for a stable local fit
            if r_i < 2:
                continue

            mean_failure_life = float(np.mean(failures))
            if mean_failure_life <= 0:
                continue

            q_fail = failures / mean_failure_life
            q_run = runouts / mean_failure_life

            alpha_i = solve_positive_root(lambda a: local_mle_equation(a, q_fail, q_run))
            n_hat_norm = local_characteristic_life(alpha_i, q_fail, q_run)
            n_hat_i = n_hat_norm * mean_failure_life

            level_records.append(
                {
                    "stress_max": stress_mean,
                    "failed_count": r_i,
                    "n_hat_i": n_hat_i,
                    "q_fail_pooled": failures / n_hat_i,
                    "q_run_pooled": runouts / n_hat_i,
                }
            )

        if len(level_records) < 2:
            raise ValueError(
                f"Whitney needs at least 2 usable stress levels for R={stress_ratio}"
            )

        # ----- Pooled fit: Eqs. (3.24) and (3.25)
        pooled_failures = [rec["q_fail_pooled"] for rec in level_records]
        pooled_runouts = [rec["q_run_pooled"] for rec in level_records]
        failed_counts = [rec["failed_count"] for rec in level_records]

        alpha_f = solve_positive_root(
            lambda a: pooled_mle_equation(a, pooled_failures, pooled_runouts, failed_counts)
        )
        q0 = pooled_scale(alpha_f, pooled_failures, pooled_runouts, failed_counts)

        # ----- Adjusted characteristic life: Eq. (3.20)
        stress_levels = np.array([rec["stress_max"] for rec in level_records], dtype=float)
        n0_levels = np.array([q0 * rec["n_hat_i"] for rec in level_records], dtype=float)

        # ----- Final regression: log(N0) vs log(sigma)
        log_sigma = np.log10(stress_levels)
        log_n0 = np.log10(n0_levels)

        reg = stats.linregress(log_sigma, log_n0)
        slope = float(reg.slope)
        intercept = float(reg.intercept)

        if slope == 0 or not np.isfinite(slope):
            raise ValueError(f"Invalid regression for R={stress_ratio}")

        sigma0 = 10 ** (-intercept / slope)
        power = -1.0 / slope

        # JSON output
        json_rows.append(
            {
                "stress_ratio": stress_ratio,
                "confidence_interval": confidence_interval,
                "alpha_f": alpha_f,
                "q0": q0,
                "sigma0": sigma0,
                "power": power,
            }
        )

        # CSV output
        for n in LIST_CYCLES_TO_FAILURE:
            csv_rows.append(
                {
                    "stress_ratio": stress_ratio,
                    "cycles_to_failure": n,
                    "stress_max": whitney_sigma(n, sigma0, power, alpha_f, p_mid),
                    "stress_lowerbound": whitney_sigma(n, sigma0, power, alpha_f, p_low),
                    "stress_upperbound": whitney_sigma(n, sigma0, power, alpha_f, p_high),
                }
            )

    pd.DataFrame(json_rows).to_json(output_json_file, orient="records")
    pd.DataFrame(csv_rows).to_csv(output_csv_file, index=False)
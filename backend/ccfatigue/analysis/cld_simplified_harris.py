#!/usr/bin/env python
"""
Simplified Harris CLD: same as Harris but with u(N) = v(N).

At each N level, f and u(=v) are jointly estimated via 2-parameter OLS
with intercept under the u=v constraint:
    log10(σ_a/UTS) = log10(f) + u·(x1 + x2)
where x1 = log10(1 − σ_m/UTS), x2 = log10(UCS/UTS + σ_m/UTS).

f and u are then each regressed linearly against log10(N).
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.analysis.utils.cld as cld
import ccfatigue.analysis.utils.harris as harris

DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_csv_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    snc_df = pd.read_csv(input_file)
    snc_df = snc_df.copy()
    snc_df["stress_ratio"] = pd.to_numeric(snc_df["stress_ratio"], errors="coerce")
    snc_df["cycles_to_failure"] = pd.to_numeric(
        snc_df["cycles_to_failure"], errors="coerce"
    )
    snc_df["stress_max"] = pd.to_numeric(snc_df["stress_max"], errors="coerce")
    snc_df = snc_df.dropna(subset=["stress_ratio", "cycles_to_failure", "stress_max"])

    r_ratios = sorted(snc_df["stress_ratio"].unique())
    if len(r_ratios) < 3:
        raise Exception(
            "Input data needs at least 3 R-ratios for Simplified Harris method."
        )

    ucs = abs(float(ucs))
    uts = abs(float(uts))

    # Step 1: fit a log-log S-N line per R-ratio
    sn_fits: dict[float, tuple[float, float]] = {}
    sn_ranges: dict[float, tuple[float, float]] = {}
    for r in r_ratios:
        group = snc_df[snc_df["stress_ratio"] == r]
        log_n = np.log10(group["cycles_to_failure"].astype(float))
        log_s = np.log10(group["stress_max"].astype(float))
        sn_ranges[r] = (float(log_n.min()), float(log_n.max()))
        if len(group) >= 2:
            fit = stats.linregress(log_n, log_s)
            sn_fits[r] = (float(fit.slope), float(fit.intercept))
        else:
            sn_fits[r] = (0.0, float(log_s.iloc[0]))

    # Step 2: N levels within ±1 log-decade of the data range
    log_n_min = float(np.log10(snc_df["cycles_to_failure"].astype(float).min()))
    log_n_max = float(np.log10(snc_df["cycles_to_failure"].astype(float).max()))
    fitting_cycles = [
        c for c in CLD_CYCLES_COUNT if log_n_min - 1.0 <= np.log10(c) <= log_n_max + 1.0
    ]

    # Step 3: 2-parameter OLS with intercept, enforcing u = v:
    #   y = log10(f) + u·z   where z = log10(d1) + log10(d2) = x1 + x2
    #   X = [1, z],  β = [log10(f), u]
    log_n_fit: list[float] = []
    f_fit: list[float] = []
    u_fit: list[float] = []

    for cycles in fitting_cycles:
        log_n = np.log10(float(cycles))
        z_vals: list[float] = []
        y_vals: list[float] = []
        for r, (b, a) in sn_fits.items():
            r_log_min, r_log_max = sn_ranges[r]
            if not (r_log_min - 1.0 <= log_n <= r_log_max + 1.0):
                continue
            sigma_max = 10.0 ** (a + b * log_n)
            sigma_a = harris.calculate_stress_amplitude(r, sigma_max)
            sigma_m = harris.calculate_stress_mean(r, sigma_max)
            if sigma_a <= 0:
                continue
            d1 = 1.0 - sigma_m / uts
            d2 = ucs / uts + sigma_m / uts
            if d1 <= 0 or d2 <= 0:
                continue
            y_vals.append(np.log10(sigma_a / uts))
            z_vals.append(np.log10(d1) + np.log10(d2))
        if len(z_vals) < 2:
            continue
        X = np.column_stack([np.ones(len(z_vals)), z_vals])
        coeffs, _, _, _ = np.linalg.lstsq(X, np.array(y_vals), rcond=None)
        log_n_fit.append(log_n)
        f_fit.append(10.0 ** float(coeffs[0]))
        u_fit.append(float(coeffs[1]))

    if len(log_n_fit) < 2:
        raise Exception(
            "Not enough cycle levels within data range to fit Simplified Harris CLD."
        )

    # Step 4: linear regression of f and u(=v) vs log10(N)
    log_n_arr = np.array(log_n_fit)
    lr_f = stats.linregress(log_n_arr, f_fit)
    lr_u = stats.linregress(log_n_arr, u_fit)

    cld_df = pd.DataFrame()

    for cycles_to_failure in CLD_CYCLES_COUNT:
        log_n = np.log10(float(cycles_to_failure))
        ff = float(lr_f.slope * log_n + lr_f.intercept)  # type: ignore
        uv_val = float(lr_u.slope * log_n + lr_u.intercept)  # type: ignore

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0, -ucs)

        increment = (uts + ucs) / 40
        start = -ucs
        stop = uts
        for sm in np.arange(start, stop, increment):
            c2 = ff * (1 - sm / uts) ** uv_val
            c3 = ((ucs / uts) + (sm / uts)) ** uv_val
            stress_amplitude = c2 * c3 * uts

            cld_df = cld.cld_add_row(
                cld_df, cycles_to_failure, max(stress_amplitude, 0.0), sm
            )

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0, uts)

    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"], ascending=[True, True]
    ).reset_index(drop=True)

    cld_df.to_csv(path_or_buf=output_csv_file, index=False)  # type: ignore

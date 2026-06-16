#!/usr/bin/env python
"""
This code takes in SNC data and outputs CLD data

Harris's CLD is described in Fatigue of Fiber-reinforced Composites [1] p. 108
The original papers are [2] and [3]

[1] https://link.springer.com/book/10.1007/978-1-84996-181-3
    DOI 10.1007/978-1-84996-181-3
[2] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[3] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import stats

import ccfatigue.analysis.utils.cld as cld
import ccfatigue.analysis.utils.harris as harris

DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_csv_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    """
    Execute the CLD Harris algorithm.

    Accepts raw experimental SNC data (one row per specimen) or pre-fitted
    S-N curve output.  A log-log S-N line is fitted per R-ratio, then
    f, u, v are determined at several N levels via 3-parameter OLS of the
    Harris equation (Eq. 4.10).  f follows a power-law in N (Eq. 4.14,
    log-log regression), u and v are linear in log(N) (Eqs. 4.12–4.13).

    Parameters
    ----------
        input_file
            SNC csv file (columns: stress_ratio, cycles_to_failure, stress_max)
        output_csv_file
            CLD csv output
        ucs
            Ultimate compressive stress (positive value)
        uts
            Ultimate tensile stress (positive value)
    """

    bounds_margin = harris.BOUNDS_MARGIN

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
        raise Exception("Input data is not enough to apply Harris' method.")

    ucs = abs(float(ucs))
    uts = abs(float(uts))

    # Step 1: fit a log-log S-N line per R-ratio.
    sn_fits: dict[float, tuple[float, float]] = {}  # r -> (slope, intercept)
    sn_ranges: dict[float, tuple[float, float]] = {}  # r -> (log_n_min, log_n_max)
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

    # Step 2: N levels within ±1 log-decade of the data range.
    log_n_min = float(np.log10(snc_df["cycles_to_failure"].astype(float).min()))
    log_n_max = float(np.log10(snc_df["cycles_to_failure"].astype(float).max()))
    fitting_cycles = [
        c for c in CLD_CYCLES_COUNT if log_n_min - 1.0 <= np.log10(c) <= log_n_max + 1.0
    ]

    # Step 3: at each N level evaluate every R-ratio's S-N curve, compute the
    # Harris log-space variables and solve the 3-parameter OLS problem
    #   log(σ_a/UTS) = log(f) + u·log(1−m) + v·log(c+m)   [Eq. 4.10]
    log_n_fit: list[float] = []
    log_f_fit: list[float] = []
    u_fit: list[float] = []
    v_fit: list[float] = []

    for cycles in fitting_cycles:
        log_n = np.log10(float(cycles))

        x1_vals, x2_vals, y_vals = [], [], []

        for r, (b, a) in sn_fits.items():
            r_log_min, r_log_max = sn_ranges[r]
            if not (r_log_min - 1.0 <= log_n <= r_log_max + 1.0):
                continue

            sigma_max = 10.0 ** (a + b * log_n)
            sigma_a = harris.calculate_stress_amplitude(r, sigma_max)
            sigma_m = harris.calculate_stress_mean(r, sigma_max)
            sigma_m = harris.bounds_stress_mean(sigma_m, uts, ucs, bounds_margin)

            if sigma_a <= 0:
                continue
            d1 = 1.0 - sigma_m / uts
            d2 = ucs / uts + sigma_m / uts
            if d1 <= 0 or d2 <= 0:
                continue

            x1_vals.append(np.log10(d1))
            x2_vals.append(np.log10(d2))
            y_vals.append(np.log10(sigma_a / uts))

        if len(x1_vals) < 3:
            continue

        X = np.column_stack([np.ones(len(x1_vals)), x1_vals, x2_vals])
        coeffs, _, _, _ = np.linalg.lstsq(X, np.array(y_vals), rcond=None)
        log_f_k, u_k, v_k = float(coeffs[0]), float(coeffs[1]), float(coeffs[2])

        if u_k <= 0 or v_k <= 0:
            continue

        log_n_fit.append(log_n)
        log_f_fit.append(log_f_k)
        u_fit.append(u_k)
        v_fit.append(v_k)

    if len(log_n_fit) < 2:
        raise Exception("Not enough cycle levels within data range to fit Harris CLD.")

    # Step 4: power-law regression for f (Eq. 4.14): log(f) vs log(N),
    #         linear regression for u and v (Eqs. 4.12–4.13): u, v vs log(N).
    log_n_arr = np.array(log_n_fit)
    lr_logf = stats.linregress(log_n_arr, log_f_fit)
    lr_u = stats.linregress(log_n_arr, u_fit)
    lr_v = stats.linregress(log_n_arr, v_fit)

    cld_df = pd.DataFrame()

    for cycles_to_failure in CLD_CYCLES_COUNT:
        log_n = np.log10(float(cycles_to_failure))

        # Eq 4.10: σ_a = f · UTS · (1 − σ_m/UTS)^u · (UCS/UTS + σ_m/UTS)^v
        ff = 10 ** (lr_logf.slope * log_n + lr_logf.intercept)  # type: ignore
        uu = lr_u.slope * log_n + lr_u.intercept  # type: ignore
        vv = lr_v.slope * log_n + lr_v.intercept  # type: ignore

        if uu <= 0 or vv <= 0:
            continue

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0, -ucs)

        increment = 0.90 * (uts + ucs) / 40
        start = -0.90 * ucs
        stop = 0.90 * uts + increment
        for sm in np.arange(start, stop, increment):
            # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L192
            c2 = ff * (1 - sm / uts) ** uu
            c3 = ((ucs / uts) + (sm / uts)) ** vv
            stress_amplitude = c2 * c3 * uts

            cld_df = cld.cld_add_row(
                cld_df, cycles_to_failure, max(stress_amplitude, 0.0), sm
            )

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0, uts)

    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"], ascending=[True, True]
    ).reset_index(drop=True)

    cld_df.to_csv(path_or_buf=output_csv_file, index=False)  # type: ignore

#!/usr/bin/env python
"""
Implementation of Kawai's CLD.

This module builds an anisomorphic constant fatigue life diagram from the
critical S-N curve at the critical stress ratio R_chi = -UCS/UTS.
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cld as cld
import ccfatigue.analysis.utils.harris as harris

DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def _interpolate_critical_curve(cycles_obs, values_obs, cycles_target):
    """Fit an S-N regression to critical data and evaluate at target cycles.

    For positive quantities (sigma_max, sigma_a) a log-log fit (σ = A·N^b)
    is used — this avoids the zero-slope extrapolation that occurs when
    adjacent data points share the same amplitude, and gives physically
    appropriate power-law behaviour outside the data range.

    For quantities that may be zero or negative (sigma_m) a linear regression
    in log-N space is used instead.
    """
    cycles = np.asarray(cycles_obs, dtype=float)
    values = np.asarray(values_obs, dtype=float)
    targets = np.asarray(cycles_target, dtype=float)

    finite_mask = np.isfinite(cycles) & np.isfinite(values) & (cycles > 0)
    cycles = cycles[finite_mask]
    values = values[finite_mask]

    if cycles.size == 0:
        raise ValueError("no observations available for critical S-N curve")

    log_cycles = np.log10(cycles)
    log_targets = np.log10(targets)

    if cycles.size == 1:
        return np.full(targets.size, values[0])

    if (values > 0).all():
        # Log-log S-N regression: log(σ) = intercept + slope·log(N)
        slope, intercept = np.polyfit(log_cycles, np.log10(values), 1)
        return np.maximum(10.0 ** (intercept + slope * log_targets), 0.0)
    else:
        # Linear-log regression for sigma_m which can be zero or negative
        slope, intercept = np.polyfit(log_cycles, values, 1)
        return intercept + slope * log_targets


def _get_critical_rows(snc_df: pd.DataFrame, r_chi: float) -> pd.DataFrame:
    """Return rows belonging to the Kawai critical S-N curve."""
    stress_ratio_text = snc_df["stress_ratio"].astype(str).str.strip().str.lower()
    critical_rows = snc_df.loc[stress_ratio_text == "chi"].copy()

    if not critical_rows.empty:
        critical_rows["stress_ratio_for_calc"] = r_chi
        return critical_rows

    numeric_ratios = pd.to_numeric(snc_df["stress_ratio"], errors="coerce")

    if numeric_ratios.isna().all():
        raise ValueError("No numeric stress_ratio values found for Kawai")

    critical_df = snc_df.copy()
    critical_df["_r_diff"] = np.abs(numeric_ratios - r_chi)
    min_diff = critical_df["_r_diff"].min()
    critical_rows = critical_df.loc[critical_df["_r_diff"] == min_diff].copy()
    critical_rows["stress_ratio_for_calc"] = pd.to_numeric(
        critical_rows["stress_ratio"], errors="coerce"
    ).fillna(r_chi)

    return critical_rows


def _kawai_stress_amplitude(
    stress_mean: float,
    sigma_a_chi: float,
    sigma_m_chi: float,
    psi_chi: float,
    ucs: float,
    uts: float,
) -> float:
    """Return Kawai CFL stress amplitude for a mean stress.

    Based on Eq. 4.15 from "Fatigue of Fiber Reinforced Composites"
    """
    lower_bound = -abs(float(ucs))
    upper_bound = abs(float(uts))

    # Outside the material bounds
    if stress_mean <= lower_bound or stress_mean >= upper_bound:
        return 0.0

    # At the critical point
    if np.isclose(stress_mean, sigma_m_chi):
        return max(0.0, float(sigma_a_chi))

    exponent = 2.0 - psi_chi

    if stress_mean > sigma_m_chi:
        # Tension side: sigma_m from sigma_m_chi to UTS
        denominator = upper_bound - sigma_m_chi
        numerator = stress_mean - sigma_m_chi
    else:
        # Compression side: sigma_m from sigma_m_chi to UCS
        denominator = sigma_m_chi - lower_bound
        numerator = sigma_m_chi - stress_mean

    if denominator == 0:
        return 0.0

    ratio = numerator / denominator
    ratio = min(max(ratio, 0.0), 1.0)

    # Eq. 4.15: (sigma_a' - sigma_a) / sigma_a' = (ratio)^exponent
    # Therefore: sigma_a = sigma_a' * (1 - ratio^exponent)
    stress_amplitude = sigma_a_chi * (1.0 - ratio**exponent)

    return max(0.0, float(stress_amplitude))


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    """Execute the Kawai CLD algorithm."""
    snc_df = pd.read_csv(snc_csv_input_file)

    required_columns = {"stress_ratio", "cycles_to_failure", "stress_max"}
    missing_columns = required_columns - set(snc_df.columns)
    if missing_columns:
        raise ValueError(
            f"Missing required SNC columns: {', '.join(sorted(missing_columns))}"
        )

    ucs = abs(float(ucs))
    uts = abs(float(uts))
    r_chi = -ucs / uts

    critical_rows = _get_critical_rows(snc_df.copy(), r_chi)

    if critical_rows.empty:
        raise ValueError("No data found near the critical R-ratio for Kawai")

    critical_rows["cycles_to_failure"] = pd.to_numeric(
        critical_rows["cycles_to_failure"], errors="coerce"
    )
    critical_rows["sigma_max_chi"] = pd.to_numeric(
        critical_rows["stress_max"], errors="coerce"
    )
    critical_rows = critical_rows.dropna(
        subset=["cycles_to_failure", "sigma_max_chi", "stress_ratio_for_calc"]
    )

    if critical_rows.empty:
        raise ValueError("No valid critical S-N observations for Kawai")

    critical_rows["sigma_a_chi"] = critical_rows.apply(
        lambda row: harris.calculate_stress_amplitude(
            row.stress_ratio_for_calc,
            row.sigma_max_chi,
        ),
        axis=1,
    )
    critical_rows["sigma_m_chi"] = critical_rows.apply(
        lambda row: harris.calculate_stress_mean(
            row.stress_ratio_for_calc,
            row.sigma_max_chi,
        ),
        axis=1,
    )

    grouped = (
        critical_rows.groupby("cycles_to_failure")
        .agg(
            sigma_max_chi=("sigma_max_chi", "mean"),
            sigma_a_chi=("sigma_a_chi", "mean"),
            sigma_m_chi=("sigma_m_chi", "mean"),
        )
        .reset_index()
    )

    if grouped.empty:
        raise ValueError("No critical S-N observations after grouping by cycles")

    target_cycles = np.array(CLD_CYCLES_COUNT, dtype=float)
    sigma_max_chi_t = _interpolate_critical_curve(
        grouped["cycles_to_failure"], grouped["sigma_max_chi"], target_cycles
    )
    sigma_a_chi_t = _interpolate_critical_curve(
        grouped["cycles_to_failure"], grouped["sigma_a_chi"], target_cycles
    )
    sigma_m_chi_t = _interpolate_critical_curve(
        grouped["cycles_to_failure"], grouped["sigma_m_chi"], target_cycles
    )

    sigma_b = max(uts, ucs)
    cld_df = pd.DataFrame()

    for index, cycles_to_failure in enumerate(target_cycles):
        sigma_max_chi = float(sigma_max_chi_t[index])
        sigma_a_chi = float(sigma_a_chi_t[index])
        sigma_m_chi = float(sigma_m_chi_t[index])
        psi_chi = sigma_max_chi / sigma_b if sigma_b != 0 else 0.0

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0.0, -ucs)

        # Generate points with better density, especially around sigma_m_chi
        increment = (
            0.90 * (uts + ucs) / 100.0
        )  # Increased from 40 to 100 for smoother curves
        start = -0.90 * ucs
        stop = 0.90 * uts + increment
        stress_means = np.arange(start, stop, increment)

        # Also add dense points around sigma_m_chi (±3*increment)
        if not np.isclose(sigma_m_chi, 0):
            near_peak = np.arange(
                sigma_m_chi - 3 * increment, sigma_m_chi + 3 * increment, increment / 2
            )
            stress_means = np.concatenate([stress_means, near_peak])

        stress_means = np.append(stress_means, sigma_m_chi)

        for stress_mean in sorted(set(stress_means)):
            stress_amplitude = _kawai_stress_amplitude(
                float(stress_mean),
                sigma_a_chi,
                sigma_m_chi,
                psi_chi,
                ucs,
                uts,
            )
            cld_df = cld.cld_add_row(
                cld_df,
                cycles_to_failure,
                stress_amplitude,
                float(stress_mean),
            )

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0.0, uts)

    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"],
        ascending=[True, True],
    ).reset_index(drop=True)

    cld_df.to_csv(path_or_buf=cld_csv_output_file, index=False)  # type: ignore

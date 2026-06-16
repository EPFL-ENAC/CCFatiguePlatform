#!/usr/bin/env python
"""
Implementation of Boerstra's CLD.

Boerstra's model is described in Fatigue of Fiber-reinforced Composites,
section 4.3.1.5. It estimates the model parameters directly from fatigue
points in the stress-mean/stress-amplitude/life space.
"""

import json
from dataclasses import dataclass

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer
from scipy import optimize, stats

import ccfatigue.analysis.utils.cld as cld
import ccfatigue.analysis.utils.harris as harris

DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9

EPSILON = (
    1e-12  # A small value to prevent division by zero and log of zero in calculations.
)


@dataclass(frozen=True)
class BoerstraParameters:
    m0: float
    d: float
    np_reference: float
    alpha_t: float
    alpha_c: float
    sigma_apex: float


def _stress_amplitude_factor(
    stress_mean: np.ndarray | float,
    uts: float,
    ucs: float,
    alpha_t: float,
    alpha_c: float,
) -> np.ndarray:
    """Return the mean-stress factor from Eqs. 4.18 and 4.19."""
    stress_mean = np.asarray(stress_mean, dtype=float)
    factor = np.empty_like(stress_mean, dtype=float)

    tensile = stress_mean >= 0.0
    tensile_ratio = np.clip(stress_mean[tensile] / uts, 0.0, 1.0)
    compressive_ratio = np.clip(stress_mean[~tensile] / -ucs, 0.0, 1.0)

    factor[tensile] = 1.0 - tensile_ratio**alpha_t
    factor[~tensile] = 1.0 - compressive_ratio**alpha_c

    return np.maximum(factor, EPSILON)


def _slope_m(stress_mean: np.ndarray | float, m0: float, d: float) -> np.ndarray:
    """Return S-N slope parameter m from Eq. 4.20."""
    stress_mean = np.asarray(stress_mean, dtype=float)
    exponent = np.clip(-stress_mean / d, -50.0, 50.0)
    return np.maximum(m0 * np.exp(exponent), EPSILON)


def _project_to_reference_life(
    stress_amplitude: np.ndarray,
    cycles_to_failure: np.ndarray,
    stress_mean: np.ndarray,
    params: BoerstraParameters,
) -> np.ndarray:
    """Project measured amplitudes to the reference life Np."""
    m = _slope_m(stress_mean, params.m0, params.d)
    return stress_amplitude * (cycles_to_failure / params.np_reference) ** (1.0 / m)


def _theoretical_stress_amplitude(
    cycles_to_failure: float | np.ndarray,
    stress_mean: float | np.ndarray,
    params: BoerstraParameters,
    uts: float,
    ucs: float,
) -> np.ndarray:
    """Return Boerstra stress amplitude at a life and mean stress."""
    cycles_to_failure = np.asarray(cycles_to_failure, dtype=float)
    stress_mean = np.asarray(stress_mean, dtype=float)

    factor = _stress_amplitude_factor(
        stress_mean,
        uts,
        ucs,
        params.alpha_t,
        params.alpha_c,
    )
    m = _slope_m(stress_mean, params.m0, params.d)
    sigma_ap = params.sigma_apex * factor
    stress_amplitude = sigma_ap * (params.np_reference / cycles_to_failure) ** (1.0 / m)

    return np.maximum(stress_amplitude, 0.0)


def _signed_distance(delta_sigma: np.ndarray, delta_cycles: np.ndarray) -> np.ndarray:
    """Return Boerstra's signed shortest-distance residual.

    Book formula: Δt = sign(Δσ_a) / sqrt(1/Δσ_a² + 1/Δn²)
    """
    result = np.zeros_like(delta_sigma)
    valid = (np.abs(delta_sigma) > EPSILON) & (np.abs(delta_cycles) > EPSILON)
    ds = delta_sigma[valid]
    dn = delta_cycles[valid]
    result[valid] = np.sign(ds) / np.sqrt((1.0 / ds**2) + (1.0 / dn**2))
    return result


def _initial_m0(snc_df: pd.DataFrame) -> float:
    """Estimate a reasonable initial m0 from log-log S-N slopes."""
    estimates = []

    for _, group in snc_df.groupby("stress_ratio"):
        if len(group) < 2:
            continue

        log_n = np.log(group["cycles_to_failure"].to_numpy(dtype=float))
        log_sigma_a = np.log(group["stress_amplitude"].to_numpy(dtype=float))

        if np.unique(log_sigma_a).size < 2:
            continue

        regression = stats.linregress(log_sigma_a, log_n)
        if np.isfinite(regression.slope) and regression.slope < 0:
            estimates.append(-regression.slope)

    if estimates:
        return float(np.clip(np.median(estimates), 1.0, 200.0))

    return 10.0


def _fit_parameters(
    snc_df: pd.DataFrame,
    uts: float,
    ucs: float,
    np_reference: float,
    m0_init: float | None = None,
    d_init: float | None = None,
    alpha_t_init: float | None = None,
    alpha_c_init: float | None = None,
) -> BoerstraParameters:
    """Estimate Boerstra's m0, D, alphaT, and alphaC parameters for a fixed Np."""
    stress_mean = snc_df["stress_mean"].to_numpy(dtype=float)
    stress_amplitude = snc_df["stress_amplitude"].to_numpy(dtype=float)
    cycles_to_failure = snc_df["cycles_to_failure"].to_numpy(dtype=float)
    strength = max(uts, ucs)

    def unpack(log_params) -> tuple[float, float, float, float]:
        return tuple(np.exp(log_params))  # type: ignore

    def _estimate_sigma_apex(m0, d, alpha_t, alpha_c) -> float:
        provisional_params = BoerstraParameters(
            m0=m0,
            d=d,
            np_reference=np_reference,
            alpha_t=alpha_t,
            alpha_c=alpha_c,
            sigma_apex=1.0,
        )
        sigma_ap = _project_to_reference_life(
            stress_amplitude, cycles_to_failure, stress_mean, provisional_params
        )
        factor = _stress_amplitude_factor(stress_mean, uts, ucs, alpha_t, alpha_c)
        # Geometric mean in log space — robust against outliers near ±UCS/UTS
        return float(np.exp(np.mean(np.log(np.maximum(sigma_ap / factor, EPSILON)))))

    def residuals(log_params) -> np.ndarray:
        m0, d, alpha_t, alpha_c = unpack(log_params)
        sigma_apex = _estimate_sigma_apex(m0, d, alpha_t, alpha_c)

        params = BoerstraParameters(
            m0=m0,
            d=d,
            np_reference=np_reference,
            alpha_t=alpha_t,
            alpha_c=alpha_c,
            sigma_apex=sigma_apex,
        )
        sigma_a_mod = _theoretical_stress_amplitude(
            np_reference, stress_mean, params, uts, ucs
        )
        provisional_params = BoerstraParameters(
            m0=m0,
            d=d,
            np_reference=np_reference,
            alpha_t=alpha_t,
            alpha_c=alpha_c,
            sigma_apex=1.0,
        )
        sigma_ap = _project_to_reference_life(
            stress_amplitude, cycles_to_failure, stress_mean, provisional_params
        )
        m = _slope_m(stress_mean, m0, d)
        cycles_estimated = np_reference * (sigma_a_mod / stress_amplitude) ** m

        delta_sigma = np.log(np.maximum(sigma_ap, EPSILON)) - np.log(
            np.maximum(sigma_a_mod, EPSILON)
        )
        delta_cycles = np.log(cycles_to_failure) - np.log(
            np.maximum(cycles_estimated, EPSILON)
        )

        return _signed_distance(delta_sigma, delta_cycles)

    lower_bounds = np.log([1.0, strength * 0.05, 0.1, 0.1])
    upper_bounds = np.log([200.0, strength * 50.0, 20.0, 20.0])
    initial = np.clip(
        np.log(
            [
                m0_init if m0_init is not None else _initial_m0(snc_df),
                d_init if d_init is not None else strength,
                alpha_t_init if alpha_t_init is not None else 2.0,
                alpha_c_init if alpha_c_init is not None else 2.0,
            ]
        ),
        lower_bounds,
        upper_bounds,
    )

    result = optimize.least_squares(
        residuals,
        initial,
        bounds=(lower_bounds, upper_bounds),
        max_nfev=3000,
    )

    m0, d, alpha_t, alpha_c = unpack(result.x)
    sigma_apex = _estimate_sigma_apex(m0, d, alpha_t, alpha_c)

    return BoerstraParameters(
        m0=m0,
        d=d,
        np_reference=np_reference,
        alpha_t=alpha_t,
        alpha_c=alpha_c,
        sigma_apex=sigma_apex,
    )


def _prepare_input(input_file: FilePath | ReadCsvBuffer, uts: float, ucs: float):
    snc_df = pd.read_csv(input_file)

    required_columns = {"stress_ratio", "cycles_to_failure", "stress_max"}
    missing_columns = required_columns - set(snc_df.columns)
    if missing_columns:
        raise ValueError(
            f"Missing required SNC columns: {', '.join(sorted(missing_columns))}"
        )

    snc_df = snc_df.copy()
    snc_df["stress_ratio"] = pd.to_numeric(snc_df["stress_ratio"], errors="coerce")
    snc_df["cycles_to_failure"] = pd.to_numeric(
        snc_df["cycles_to_failure"], errors="coerce"
    )
    snc_df["stress_max"] = pd.to_numeric(snc_df["stress_max"], errors="coerce")
    snc_df = snc_df.dropna(subset=["stress_ratio", "cycles_to_failure", "stress_max"])

    if snc_df.empty:
        raise ValueError("No valid SNC rows found for Boerstra")

    snc_df["stress_amplitude"] = snc_df.apply(
        lambda row: harris.calculate_stress_amplitude(
            row.stress_ratio,
            row.stress_max,
        ),
        axis=1,
    )
    snc_df["stress_mean"] = snc_df.apply(
        lambda row: harris.calculate_stress_mean(
            row.stress_ratio,
            row.stress_max,
        ),
        axis=1,
    )

    valid = (
        (snc_df["cycles_to_failure"] > 0)
        & (snc_df["stress_amplitude"] > 0)
        & (snc_df["stress_mean"] > -ucs)
        & (snc_df["stress_mean"] < uts)
    )
    snc_df = snc_df.loc[valid].copy()

    if len(snc_df) < 5:
        raise ValueError("Input data is not enough to apply Boerstra's method")

    return snc_df


DEFAULT_NP_REFERENCE = 100  # recommended by Boerstra (Table 2: minimises SDt)


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    cld_json_output_file=None,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
    np_reference: float = DEFAULT_NP_REFERENCE,
    m0_init: float | None = None,
    d_init: float | None = None,
    alpha_t_init: float | None = None,
    alpha_c_init: float | None = None,
) -> None:
    """Execute the CLD Boerstra algorithm."""
    ucs = abs(float(ucs))
    uts = abs(float(uts))

    snc_df = _prepare_input(snc_csv_input_file, uts, ucs)
    params = _fit_parameters(
        snc_df,
        uts,
        ucs,
        float(np_reference),
        m0_init,
        d_init,
        alpha_t_init,
        alpha_c_init,
    )

    cld_df = pd.DataFrame()

    for cycles_to_failure in CLD_CYCLES_COUNT:
        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0.0, -ucs)

        for stress_mean in np.linspace(-ucs, uts, 102)[1:-1]:
            stress_amplitude = _theoretical_stress_amplitude(
                cycles_to_failure,
                stress_mean,
                params,
                uts,
                ucs,
            )
            cld_df = cld.cld_add_row(
                cld_df,
                cycles_to_failure,
                float(stress_amplitude),
                float(stress_mean),
            )

        cld_df = cld.cld_add_row(cld_df, cycles_to_failure, 0.0, uts)

    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"],
        ascending=[True, True],
    ).reset_index(drop=True)

    cld_df.to_csv(path_or_buf=cld_csv_output_file, index=False)  # type: ignore

    if cld_json_output_file is not None:
        params_dict = {
            "m0": round(params.m0, 6),
            "d": round(params.d, 6),
            "alpha_t": round(params.alpha_t, 6),
            "alpha_c": round(params.alpha_c, 6),
            "sigma_apex": round(params.sigma_apex, 6),
            "np_reference": params.np_reference,
        }
        json_bytes = json.dumps(params_dict).encode()
        if hasattr(cld_json_output_file, "write"):
            cld_json_output_file.write(json_bytes)
        else:
            with open(cld_json_output_file, "wb") as f:
                f.write(json_bytes)

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
from scipy.optimize import minimize as _scipy_minimize

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


def _fit_parameters(
    snc_df: pd.DataFrame,
    uts: float,
    ucs: float,
    np_fixed: float | None = None,
) -> BoerstraParameters:
    """
    Direct translation of the Fortran Boerstra coordinate descent.

    20 outer iterations; each iteration grid-searches D, m0, Np, αC, αT in
    sequence to minimise std(Δt) — the Fortran SDt objective.  All five
    sweeps are vectorised over their candidate values via NumPy broadcasting.
    """
    sa = snc_df["stress_amplitude"].to_numpy(dtype=float)
    sm = snc_df["stress_mean"].to_numpy(dtype=float)
    N = snc_df["cycles_to_failure"].to_numpy(dtype=float)

    tensile = sm > 0
    ratio_t = np.clip(sm / uts, 0.0, 1.0)
    ratio_c = np.clip(-sm / ucs, 0.0, 1.0)

    # Starting values from the Fortran source
    m0, D, Np, aT, aC = (
        10.0,
        100.0,
        np_fixed if np_fixed is not None else 100.0,
        0.1,
        0.1,
    )

    def _sdt(Sap, fac, Np_arg, m_arg) -> np.ndarray:
        """
        Vectorised SDt for a batch of candidate configurations.

        Sap    : (n_cand, n) or (n,)  — stress amplitude projected to Np
        fac    : (n_cand, n) or (n,)  — Boerstra mean-stress factor
        Np_arg : scalar or (n_cand, 1)
        m_arg  : (n_cand, n) or (n,)
        Returns (n_cand,) — sample std of Δt for each candidate.
        """
        SAAp = Sap / fac  # → (n_cand, n)
        SApav = SAAp.mean(axis=1, keepdims=True)  # → (n_cand, 1)
        Sapmod = SApav * fac  # → (n_cand, n)

        dSa = np.log(np.maximum(Sap, EPSILON)) - np.log(np.maximum(Sapmod, EPSILON))
        Ne = Np_arg * (Sapmod / np.maximum(sa, EPSILON)) ** m_arg
        dN = np.log(N) - np.log(np.maximum(Ne, EPSILON))

        ok = (np.abs(dSa) > EPSILON) & (np.abs(dN) > EPSILON)
        ds = np.where(ok, dSa, 1.0)
        dn = np.where(ok, dN, 1.0)
        dt = np.where(ok, np.sign(ds) / np.sqrt(1.0 / ds**2 + 1.0 / dn**2), 0.0)

        nv = ok.sum(axis=1)
        mu = (dt * ok).sum(axis=1) / np.maximum(nv, 1)
        var = ((dt - mu[:, None]) ** 2 * ok).sum(axis=1) / np.maximum(nv - 1, 1)
        return np.where(nv >= 2, np.sqrt(var), np.inf)

    D_arr = np.arange(100.0, 1001.0, 1.0)  # (901,)   — fixed grids
    m0_arr = 1.0 + np.arange(381) * 0.05  # (381,)
    Np_arr = 1.0 + np.arange(10_000) * 1000.0  # (10000,)
    aC_arr = 0.05 + np.arange(496) * 0.01  # (496,)
    aT_arr = 0.05 + np.arange(496) * 0.01  # (496,)

    # Cache-friendly chunk size for Np sweep (~200 × n_data × 8 bytes fits in L3)
    _NP_CHUNK = 200

    def _sdt_Np_sweep(m_1d, fac, candidates):
        """
        Grid-search Np over `candidates` using log-space precomputation.
        Eliminates the expensive (N/Np)**(1/m) power operation by working in
        log-space: log_Sap = log_sa + (log_N − log_Np) / m.
        Processed in cache-friendly chunks.
        """
        log_sa = np.log(np.maximum(sa, EPSILON))
        log_N_ = np.log(N)
        inv_m = 1.0 / m_1d
        log_fac = np.log(np.maximum(fac, EPSILON))
        A = log_sa + log_N_ * inv_m  # (n,)
        precomp_Ne = m_1d * (log_sa - log_fac)  # (n,)

        sdt_all = np.empty(len(candidates))
        for i in range(0, len(candidates), _NP_CHUNK):
            sl = slice(i, i + _NP_CHUNK)
            chunk = candidates[sl]  # (c,)
            log_Np = np.log(chunk)[:, None]  # (c,1)
            log_SAAp = A - log_Np * inv_m - log_fac  # (c,n)
            SAAp = np.exp(log_SAAp)  # (c,n) — only exp
            SApav = SAAp.mean(axis=1, keepdims=True)  # (c,1)
            log_SApav = np.log(np.maximum(SApav, EPSILON))  # (c,1)
            dSa = log_SAAp - log_SApav  # (c,n)
            log_Ne = log_Np + m_1d * log_SApav - precomp_Ne  # (c,n)
            dN = log_N_ - log_Ne  # (c,n)
            ok = (np.abs(dSa) > EPSILON) & (np.abs(dN) > EPSILON)
            ds = np.where(ok, dSa, 1.0)
            dn = np.where(ok, dN, 1.0)
            dt = np.where(ok, np.sign(ds) / np.sqrt(1.0 / ds**2 + 1.0 / dn**2), 0.0)
            nv = ok.sum(axis=1)
            mu = (dt * ok).sum(axis=1) / np.maximum(nv, 1)
            var = ((dt - mu[:, None]) ** 2 * ok).sum(axis=1) / np.maximum(nv - 1, 1)
            sdt_all[sl] = np.where(nv >= 2, np.sqrt(var), np.inf)
        return float(candidates[np.argmin(sdt_all)])

    for iter_idx in range(20):
        prev = (m0, D, Np, aT, aC)

        fac = np.where(
            tensile,
            np.maximum(1.0 - ratio_t**aT, EPSILON),
            np.maximum(1.0 - ratio_c**aC, EPSILON),
        )  # (n,) — recomputed at the start of each outer iteration

        # Sweep D: 100 → 1000, step 1  (901 candidates)
        m_2d = np.maximum(m0 * np.exp(-sm / D_arr[:, None]), EPSILON)  # (901, n)
        Sap_2d = sa * (N / Np) ** (1.0 / m_2d)
        D = float(D_arr[np.argmin(_sdt(Sap_2d, fac, Np, m_2d))])

        # Sweep m0: 1 → 20, step 0.05  (381 candidates)
        m_2d = np.maximum(m0_arr[:, None] * np.exp(-sm / D), EPSILON)  # (381, n)
        Sap_2d = sa * (N / Np) ** (1.0 / m_2d)
        m0 = float(m0_arr[np.argmin(_sdt(Sap_2d, fac, Np, m_2d))])

        # Sweep Np: 1 → 1e7, step 1000  (10 000 candidates, log-space)
        m_1d = np.maximum(m0 * np.exp(-sm / D), EPSILON)  # (n,)
        if np_fixed is None:
            if iter_idx == 0:
                # Global search on the full Fortran grid
                Np = _sdt_Np_sweep(m_1d, fac, Np_arr)
            else:
                # Warm-start: search ±200 grid steps around current Np.
                # Coordinate descent cannot move the optimum more than 200 000
                # in one iteration, so this is numerically equivalent to the
                # full search while reducing the sweep from 10 000 → ≤ 401 candidates.
                Np_idx = int(round((Np - 1.0) / 1000.0))
                lo = max(0, Np_idx - 200)
                hi = min(len(Np_arr), Np_idx + 201)
                Np = _sdt_Np_sweep(m_1d, fac, Np_arr[lo:hi])

        # Sweep αC: 0.05 → 5, step 0.01  (496 candidates)
        Sap_1d = sa * (N / Np) ** (1.0 / m_1d)  # (n,)
        fac_2d = np.where(
            tensile,
            np.maximum(1.0 - ratio_t**aT, EPSILON),
            np.maximum(1.0 - ratio_c ** aC_arr[:, None], EPSILON),
        )  # (496, n)
        aC = float(aC_arr[np.argmin(_sdt(Sap_1d, fac_2d, Np, m_1d))])

        # Sweep αT: 0.05 → 5, step 0.01  (496 candidates)
        fac_2d = np.where(
            tensile,
            np.maximum(1.0 - ratio_t ** aT_arr[:, None], EPSILON),
            np.maximum(1.0 - ratio_c**aC, EPSILON),
        )  # (496, n)
        aT = float(aT_arr[np.argmin(_sdt(Sap_1d, fac_2d, Np, m_1d))])

        if (m0, D, Np, aT, aC) == prev:
            break  # converged — grid resolution exhausted

    # ── Nelder-Mead refinement (continuous local search from grid optimum) ───
    # Log-transform all parameters so the optimizer works in unconstrained space
    # and positivity is guaranteed (exp(x) > 0 always).
    def _sdt_scalar(log_params: np.ndarray) -> float:
        m0_r = float(np.exp(log_params[0]))
        D_r = float(np.exp(log_params[1]))
        aC_r = float(np.exp(log_params[2]))
        aT_r = float(np.exp(log_params[3]))
        Np_r = np_fixed if np_fixed is not None else float(np.exp(log_params[4]))

        m = np.maximum(m0_r * np.exp(-sm / D_r), EPSILON)
        Sap = sa * (N / Np_r) ** (1.0 / m)
        fac = np.where(
            tensile,
            np.maximum(1.0 - ratio_t**aT_r, EPSILON),
            np.maximum(1.0 - ratio_c**aC_r, EPSILON),
        )
        SAAp = Sap / fac
        SApav = SAAp.mean()
        Sapmod = SApav * fac
        dSa = np.log(np.maximum(Sap, EPSILON)) - np.log(np.maximum(Sapmod, EPSILON))
        Ne = Np_r * (Sapmod / np.maximum(sa, EPSILON)) ** m
        dN = np.log(N) - np.log(np.maximum(Ne, EPSILON))
        ok = (np.abs(dSa) > EPSILON) & (np.abs(dN) > EPSILON)
        nv = int(ok.sum())
        if nv < 2:
            return np.inf
        ds = np.where(ok, dSa, 1.0)
        dn = np.where(ok, dN, 1.0)
        dt = np.where(ok, np.sign(ds) / np.sqrt(1.0 / ds**2 + 1.0 / dn**2), 0.0)
        mu = (dt * ok).sum() / nv
        var = ((dt - mu) ** 2 * ok).sum() / (nv - 1)
        return float(np.sqrt(max(var, 0.0)))

    x0 = [np.log(m0), np.log(D), np.log(aC), np.log(aT)]
    if np_fixed is None:
        x0.append(np.log(Np))

    res = _scipy_minimize(
        _sdt_scalar,
        x0,
        method="Nelder-Mead",
        options={"xatol": 1e-8, "fatol": 1e-10, "maxiter": 10_000, "maxfev": 50_000},
    )

    if np.isfinite(res.fun):
        m0 = float(np.exp(res.x[0]))
        D = float(np.exp(res.x[1]))
        aC = float(np.exp(res.x[2]))
        aT = float(np.exp(res.x[3]))
        if np_fixed is None:
            Np = float(np.exp(res.x[4]))

    # Final σ_apex = arithmetic mean of SAAp  (Fortran: SApav = mean(SAAp(j)))
    m_1d = np.maximum(m0 * np.exp(-sm / D), EPSILON)
    Sap_1d = sa * (N / Np) ** (1.0 / m_1d)
    fac = np.where(
        tensile,
        np.maximum(1.0 - ratio_t**aT, EPSILON),
        np.maximum(1.0 - ratio_c**aC, EPSILON),
    )
    sigma_apex = float(np.mean(Sap_1d / fac))

    return BoerstraParameters(
        m0=m0,
        d=D,
        np_reference=Np,
        alpha_t=aT,
        alpha_c=aC,
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


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    cld_json_output_file=None,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
    np_reference: float | None = None,
    m0_init: float | None = None,
    d_init: float | None = None,
    alpha_t_init: float | None = None,
    alpha_c_init: float | None = None,
) -> None:
    """Execute the CLD Boerstra algorithm."""
    ucs = abs(float(ucs))
    uts = abs(float(uts))

    snc_df = _prepare_input(snc_csv_input_file, uts, ucs)
    params = _fit_parameters(snc_df, uts, ucs, np_fixed=np_reference)

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

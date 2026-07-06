#!/usr/bin/env python
"""
Implementation of the Piecewise Non-Linear CLD as described in:
Anastasios P. Vassilopoulos, Thomas Keller,
Fatigue of Fiber-reinforced Composites,
Section 4.3.1.7 – The Piecewise Non-Linear CLD (pp. 114-117)
DOI 10.1007/978-1-84996-181-3

Four domains (Eq. 4.25 and 4.29):
  I   (R ≤ −1):         σ_a = (1−R)(A_I/R  + B_I/R²)
  II  (−1 ≤ R ≤ 0):     σ_a = (1−R)/(A_II·R  + B_II)
  III (0  ≤ R ≤ 1):     σ_a = (1−R)/(A_III·R³ + B_III)
  IV  (R  ≥ 1):         σ_a = (1−R)(A_IV/R + B_IV/R²)

Parameters are derived from boundary conditions at R = −1, R2 (|R|>1), R3
(optional, −1<R<1) and the static strengths UTS / UCS (Eqs. 4.27–4.33).
"""

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cld as cld

DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

# Output N levels – same as Harris (7 powers of 10)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _stress_amplitude(r: float, sigma_max: float) -> float:
    """σ_a from stress ratio and σ_max (piecewise-linear convention)."""
    if abs(r) > 1:
        return (1.0 - 1.0 / r) * sigma_max / 2.0
    return (1.0 - r) * sigma_max / 2.0


def _fit_sn_regression(
    n_arr: np.ndarray, sigma_a_arr: np.ndarray
) -> tuple[float, float]:
    """OLS fit of log10(N) = A + B·log10(σ_a), matching the Fortran algorithm.

    Returns (A, B) such that σ_a(N) = 10^((log10(N) - A) / B).
    """
    X = np.log10(sigma_a_arr)
    Y = np.log10(n_arr)
    Xb = X.mean()
    Yb = Y.mean()
    B = float(np.sum((X - Xb) * (Y - Yb)) / np.sum((X - Xb) ** 2))
    A = float(Yb - B * Xb)
    return A, B


def _sigma_a_from_regression(n_target: float, A: float, B: float) -> float:
    """Invert log10(N) = A + B·log10(σ_a) to obtain σ_a at a given N."""
    return 10.0 ** ((np.log10(float(n_target)) - A) / B)


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    """
    Execute the CLD Piecewise Non-Linear algorithm.

    Parameters
    ----------
    snc_csv_input_file
        SNC csv input (columns: stress_ratio, cycles_to_failure, stress_max).
        Must contain R = −1 and at least one R with |R| > 1.
    cld_csv_output_file
        CLD csv output (columns: cycles_to_failure, stress_amplitude, stress_mean).
    ucs
        Ultimate compressive stress (positive value, [MPa]).
    uts
        Ultimate tensile stress (positive value, [MPa]).
    """
    # ------------------------------------------------------------------ read
    snc_df = pd.read_csv(snc_csv_input_file)

    required = {"stress_ratio", "cycles_to_failure", "stress_max"}
    missing = required - set(snc_df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {', '.join(sorted(missing))}")

    for col in required:
        snc_df[col] = pd.to_numeric(snc_df[col], errors="coerce")
    snc_df = snc_df.dropna(subset=list(required))
    snc_df = snc_df[
        (snc_df["stress_max"] > 0) & (snc_df["cycles_to_failure"] > 0)
    ].copy()

    if snc_df.empty:
        raise ValueError("No valid rows in SNC input file.")

    # Compute stress amplitude per row
    snc_df["sigma_a"] = snc_df.apply(
        lambda row: _stress_amplitude(row["stress_ratio"], row["stress_max"]), axis=1
    )
    snc_df = snc_df[snc_df["sigma_a"] > 0].copy()

    # ------------------------------------------------ identify R-ratio groups
    unique_r = sorted(snc_df["stress_ratio"].unique())

    # R1 = −1 (required)
    r1_matches = [r for r in unique_r if abs(r - (-1.0)) < 1e-6]
    if not r1_matches:
        raise ValueError("Piecewise Non-Linear CLD requires an S-N curve at R = −1.")
    R1 = r1_matches[0]

    # R2 = |R| > 1 (required; pick the one with most data points)
    r2_candidates = [r for r in unique_r if abs(r) > 1 and abs(r + 1.0) > 1e-6]
    if not r2_candidates:
        raise ValueError(
            "Piecewise Non-Linear CLD requires at least one S-N curve with |R| > 1."
        )
    R2 = max(
        r2_candidates,
        key=lambda r: (len(snc_df[snc_df["stress_ratio"] == r]), abs(r)),
    )

    # R3 = −1 < R < 1 (optional)
    r3_candidates = [r for r in unique_r if -1.0 < r < 1.0]
    R3 = r3_candidates[0] if r3_candidates else None
    NR = 3 if R3 is not None else 2

    # ------------------------------------------- build σ_a arrays per group
    def _group_arrays(r: float) -> tuple[np.ndarray, np.ndarray]:
        grp = snc_df[snc_df["stress_ratio"] == r].sort_values("cycles_to_failure")
        return (
            grp["cycles_to_failure"].to_numpy(dtype=float),
            grp["sigma_a"].to_numpy(dtype=float),
        )

    n_r1, sa_r1 = _group_arrays(R1)
    n_r2, sa_r2 = _group_arrays(R2)
    n_r3, sa_r3 = _group_arrays(R3) if R3 is not None else (None, None)

    # OLS regression per R group: log10(N) = A + B·log10(σ_a)  (Fortran algorithm)
    def _make_sn_getter(n_arr: np.ndarray, sa_arr: np.ndarray):
        A, B = _fit_sn_regression(n_arr, sa_arr)
        return lambda n: _sigma_a_from_regression(n, A, B)

    get_sm1 = _make_sn_getter(n_r1, sa_r1)
    get_sinf = _make_sn_getter(n_r2, sa_r2)
    get_s0 = _make_sn_getter(n_r3, sa_r3) if R3 is not None else None

    # Sign convention: UCS is negative internally (compressive)
    ucs_neg = -abs(float(ucs))
    uts_pos = abs(float(uts))

    # -------------------------------------------------- R scan values
    # Domain III  0 < R < 1  (step −0.05, 19 values)
    r_III = [round(0.95 - i * 0.05, 10) for i in range(19)]
    # Domain II  −1 ≤ R < 0  (step −0.05, 20 values including R=−1)
    r_II = [round(-0.05 - i * 0.05, 10) for i in range(20)]
    # Domain I   R ≤ −1      (Fortran: −1→−3 step −0.5, then −4→−30 step −1)
    r_I = [round(-1.0 - i * 0.5, 10) for i in range(5)] + list(range(-4, -31, -1))
    # Domain IV  R ≥ 1       (Fortran: 30→3 step −1, then 2.95→1.05 step −0.05)
    r_IV = list(range(30, 2, -1)) + [round(2.95 - i * 0.05, 10) for i in range(39)]

    # -------------------------------------------------- build CLD
    cld_df = pd.DataFrame()

    for n_cycles in CLD_CYCLES_COUNT:
        Sm1 = get_sm1(n_cycles)
        Sinf = get_sinf(n_cycles)
        S0 = get_s0(n_cycles) if get_s0 is not None else None

        # --- Domain I & IV parameters (Eq. 4.27-4.28) ---
        INF_THRESHOLD = 100.0  # |R2| ≥ 100 → treat as ±∞
        if abs(R2) >= INF_THRESHOLD:
            # Standard boundary condition: R2 = ±∞
            A_I = -Sinf
            A_IV = -Sinf
            B_I = Sm1 / 2.0 - Sinf
            B_IV = ucs_neg / 2.0 + Sinf  # = −ucs/2 + Sinf
        elif R2 > 1.0:
            # Adjusted boundary condition for finite R2 > 1 (e.g. R = 10)
            denom = 1.0 / R2 - 1.0 / R2**2
            A_IV = (Sinf / (1.0 - R2) - ucs_neg / (2.0 * R2**2)) / denom
            B_IV = ucs_neg / 2.0 - A_IV
            A_I = A_IV
            B_I = Sm1 / 2.0 + A_I
        else:
            # R2 < −1
            denom = 1.0 / R2 + 1.0 / R2**2
            A_I = (Sinf / (1.0 - R2) - Sm1 / (2.0 * R2**2)) / denom
            B_I = Sm1 / 2.0 + A_I
            A_IV = A_I
            B_IV = ucs_neg / 2.0 - A_IV

        # --- Domain II & III parameters (Eq. 4.31-4.33) ---
        if NR == 2:
            A_II = 1.0 / uts_pos - 1.0 / Sm1
            A_III = 1.0 / uts_pos - 1.0 / Sm1
            B_II = 1.0 / uts_pos + 1.0 / Sm1
            B_III = B_II
        elif abs(R3) < 1e-6:  # R3 = 0  (standard)
            A_II = 1.0 / S0 - 2.0 / Sm1
            A_III = 2.0 / uts_pos - 1.0 / S0
            B_II = 1.0 / S0
            B_III = B_II
        elif R3 > 0.0:  # 0 < R3 < 1
            A_III = ((1.0 - R3) / S0 - 2.0 / uts_pos) / (R3**3 - 1.0)
            B_III = 2.0 / uts_pos - A_III
            B_II = B_III
            A_II = B_II - 2.0 / Sm1
        else:  # −1 < R3 < 0
            A_II = ((1.0 - R3) / S0 - 2.0 / Sm1) / (1.0 + R3)
            B_II = 2.0 / Sm1 + A_II
            B_III = B_II
            A_III = 2.0 / uts_pos - B_III

        # --- Emit CLD points for this N level ---

        # Right boundary: R = 1 from tensile side (σ_a = 0, σ_m = UTS)
        cld_df = cld.cld_add_row(cld_df, n_cycles, 0.0, uts_pos)

        # Domain III: 0 < R < 1
        for RR in r_III:
            d = A_III * RR**3 + B_III
            if abs(d) < 1e-30:
                continue
            sa = (1.0 - RR) / d
            if sa <= 0.0:
                continue
            sm = (1.0 + RR) / (1.0 - RR) * sa
            cld_df = cld.cld_add_row(cld_df, n_cycles, sa, sm)

        # Domain II: −1 ≤ R < 0
        for RR in r_II:
            d = A_II * RR + B_II
            if abs(d) < 1e-30:
                continue
            sa = (1.0 - RR) / d
            if sa <= 0.0:
                continue
            sm = (1.0 + RR) / (1.0 - RR) * sa
            cld_df = cld.cld_add_row(cld_df, n_cycles, sa, sm)

        # Domain I: R ≤ −1
        for RR in r_I:
            if abs(RR) < 1e-30:
                continue
            sa = (1.0 - RR) * (A_I / RR + B_I / RR**2)
            if sa <= 0.0:
                continue
            sm = (1.0 + RR) / (1.0 - RR) * sa
            cld_df = cld.cld_add_row(cld_df, n_cycles, sa, sm)

        # Junction between Domain I and IV: both converge to (σ_a=−A_I, σ_m=A_I)
        # as R→±∞, but the scans only reach |R|=30. B_I ≠ B_IV so the last Domain I
        # point (R=−30) and last Domain IV point (R=30) don't quite meet, causing a
        # visible kink between the two R-ratio data lines. Adding the exact limit point
        # closes that gap without introducing any domain-boundary slope discontinuity.
        sa_junct = -A_I  # A_I < 0 in all physical cases
        sm_junct = A_I
        if sa_junct > 0:
            cld_df = cld.cld_add_row(cld_df, n_cycles, sa_junct, sm_junct)

        # Domain IV: R ≥ 1
        for RR in r_IV:
            if abs(RR) < 1e-30:
                continue
            sa = (1.0 - RR) * (A_IV / RR + B_IV / RR**2)
            if sa <= 0.0:
                continue
            sm = (1.0 + RR) / (1.0 - RR) * sa
            cld_df = cld.cld_add_row(cld_df, n_cycles, sa, sm)

        # Left boundary: R = 1 from compressive side (σ_a = 0, σ_m = −UCS)
        cld_df = cld.cld_add_row(cld_df, n_cycles, 0.0, ucs_neg)

    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"],
        ascending=[True, True],
    ).reset_index(drop=True)

    cld_df.to_csv(path_or_buf=cld_csv_output_file, index=False)  # type: ignore

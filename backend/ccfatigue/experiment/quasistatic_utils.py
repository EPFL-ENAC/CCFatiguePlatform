"""
quasistatic_utils.py
General-purpose mechanical‑property helpers for quasi‑static tests
(toughness, Young modulus, Poisson ratio).
"""

from __future__ import annotations
from typing import List, Optional

import numpy as np
from sklearn.linear_model import LinearRegression

__all__ = [
    "calc_toughness",
    "calc_young_modulus",
    "calc_poisson_ratio",
]

# ------------------------------------------------------------------
#  Toughness  (area under the stress–strain curve)
# ------------------------------------------------------------------

def calc_toughness(
    stress: Optional[List[float]],
    strain: Optional[List[float]],
) -> float | None:
    """Return ∫ σ dε (MPa · strain) up to the shorter vector length.
    Units come out as J mm⁻³; caller may post‑convert if desired.
    """
    if stress is None or strain is None:
        return None
    if len(stress) < 2 or len(strain) < 2:
        return None

    n = min(len(stress), len(strain))
    return float(np.trapz(stress[:n], strain[:n]))


# ------------------------------------------------------------------
#  Young modulus  (0.15 % – 0.35 % engineering‑strain window)
# ------------------------------------------------------------------

def calc_young_modulus(
    stress: Optional[List[float]],
    strain: Optional[List[float]],
    eps_low: float = 0.0015,
    eps_high: float = 0.0035,
) -> float | None:
    """Linear regression of σ vs ε in the given window; returns *GPa*."""
    if stress is None or strain is None:
        return None

    sig = np.asarray(stress, dtype=float)
    eps = np.asarray(strain, dtype=float)
    mask = (eps >= eps_low) & (eps <= eps_high)
    if mask.sum() < 2:
        return None

    reg = LinearRegression().fit(eps[mask].reshape(-1, 1), sig[mask])
    return float(reg.coef_[0]) / 1000.0  # MPa → GPa


# ------------------------------------------------------------------
#  Poisson ratio  (−εyy / εxx in the same window)
# ------------------------------------------------------------------

def calc_poisson_ratio(
    exx: Optional[List[float]],
    eyy: Optional[List[float]],
    eps_low: float = 0.0015,
    eps_high: float = 0.0035,
) -> float | None:
    """Return ν from linear fit of −εyy vs εxx.

    The function now considers the *eyy* column *unusable* when it is
    absent **or** effectively zero within the analysis window. In such
    cases it returns **None** so that the UI can show “–” instead of
    *0.00*.
    """
    # --- column checks ------------------------------------------------
    if exx is None or eyy is None:
        return None  # eyy truly missing

    exx_arr = np.asarray(exx, dtype=float)
    eyy_arr = np.asarray(eyy, dtype=float)
    if exx_arr.size != eyy_arr.size or exx_arr.size == 0:
        return None  # length mismatch / empty

    # --- window selection ---------------------------------------------
    mask = (exx_arr >= eps_low) & (exx_arr <= eps_high)
    if mask.sum() < 2:
        return None  # not enough points

    # If εyy is *practically* zero in the window, treat as missing
    if np.all(np.abs(eyy_arr[mask]) < 1e-8):
        return None

    # --- linear fit ---------------------------------------------------
    reg = LinearRegression().fit(
        exx_arr[mask].reshape(-1, 1),
        -eyy_arr[mask]
    )
    nu = float(reg.coef_[0])

    # Guard against numerical noise (slope ≈ 0 → treat as None)
    if abs(nu) < 1e-6:
        return None

    return nu

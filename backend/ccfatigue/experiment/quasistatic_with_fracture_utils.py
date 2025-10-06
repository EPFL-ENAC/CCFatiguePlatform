"""quasistatic_with_fracture_utils.py
Utility functions for quasi‑static with fracture tests with crack‑length compliance fitting
and three energy‑release‑rate methods (MBT, MCC, ECM).
Extracted from the original `quasi_static_test` pipeline so they can be
imported and unit‑tested independently.
"""

from __future__ import annotations

import math
from typing import List, Tuple

import numpy as np
from numpy.typing import ArrayLike
from scipy.optimize import curve_fit
from sklearn.linear_model import LinearRegression

__all__ = [
    "fit_func",
    "fit_crack_length_from_compliance",
    "compute_compliance_and_crack_length_fitted",
    "compute_factors",
    "find_plateau_start",
    "compute_g_stats",
    "compute_g_mbt",
    "compute_g_mcc",
    "compute_g_ecm",
]

# ----------------------------------------------------------------------
#  Generic helpers
# ----------------------------------------------------------------------

def fit_func(x: ArrayLike, a: float, b: float) -> ArrayLike:
    """Linear model a·x + b (used by curve_fit)."""
    return a * x + b


# ----------------------------------------------------------------------
#  Compliance → crack‑length back‑calculation (power law)
# ----------------------------------------------------------------------

def fit_crack_length_from_compliance(
    crack_length: List[float],
    crack_displacement: List[float],
    crack_load: List[float],
) -> List[float]:
    """Fit *compliance = a · a^m* and invert to obtain *a_fitted* for every point."""
    compliance = np.asarray(crack_displacement) / np.asarray(crack_load)
    a_array    = np.asarray(crack_length)

    def _power(x, a, m):  # noqa: ANN001 – local model for curve_fit
        return a * x ** m

    popt, _ = curve_fit(_power, a_array, compliance, maxfev=2000)
    a_fit, m_fit = popt
    return (compliance / a_fit) ** (1.0 / m_fit)


def compute_compliance_and_crack_length_fitted(
    crack_displacement: List[float],
    crack_load: List[float],
    crack_length: List[float],
) -> Tuple[np.ndarray, np.ndarray]:
    """Return compliance vector *and* fitted crack length as NumPy arrays."""
    compliance = np.asarray(crack_displacement) / np.asarray(crack_load)
    a_fitted   = np.asarray(
        fit_crack_length_from_compliance(crack_length, crack_displacement, crack_load)
    )
    return compliance, a_fitted

# ----------------------------------------------------------------------
#  Corrective factors F and N (Norwood–Paris ASTM)
# ----------------------------------------------------------------------

def compute_factors(
    crack_displacement: List[float],
    crack_load: List[float],  # kept for signature symmetry – not used here
    crack_length_fitted: np.ndarray,
    thickness: float,
    t: float,
    l_prime: float,
) -> Tuple[np.ndarray, np.ndarray]:
    displacement = np.asarray(crack_displacement)
    a            = crack_length_fitted

    F = 1.0 - (3.0 / 10.0) * (displacement / a) ** 2 - (3.0 / 2.0) * (
        (displacement * t) / a**2
    )
    N = (
        1.0
        - (l_prime / a) ** 3
        - (9.0 / 8.0) * (1.0 - (l_prime / a) ** 2) * (displacement * t) / a**2
        - (9.0 / 35.0) * (displacement / a) ** 2
    )
    return F, N

# ----------------------------------------------------------------------
#  Plateau metrics (g_init, bridging length, g_plateau)
# ----------------------------------------------------------------------

def find_plateau_start(
    crack_length: List[float],
    fracture_energy: List[float],
    threshold: float = 40.0,
) -> float:
    """Return first *a* where |dG/da| < *threshold* (J/m²·mm⁻¹)."""
    if len(crack_length) < 2:
        return math.nan

    a_arr = np.asarray(crack_length)
    G_arr = np.asarray(fracture_energy)

    derivative = np.gradient(G_arr, a_arr)
    candidates = np.where((a_arr > a_arr[0]) & (np.abs(derivative) < threshold))[0]
    return a_arr[candidates[0]] if candidates.size else math.nan


def compute_g_stats(
    compliance: np.ndarray,
    fracture_energy: List[float],
    crack_length_fitted: np.ndarray,
    threshold: float = 10.0,
) -> Tuple[float, float, float]:
    """Return g_init, bridging length, g_plateau as defined in the notebook."""
    G_arr = np.asarray(fracture_energy)

    # g_init = G where compliance exceeds its mean by 1 %
    linear_c = compliance[:5].mean()
    idx_thr  = np.where(compliance >= linear_c * 1.01)[0]
    g_init   = float(G_arr[idx_thr[0]]) if idx_thr.size else math.nan

    # bridging length
    bridge = find_plateau_start(crack_length_fitted.tolist(), fracture_energy, threshold)

    # g_plateau = average G beyond bridging length
    if math.isnan(bridge):
        g_plateau = math.nan
    else:
        mask = crack_length_fitted >= bridge
        g_plateau = float(G_arr[mask].mean()) if mask.any() else math.nan

    return g_init, bridge, g_plateau

# ----------------------------------------------------------------------
#  Energy‑release‑rate formulas
# ----------------------------------------------------------------------

def compute_g_mbt(
    compliance: np.ndarray,
    crack_displacement: List[float],
    crack_load: List[float],
    crack_length_fitted: np.ndarray,
    F: np.ndarray,
    N: np.ndarray,
    width: float,
):
    """Modified Beam Theory."""
    C_N_1_3 = (compliance / N) ** (1.0 / 3.0)
    (a_lin, b_lin), _ = curve_fit(fit_func, crack_length_fitted, C_N_1_3)
    delta = abs(b_lin / a_lin)  # Norwood triangle

    G = (
        3.0
        * np.asarray(crack_load)
        * np.asarray(crack_displacement)
        / (2.0 * width * (crack_length_fitted + delta))
        * F
        / N
        * 1e6
    )
    return G.tolist(), *compute_g_stats(compliance, G.tolist(), crack_length_fitted)


def compute_g_mcc(
    compliance: np.ndarray,
    crack_displacement: List[float],
    crack_load: List[float],
    crack_length_fitted: np.ndarray,
    F: np.ndarray,
    N: np.ndarray,
    width: float,
    thickness: float,
):
    """Compliance‑Calibration (MCC)."""
    a_over_h  = crack_length_fitted / thickness
    C_N_1_3   = (compliance / N) ** (1.0 / 3.0)
    (A1, _), _ = curve_fit(fit_func, C_N_1_3, a_over_h)

    G = (
        3.0
        * np.asarray(crack_load) ** 2
        * (compliance / N) ** (2.0 / 3.0)
        / (2.0 * A1 * width * thickness)
        * F
        * 1e6
    )
    return G.tolist(), *compute_g_stats(compliance, G.tolist(), crack_length_fitted)


def compute_g_ecm(
    compliance: np.ndarray,
    crack_displacement: List[float],
    crack_load: List[float],
    crack_length_fitted: np.ndarray,
    F: np.ndarray,
    N: np.ndarray,
    width: float,
):
    """Extended Compliance Method (ECM)."""
    a_log   = np.log(crack_length_fitted) / math.log(10)
    ln_C_N  = np.log(compliance / N) / math.log(10)
    (m_slope, _), _ = curve_fit(fit_func, a_log, ln_C_N)

    G = (
        m_slope
        * np.asarray(crack_load)
        * np.asarray(crack_displacement)
        / (2.0 * width * crack_length_fitted)
        * F
        / N
        * 1e6
    )
    return G.tolist(), *compute_g_stats(compliance, G.tolist(), crack_length_fitted)

import math
from typing import List, Tuple, Optional
import numpy as np
from sklearn.linear_model import LinearRegression
from scipy.optimize import curve_fit

def fit_func(x, a, b):
    return a * x + b

def fit_crack_length_from_compliance(crack_length: List[float], crack_displacement: List[float], crack_load: List[float]) -> List[float]:
    compliance = np.array(crack_displacement) / np.array(crack_load)
    crack_length_array = np.array(crack_length)

    def power_law(x, a, m):
        return a * x ** m

    popt, _ = curve_fit(power_law, crack_length_array, compliance)
    a_fit, m_fit = popt
    crack_length_fitted = (compliance / a_fit) ** (1 / m_fit)
    return crack_length_fitted.tolist()

def compute_compliance_and_crack_length_fitted(crack_displacement, crack_load, crack_length) -> Tuple[np.ndarray, np.ndarray]:
    compliance = np.array(crack_displacement) / np.array(crack_load)
    crack_length_fitted = np.array(
        fit_crack_length_from_compliance(crack_length, crack_displacement, crack_load)
    )
    return compliance, crack_length_fitted

def compute_factors(crack_displacement, crack_length_fitted, t, l_prime):
    F = (
        1
        - 3/10 * (np.array(crack_displacement) / crack_length_fitted)**2
        - 3/2 * ((np.array(crack_displacement) * t) / crack_length_fitted**2)
    )
    N = (
        1
        - (l_prime / crack_length_fitted)**3
        - 9/8 * (1 - (l_prime / crack_length_fitted)**2) * (np.array(crack_displacement) * t) / crack_length_fitted**2
        - 9/35 * (np.array(crack_displacement) / crack_length_fitted)**2
    )
    return F, N

def compute_g_mbt(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, w):
    C_N_1_3 = (compliance / N)**(1/3)
    params, _ = curve_fit(fit_func, crack_length_fitted, C_N_1_3)
    a, b = params
    triangle = abs(b / a)

    G = (
        (3 * np.array(crack_load) * np.array(crack_displacement))
        / (2 * w * (crack_length_fitted + triangle))
        * F / N
        * 1e6
    )
    return G.tolist()

def compute_g_mcc(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width, thickness):
    a_over_h = crack_length_fitted / thickness
    C_N_1_3 = (compliance / N)**(1/3)
    # A1, _ = np.polyfit(C_N_1_3, a_over_h, 1)
    params, _ = curve_fit(fit_func, C_N_1_3, a_over_h)
    A1 = params[0]          # slope
    G = (3 * np.array(crack_load)**2 * (compliance / N)**(2/3)) / (2 * A1 * width * thickness) * F * 1e6
    return G.tolist()

def compute_g_ecm(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width):
    crack_array = crack_length_fitted
    c_over_n = compliance / N
    #valid_mask = (crack_array > 0) & (c_over_n > 0) & np.isfinite(c_over_n)
    #if np.sum(valid_mask) < 2:
    #    return [float("nan")] * len(crack_array)

    #a_log = np.log10(crack_array[valid_mask])
    #log_C_N = np.log10(c_over_n[valid_mask])
    #m, _ = np.polyfit(a_log, log_C_N, 1)
    a_log      = np.log(crack_length_fitted) / np.log(10)
    log_C_N    = np.log(compliance / N)      / np.log(10)

    params, _ = curve_fit(fit_func, a_log, log_C_N)
    m = params[0]           # slope

    G = (m * np.array(crack_load) * np.array(crack_displacement)) / (2 * width * crack_array) * F / N * 1e6
    return G.tolist()

def find_fit_limit(G, da_dn, slope_threshold=0.04, window_size=5):
    """
    Performs a linear fit on the initial lower-left points of the crack growth data
    and returns the value of G at which the slope of the fit drops below a specified threshold.

    Parameters:
    - G: list of G values (fracture energy)
    - da_dn: list of da/dN values (crack growth rate)
    - slope_threshold: minimum slope value to continue fitting (default = 0.05)
    - window_size: minimum number of points used for each incremental fit

    Returns:
    - G_stop: the G value at which the slope drops below the threshold
    - final_slope: the last slope value calculated before falling below the threshold
    """
    # Sort the data to start from the lower-left region
    G_sorted = G[::-1]
    da_dn_sorted = da_dn[::-1]
    log_da_dn_sorted = np.log10(da_dn_sorted)

    # Incremental linear fitting
    for i in range(window_size, len(G_sorted)):
        x = np.array(G_sorted[:i]).reshape(-1, 1)
        y = np.array(log_da_dn_sorted[:i])
        reg = LinearRegression().fit(x, y)
        slope = reg.coef_[0]

        if abs(slope) < slope_threshold:
            G_stop = G_sorted[i - 1]
            return G_stop, slope

    return G_sorted[-1], slope  # No threshold breach, return last point

def find_best_paris_fit(
    G: List[float],
    da_dn: List[float],
    da_dn_high: float = 1e-3,   # high threshold (≈ 10-3 mm/cycle)
    da_dn_low: float  = 1e-5,   # low threshold (≈ 10-5 mm/cycle)
    min_points: int   = 3
) -> Tuple[float, float, float]:
    """
    Exactly reproduces the procedure from the notebook:
    1.  transforms G and da/dN to natural log.
    2.  finds the index closest to da_dn_high and the one
        closest to da_dn_low.
    3.  takes *all* points between the two indices (contiguous slice).
    4.  performs a linear regression log-log and returns:
        m, C, R².

    Parameters
    ----------
    G          : list of G (J/m²)
    da_dn      : list of da/dN (mm/cycle)
    da_dn_high : upper threshold to choose the start
    da_dn_low  : lower threshold to choose the end
    min_points : minimum points required for the fit
    """
    G_arr     = np.asarray(G, dtype=float)
    da_dn_arr = np.asarray(da_dn, dtype=float)

    log_G      = np.log(G_arr)
    log_da_dn  = np.log(da_dn_arr)

    # --- 1. find the indices “closest” to the two thresholds ---
    idx_high = int(np.argmin(np.abs(da_dn_arr - da_dn_high)))
    idx_low  = int(np.argmin(np.abs(da_dn_arr - da_dn_low)))

    # ensure that start < end
    start, end = sorted([idx_high, idx_low])

    if end - start + 1 < min_points:
        raise ValueError(
            f"Window too small: {end-start+1} points (<{min_points})"
        )

    # --- 2. linear regression log-log on the contiguous interval ---
    x = log_G[start : end + 1].reshape(-1, 1)
    y = log_da_dn[start : end + 1]

    reg = LinearRegression().fit(x, y)
    m      = float(reg.coef_[0])        # slope
    log_C  = float(reg.intercept_)
    r2     = float(reg.score(x, y))
    C      = math.exp(log_C)

    return m, C, r2


def compute_da_dn(crack_length, crack_n_cycles):
    """
    Computes da/dN exactly following the sequence:
    i=0, i=1, i=2, central loop (i=3..N-4), i=N-3, i=N-2, i=N-1
    as in the "manual" block, but using two arrays:
    - crack_length_fitted: list or array of fitted crack lengths
    - crack_n_cycles:      list or array of corresponding cycle numbers

    Returns:
    - da_dn_new: list of da/dN values calculated point by point
    """
    def poly_func(x, b0, b1, b2):
        return b0 + b1 * x + b2 * x**2

    da_dn_new = []
    a_estimated = []

    
    a_arr = np.array(crack_length)
    N_arr = np.array(crack_n_cycles)
    N = len(a_arr)

    
    a_0 = 0.5 * (a_arr[1] + a_arr[0])
    a_estimated.append(a_0)
    da_dn_0 = (a_arr[1] - a_arr[0]) / (N_arr[1] - N_arr[0])
    da_dn_new.append(da_dn_0)

    
    C1 = 0.5 * (N_arr[0] + N_arr[2])
    C2 = 0.5 * (N_arr[2] - N_arr[0])
    # x_vals and y_vals for i=1
    x_vals = (N_arr[0:3] - C1) / C2
    y_vals = a_arr[0:3]
    popt, _ = curve_fit(poly_func, x_vals, y_vals)
    # estimate a_1 at the central point (index 1 of block 0:3)
    xi = x_vals[1]
    a_1_estimated = popt[0] + popt[1] * xi + popt[2] * xi**2
    a_estimated.append(a_1_estimated)
    da_dn_1 = popt[1] / C2 + (2 * popt[2] * (N_arr[1] - C1)) / (C2**2)
    da_dn_new.append(da_dn_1)

    # --- Point i = 2 (window on indices 0..4) ---
    C1 = 0.5 * (N_arr[0] + N_arr[4])
    C2 = 0.5 * (N_arr[4] - N_arr[0])
    x_vals = (N_arr[0:5] - C1) / C2
    y_vals = a_arr[0:5]
    popt2, _ = curve_fit(poly_func, x_vals, y_vals)
    xi = x_vals[2]
    a_2_estimated = popt2[0] + popt2[1] * xi + popt2[2] * xi**2
    a_estimated.append(a_2_estimated)
    da_dn_2 = popt2[1] / C2 + (2 * popt2[2] * (N_arr[2] - C1)) / (C2**2)
    da_dn_new.append(da_dn_2)

    # --- Main loop i = 3 .. N-4 (window on 7 points) ---
    for i in range(3, N - 3):
        C1 = 0.5 * (N_arr[i - 3] + N_arr[i + 3])
        C2 = 0.5 * (N_arr[i + 3] - N_arr[i - 3])
        x_vals = (N_arr[i - 3 : i + 4] - C1) / C2
        y_vals = a_arr[i - 3 : i + 4]
        popt3, _ = curve_fit(poly_func, x_vals, y_vals)
        # estimate a_i at the central point (index 3 of block i-3:i+4)
        xi = x_vals[3]
        a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
        a_estimated.append(a_3_estimated)
        da_dn_3 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
        da_dn_new.append(da_dn_3)

    # --- Point i = N-3 (window on indices N-5..N-1) ---
    i = N - 3
    C1 = 0.5 * (N_arr[i - 2] + N_arr[i + 2])
    C2 = 0.5 * (N_arr[i + 2] - N_arr[i - 2])
    x_vals = (N_arr[i - 2 : i + 3] - C1) / C2
    y_vals = a_arr[i - 2 : i + 3]
    popt3, _ = curve_fit(poly_func, x_vals, y_vals)
    xi = x_vals[2]  # corresponds to the central index i
    a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
    a_estimated.append(a_3_estimated)
    da_dn_4 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
    da_dn_new.append(da_dn_4)

    # --- Point i = N-2 (window on indices N-3..N-1) ---
    i = N - 2
    C1 = 0.5 * (N_arr[i - 1] + N_arr[i + 1])
    C2 = 0.5 * (N_arr[i + 1] - N_arr[i - 1])
    x_vals = (N_arr[i - 1 : i + 2] - C1) / C2
    y_vals = a_arr[i - 1 : i + 2]
    popt3, _ = curve_fit(poly_func, x_vals, y_vals)
    xi = x_vals[1]  # central index of the 3-point block
    a_3_estimated = popt3[0] + popt3[1] * xi + popt3[2] * xi**2
    a_estimated.append(a_3_estimated)
    da_dn_5 = popt3[1] / C2 + (2 * popt3[2] * (N_arr[i] - C1)) / (C2**2)
    da_dn_new.append(da_dn_5)

    # --- Point i = N-1 (last point, simple derivative on two values) ---
    i = N - 1
    a_last = 0.5 * (a_arr[N - 2] + a_arr[N - 1])
    a_estimated.append(a_last)
    da_dn_last = (a_arr[N - 1] - a_arr[N - 2]) / (N_arr[N - 1] - N_arr[N - 2])
    da_dn_new.append(da_dn_last)

    return da_dn_new

def find_paris_fit_auto(
    G: List[float],
    da_dn: List[float],
    min_points,
    r2_threshold,
) -> Tuple[float, float, float, float, float]:
    """
    1) Exhaustive search of every window (length >= min_points) to find
       the slice with the *highest* R² (log(da/dN) vs log(G)).
    2) Starting from that slice, expand outward as long as the new window
       keeps R² >= r2_threshold.
    3) Return m, C, final R², and the da/dN values at the window edges.
    """

    # ---------------- sanity check -----------------------------------
    if len(G) < min_points:
        raise ValueError(f"Need at least {min_points} points, got {len(G)}.")

    # ---------------- sort & log-transform ---------------------------
    G_arr  = np.asarray(G, dtype=float)
    da_arr = np.asarray(da_dn, dtype=float)

    order = np.argsort(G_arr)          # ascending G
    G_arr, da_arr = G_arr[order], da_arr[order]

    log_G  = np.log(G_arr)
    log_da = np.log(da_arr)

    n = len(log_G)
    reg = LinearRegression()

    # ---------------- 1) exhaustive seed search ----------------------
    best_r2 = -np.inf
    best_start = best_end = None
    best_m = best_logC = None

    for s in range(n - min_points + 1):
        for e in range(s + min_points - 1, n):
            x = log_G[s:e + 1].reshape(-1, 1)
            y = log_da[s:e + 1]
            reg.fit(x, y)
            r2 = reg.score(x, y)
            if r2 > best_r2:
                best_r2   = r2
                best_start, best_end = s, e
                best_m     = float(reg.coef_[0])
                best_logC  = float(reg.intercept_)

    if best_r2 < r2_threshold:
        raise RuntimeError(
            f"The best window found has R² = {best_r2:.4f} "
            f"(below r2_threshold = {r2_threshold})."
        )

    # fitting helper
    def fit_slice(a: int, b: int):
        x = log_G[a:b + 1].reshape(-1, 1)
        y = log_da[a:b + 1]
        reg.fit(x, y)
        return float(reg.coef_[0]), float(reg.intercept_), float(reg.score(x, y))

    # ---------------- 2) grow the window -----------------------------
    m, logC, r2 = best_m, best_logC, best_r2

    while True:
        expanded = False

        # try one point to the left
        if best_start > 0:
            m_tmp, logC_tmp, r2_tmp = fit_slice(best_start - 1, best_end)
            if r2_tmp >= r2_threshold:
                best_start -= 1
                m, logC, r2 = m_tmp, logC_tmp, r2_tmp
                expanded = True

        # try one point to the right
        if best_end < n - 1:
            m_tmp, logC_tmp, r2_tmp = fit_slice(best_start, best_end + 1)
            if r2_tmp >= r2_threshold:
                best_end += 1
                m, logC, r2 = m_tmp, logC_tmp, r2_tmp
                expanded = True

        if not expanded:
            break

    # ---------------- 3) final outputs -------------------------------
    C         = math.exp(logC)
    da_start  = float(da_arr[best_start])
    da_end    = float(da_arr[best_end])

    return m, C, r2, da_start, da_end
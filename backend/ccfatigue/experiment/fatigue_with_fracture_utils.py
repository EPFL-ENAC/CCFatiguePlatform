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
    A1, _ = np.polyfit(C_N_1_3, a_over_h, 1)
    G = (3 * np.array(crack_load)**2 * (compliance / N)**(2/3)) / (2 * A1 * width * thickness) * F * 1e6
    return G.tolist()

def compute_g_ecm(compliance, crack_displacement, crack_load, crack_length_fitted, F, N, width):
    crack_array = crack_length_fitted
    c_over_n = compliance / N
    valid_mask = (crack_array > 0) & (c_over_n > 0) & np.isfinite(c_over_n)
    if np.sum(valid_mask) < 2:
        return [float("nan")] * len(crack_array)

    a_log = np.log10(crack_array[valid_mask])
    log_C_N = np.log10(c_over_n[valid_mask])
    m, _ = np.polyfit(a_log, log_C_N, 1)

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

def find_best_paris_fit(G: List[float], da_dn: List[float], min_window_size=3, r2_threshold=0.98, da_dn_min=None, da_dn_max=None):
    da_dn = np.array(da_dn)
    log_G = np.log(G)
    log_da_dn = np.log(da_dn)

    if da_dn_min is not None and da_dn_max is not None:
        mask = (da_dn >= da_dn_min) & (da_dn <= da_dn_max)
        filtered_log_G = log_G[mask]
        filtered_log_da_dn = log_da_dn[mask]

        if len(filtered_log_G) < min_window_size:
            raise ValueError(f"Selected window too small ({len(filtered_log_G)} points)")

        x = filtered_log_G.reshape(-1, 1)
        y = filtered_log_da_dn
        reg = LinearRegression().fit(x, y)
        r2 = reg.score(x, y)
        m = reg.coef_[0]
        logC = reg.intercept_
        C = math.exp(logC)
        return m, C, r2

    best_r2 = -np.inf
    best_start, best_end, best_params = None, None, None

    for start in range(len(log_G) - min_window_size + 1):
        for end in range(start + min_window_size - 1, len(log_G)):
            x = log_G[start:end+1].reshape(-1, 1)
            y = log_da_dn[start:end+1]
            reg = LinearRegression().fit(x, y)
            r2 = reg.score(x, y)
            if r2 > best_r2:
                best_r2 = r2
                best_start = start
                best_end = end
                best_params = (reg.coef_[0], reg.intercept_)

    while True:
        expanded = False
        if best_start > 0:
            new_start = best_start - 1
            x = log_G[new_start:best_end+1].reshape(-1, 1)
            y = log_da_dn[new_start:best_end+1]
            reg = LinearRegression().fit(x, y)
            r2 = reg.score(x, y)
            if r2 >= r2_threshold:
                best_start = new_start
                best_params = (reg.coef_[0], reg.intercept_)
                best_r2 = r2
                expanded = True
        if best_end < len(log_G) - 1:
            new_end = best_end + 1
            x = log_G[best_start:new_end+1].reshape(-1, 1)
            y = log_da_dn[best_start:new_end+1]
            reg = LinearRegression().fit(x, y)
            r2 = reg.score(x, y)
            if r2 >= r2_threshold:
                best_end = new_end
                best_params = (reg.coef_[0], reg.intercept_)
                best_r2 = r2
                expanded = True
        if not expanded:
            break

    m, logC = best_params
    C = math.exp(logC)
    return m, C, best_r2

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
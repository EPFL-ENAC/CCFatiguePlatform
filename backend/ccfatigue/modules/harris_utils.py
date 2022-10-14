#!/usr/bin/env python
""" CCFatigue - Modules 3 + 5 harris_utils.py
Common code for cld_harris.py and miner_harris.py
"""

import numpy as np

# staticvalue.txt => constants
UCS = 27.1
UTS = 27.7

BOUNDS_MARGIN = 0.99

# # Cycles for the isolines (the lines of the CLD)
# CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def calculate_stress_mean(stress_ratio: float, stress_max: float) -> float:
    """
    Calculate mean stress (sigma_mean) for given stress ratio (R) and max stress
    Parameters
    ----------
        stress_ratio
            Stress ratio (R)
        stress_max
            Max stress (sigma_max) [MPa]
    Returns
    -------
        stress_mean
            Mean stress (sigma_mean) [MPa]
    """
    # Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L68  # noqa
    #      lines 68-74

    if abs(stress_ratio) > 1:
        stress_mean = -(1 + (1 / stress_ratio)) * stress_max / 2
    else:
        stress_mean = (1 + stress_ratio) * stress_max / 2
    return stress_mean


def bounds_stress_mean(
    stress_mean: float, uts: float, ucs: float, margin: float
) -> float:
    """
    Apply bounds (uts, ucs) to mean stress (sigma_mean)
    Parameters
    ----------
        stress_mean
            Mean stress [MPa] (sigma_mean)
        uts
            upper bound
        ucs
            lower bound
        margin
    Returns
    -------
        stress_mean
            Mean stress [MPa] (sigma_mean)
    """
    # Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L86 # noqa
    #      lines 86-95
    if stress_mean > uts:
        stress_mean = margin * uts
    if stress_mean < -ucs:
        stress_mean = margin * -ucs

    return stress_mean


def calculate_stress_amplitude(stress_ratio: float, stress_max: float) -> float:
    """
    Calculate stress amplitude (sigma_a) for given stress ratio (R) and max stress
    Parameters
    ----------
        stress_ratio
            Stress ratio (R)
        stress_max
            Max stress (sigma_max) [MPa]
    Returns
    -------
        stress_amplitude
            Stress amplitude (sigma_a) [MPa]
    """
    # Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L68  # noqa
    #      lines 68-74

    if abs(stress_ratio) > 1:
        stress_amplitude = (1 - (1 / stress_ratio)) * stress_max / 2
    else:
        stress_amplitude = (1 - stress_ratio) * stress_max / 2
    return stress_amplitude


def calculate_fuv(x1_array, x2_array, y_array) -> tuple[float, float, float]:
    """
    Calculate f, u and v, which are:
    - f: function of the laminate tensile strength
    - u: shape of the right (predominatly tensile) wings of bell-shaped curve
    - v: shape of the left (predominatly compressive) wings of bell-shaped curve
    Parameters
    ----------
        x1_array
            np array of x1, one per stress ratio TODO define x1
        x2_array
            np array of x2, one per stress ratio TODO define x2
        y_array
            np array of y, one per stress ratio TODO define y
    Returns
    -------
        f
        u
        v
    """
    # Src: https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/3_CLD/Harris/CLD-Harris.for#L109  # noqa
    #      lines 109-135

    # Prepare matrix
    # x_matrix = np.vstack(x1_array)
    x_matrix = np.concatenate([np.vstack(x1_array), np.vstack(x2_array)], axis=1)
    x_matrix = np.insert(x_matrix, 0, 1, axis=1)

    y_matrix = np.vstack(y_array)

    # Computes the transpose product of a matrix
    xtx = np.dot(x_matrix.T, x_matrix)

    # Computes the generalized inverse of a real matrix
    invxtx = np.linalg.pinv(xtx)

    # Multiplies two real rectangular matrices, AB.
    xty = np.dot(x_matrix.T, y_matrix)
    bet = np.dot(invxtx, xty)

    f = 10 ** bet[0, 0]
    u = bet[1, 0]
    v = bet[2, 0]

    return (f, u, v)

#!/usr/bin/env python
""" ccfatigue/analsysis/piecewiselinear_utils.py
Piecewise linear common function for CLD and DAS modules

There is no documentation specifically for this one.
A description is given in the red book, after p. 206
ref:
[1] https://www.sciencedirect.com/science/article/pii/S1359835X04000466?via%3Dihub
    (https://doi.org/10.1016/j.compositesa.2004.02.009)
"""


def calculate_stress_amplitude(stress_ratio: float, stress_max: float) -> float:
    """
    Calculate stress amplitude corresponsing to given R and sigma_max
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
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/cde13599121bceb95d579adfe3e56056ba622d60/CCFatigue_modules/3_CLD/Piecewise-Linear/CLD-Piecewise-Linear.for#L80
    stress_amplitude = 0
    if abs(stress_ratio) > 1:
        stress_amplitude = (1 - (1 / stress_ratio)) * stress_max / 2
    else:
        stress_amplitude = (1 - stress_ratio) * stress_max / 2

    return stress_amplitude


def sort_by_stress_ratios(df):
    # Sort df by stress ratios, respecting sectors counterclockwise
    # according to fig 1, p660, ref [2]

    # - sector 0 = TT (0 < R < 1)
    df.loc[(df.stress_ratio < 1) & (df.stress_ratio >= 0), "sector"] = 0
    # - sector 1 = TC (R < 0)
    df.loc[df.stress_ratio < 0, "sector"] = 1
    # - sector 2 = CC (R > 1)
    df.loc[df.stress_ratio > 1, "sector"] = 2
    df.sort_values(
        by=["sector", "stress_ratio"],
        ascending=[True, False],
        kind="mergesort",
        inplace=True,
    )

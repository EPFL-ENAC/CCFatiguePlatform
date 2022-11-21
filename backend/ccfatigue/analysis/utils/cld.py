#!/usr/bin/env python
""" ccfatigue - analysis - Constant Life Diagram - cld_utils.py
Common CLD functions
"""

import pandas as pd


def cld_add_row(
    cld_df, cycles_to_failure: float, stress_amplitude: float, stress_mean: float
) -> pd.DataFrame:
    """
    Add row to CLD DataFrame
    Parameters
    ----------
        cld_df
            CLD DataFrame
        cycles_to_failure: float
        stress_amplitude: float
        stress_mean: float
    Returns
    -------
        CLD DataFrame
    """
    row = pd.DataFrame(
        {
            "cycles_to_failure": [cycles_to_failure],
            "stress_amplitude": [stress_amplitude],
            "stress_mean": [stress_mean],
        }
    )

    cld_df = pd.concat(
        [
            cld_df,
            row,
        ],
        ignore_index=True,
    )
    return cld_df

#!/usr/bin/env python
""" CCFatigue - Module 2 - sn_curve_loglog.py
This code takes in AGG data and outputs SNC data

Translated from
https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for

It applies a linear regression on log(number of cycles) and log(stress ratios)
as described in
[1] https://www.astm.org/e0739-10r15.html
    DOI: 10.1520/E0739-10R15
"""

from itertools import chain

import pandas as pd
from pandas._typing import FilePath, ReadBuffer, WriteBuffer

import ccfatigue.modules.sn_curve_utils as sn_curve_utils

# Prepare all cycles to failure output (SNC)
# Refs:
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L397
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L410
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L423
# ...
LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(3000000, 20000000, 1000000),
        range(30000000, 1400000000, 100000000),
    )
)


def get_loglog_stress_at_failure(
    aa: float, bb: float, cycles_to_failure: float
) -> float:
    """
    Calculate stress at failure.
    Translated from:
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L397
    Parameters
    ----------
        aa: float
            TODO ask Tassos
        bb: float
            TODO ask Tassos
        cycles_to_failure: float
            Number of cycles to failure
    Returns
    -------
        stress: float
            stress at failure
    """
    stress = aa * cycles_to_failure ** (-bb)
    return stress


def get_loglog_a_b(stress_ratio_df: pd.DataFrame):
    """
    Return LogLog specific value used for "a" and "b"
    Parameters
    ----------
        stress_ratio_df: Dataframe
    Returns
    -------
        a: float
            aa
        b: float
            bb
    """
    return (stress_ratio_df.aa, stress_ratio_df.bb)


def get_loglog_aa_bb(slope, intercept):
    """
    Return LogLog specific value used for "aa" and "bb"
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L368
    Parameters
    ----------
        slope: pd.Series
        intercept: pd.Series
    Returns
    -------
        aa: pd.Series
        bb: pd.Series
    """
    aa = 10 ** (-slope / intercept)
    bb = -1 / intercept
    return (aa, bb)


def get_loglog_lsst(
    stress_max: float, cycles_to_failure: float, avg_cycles_to_failure: float
) -> float:
    """
    Return LogLog specific value for lsst
    Parameters
    ----------
        stress_max: float
        cycles_to_failure: float
        avg_cycles_to_failure: float
    Returns
    -------
        lsst: float
    """
    lsst = (stress_max - avg_cycles_to_failure) ** 2
    return lsst


def execute(
    input_file: FilePath | ReadBuffer,
    output_json_file: FilePath | WriteBuffer,
    output_csv_file: FilePath | WriteBuffer,
    reliability_level: float = 50,
    confidence: int = 95,
) -> None:
    """
    Execute the LogLog algorithm
    Parameters
    ----------
        input_file: FilePath | ReadBuffer
            AGG input file
        output_json_file: FilePath | WriteBuffer
            SNC json file
        output_csv_file: FilePath | WriteBuffer
            SNC csv file
        reliability_level: float
            Reliability level P(N)
        confidence: float
            Confidence={95|99}
    Returns
    -------
        None
    """
    sn_curve_utils.execute_linlog_loglog(
        use_logarithm=True,
        input_file=input_file,
        output_json_file=output_json_file,
        output_csv_file=output_csv_file,
        list_cycles_to_failure=LIST_CYCLES_TO_FAILURE,
        get_stress_at_failure=get_loglog_stress_at_failure,
        get_a_b=get_loglog_a_b,
        get_aa_bb=get_loglog_aa_bb,
        get_lsst=get_loglog_lsst,
        reliability_level=reliability_level,
        confidence=confidence,
    )

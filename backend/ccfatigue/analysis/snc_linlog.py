#!/usr/bin/env python
""" CCFatigue - Module 2 - sn_curve_linlog.py
This code takes in AGG data and outputs SNC data

Translated from
https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for

It applies a linear regression on log(number of cycles) and stress ratios
as described in
[1] https://www.astm.org/e0739-10r15.html
    DOI: 10.1520/E0739-10R15
"""

from itertools import chain
from math import log10

import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.snc as snc

# Prepare all cycles to failure output (SNC)
# Refs:
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L396
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L409
# https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L422
# ...
LIST_CYCLES_TO_FAILURE = list(
    chain(
        range(1, 1000, 50),
        range(1000, 1001),
        range(10000, 2000000, 10000),
        range(2000000, 20000000, 1000000),
        range(30000000, 1400000000, 100000000),
    )
)


def get_linlog_stress_at_failure(
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
    stress = aa + log10(cycles_to_failure) * bb
    return stress


def get_linlog_a_b(stress_ratio_df: pd.DataFrame):
    """
    Return LinLog specific value used for "a" and "b"
    Parameters
    ----------
        stress_ratio_df: Dataframe
    Returns
    -------
        a: float
            slope
        b: float
            intercept
    """
    return (stress_ratio_df.slope, stress_ratio_df.intercept)


def get_linlog_aa_bb(slope, intercept):
    """
    Return LinLog specific value used for "aa" and "bb"
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L367
    Parameters
    ----------
        slope: pd.Series
        intercept: pd.Series
    Returns
    -------
        aa: pd.Series
        bb: pd.Series
    """
    aa = -slope / intercept
    bb = 1 / intercept
    return (aa, bb)


def get_linlog_lsst(
    stress_max: float, cycles_to_failure: float, avg_cycles_to_failure: float
) -> float:
    """
    Return LinLog specific value for lsst
    Parameters
    ----------
        stress_max: float
        cycles_to_failure: float
        avg_cycles_to_failure: float
    Returns
    -------
        lsst: float
    """
    lsst = (cycles_to_failure - avg_cycles_to_failure) ** 2
    return lsst


def execute(
    input_file: FilePath | ReadCsvBuffer,
    output_json_file: FilePath | WriteBuffer | None,
    output_csv_file: FilePath | WriteBuffer | None,
    confidence: int = 95,
) -> None:
    """
    Execute the LinLog algorithm
    Parameters
    ----------
        input_file: FilePath | ReadBuffer
            AGG input file
        output_json_file: FilePath | WriteBuffer
            SNC json file
        output_csv_file: FilePath | WriteBuffer
            SNC csv file
        confidence: float
            Confidence={95|99}
    Returns
    -------
        None
    """
    snc.execute_linlog_loglog(
        use_logarithm=False,
        input_file=input_file,
        output_json_file=output_json_file,
        output_csv_file=output_csv_file,
        list_cycles_to_failure=LIST_CYCLES_TO_FAILURE,
        get_stress_at_failure=get_linlog_stress_at_failure,
        get_a_b=get_linlog_a_b,
        get_aa_bb=get_linlog_aa_bb,
        get_lsst=get_linlog_lsst,
        confidence=confidence,
    )

#!/usr/bin/env python
""" CCFatigue - Module 2 - sn_curve_utils.py
This code regroups shared ressources for module 2
"""

from math import log10, sqrt
import numpy as np
import pandas as pd
from pandas._typing import ReadBuffer, WriteBuffer, FilePath
from scipy import stats
from ccfatigue.modules.astm import Astm


def equation_ycar(slope: float, intercept: float, stress_parameter: float) -> float:
    """
    Calculate ycar TODO check with Tassos for a meaningful name: estimated_y?
    Translated from:
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L314
    Parameters
    ----------
        slope: float
            steepness of a line
        intercept: float
            location where the line intersects an axis
        stress_parameter: float
            Cyclic max stress
    Returns
    -------
        ycar: float
    """
    ycar = slope + intercept * stress_parameter
    return ycar


def stress_at_failure_bounds(
    sample_count: int,
    q: float,
    slope: float,
    intercept: float,
    cycles_to_failure: float,
    pp: float,
    xb: float,
) -> tuple[float, float]:
    """
    Calculate stress at failure bounds (sigma_max)
    Parameters
    ----------
        sample_count: int
        q: float
            TODO ask Tassos
        slope: float
            steepness of a line
        intercept: float
            location where the line intersects an axis
        cycles_to_failure: float
            Number of cycles to failure
        pp: float
            TODO ask Tassos
        xb: float
            TODO ask Tassos
    Returns
    -------
        stress_lowerbound: float
            Stress at failure (lower bound)
        stress_upperbound: float
            Stress at failure (upper bound)
    """

    term1 = (
        (sample_count * q * slope * intercept)
        - (sample_count * q * log10(cycles_to_failure) * intercept)
        + (pp * sample_count * xb)
    )
    term2 = sqrt(
        sample_count
        * pp
        * q
        * (
            2 * sample_count * slope * intercept * xb
            - 2 * sample_count * log10(cycles_to_failure) * intercept * xb
            + q * intercept**2
            + sample_count * intercept**2 * xb**2
            + sample_count * log10(cycles_to_failure) ** 2
            - 2 * sample_count * log10(cycles_to_failure) * slope
            - pp
            + sample_count * slope**2
        )
    )
    term3 = sample_count * (intercept**2 * q - pp)

    stress_upperbound = -(term1 + term2) / term3
    stress_lowerbound = -(term1 - term2) / term3

    return (stress_lowerbound, stress_upperbound)


def execute_linlog_loglog(
    use_logarithm: bool,
    input_file: FilePath | ReadBuffer,
    output_json_file: FilePath | WriteBuffer,
    output_csv_file: FilePath | WriteBuffer,
    list_cycles_to_failure: list[int],
    get_stress_at_failure,
    get_a_b,
    get_aa_bb,
    get_lsst,
    reliability_level: float = 50,
    confidence: int = 95,
) -> None:
    """
    Execute the LinLog or LogLog algorithm, they are very similar
    and share most of the code
    Parameters
    ----------
        use_logarithm: bool
            Use log10(stress_max) if True (LogLog) or stress_max if False (LinLog)
        input_file: FilePath | ReadBuffer
            AGG input file
        output_json_file: FilePath | WriteBuffer
            SNC json file
        output_csv_file: FilePath | WriteBuffer
            SNC csv file
        list_cycles_to_failure: list[int]
            List of cycles_to_failure in SNC output
        get_stress_at_failure
            Get LinLog or LogLog specific stress at failure
        get_a_b
            Get LinLog or LogLog specific a and b
        get_aa_bb
            Get LinLog or LogLog specific aa and bb
        get_lsst
            Get LinLog or LogLog specific lsst
        reliability_level: float = 50,
        confidence: float
            Confidence={95|99}
    Returns
    -------
        None
    """

    astm = Astm(confidence)

    # Import input file (AGG format)
    agg_df = pd.read_csv(input_file)

    # Use stress_max or log10(stress_max) according to used algorithm
    if use_logarithm:
        agg_df["stress_max"] = np.log10(agg_df.stress_max)

    # Use log10(cycles_to_failure) instead of cycles_to_failure
    agg_df["log10_cycles_to_failure"] = np.log10(agg_df.cycles_to_failure)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    agg_df["stress_ratio_id"] = (
        agg_df.stress_ratio != agg_df.stress_ratio.shift()
    ).cumsum()

    # Average by stress_ratio
    stress_ratios_df = (
        agg_df[
            [
                "stress_ratio_id",
                "stress_ratio",
                "stress_max",
                "log10_cycles_to_failure",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
        .rename(
            columns={
                "stress_max": "avg_stress_max",
                "log10_cycles_to_failure": "avg_log10_cycles_to_failure",
            }
        )
    )

    cycles_to_failure = pd.DataFrame(
        list_cycles_to_failure,
        columns=["cycles_to_failure"],
    )

    snc_output_csv_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "cycles_to_failure",
            "stress_max",
            "stress_lowerbound",
            "stress_upperbound",
        ]
    )

    # Calculate slope A and intercept B
    linregress = agg_df.groupby("stress_ratio_id").apply(
        lambda x: stats.linregress(x.stress_max, x.log10_cycles_to_failure)
    )
    stress_ratios_df["slope"] = linregress.apply(lambda x: x[1])
    stress_ratios_df["intercept"] = linregress.apply(lambda x: x[0])

    # Add AA and BB TODO reference for these equations
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L367
    stress_ratios_df["aa"], stress_ratios_df["bb"] = get_aa_bb(
        stress_ratios_df.slope, stress_ratios_df.intercept
    )
    #  = aa
    #  = bb

    # TODO ask Tassos define ycar
    agg_df["ycar"] = agg_df.apply(
        lambda x: equation_ycar(
            stress_ratios_df.loc[x.stress_ratio_id].slope,
            stress_ratios_df.loc[x.stress_ratio_id].intercept,
            x.stress_max,
        ),
        axis=1,
    )

    # TODO ask Tassos define LSST
    agg_df["lsst"] = agg_df.apply(
        lambda x: get_lsst(
            x.stress_max,
            x.log10_cycles_to_failure,
            stress_ratios_df.loc[x.stress_ratio_id].avg_log10_cycles_to_failure,
        ),
        axis=1,
    )

    stress_ratios_df["lsst"] = (
        agg_df[["stress_ratio_id", "lsst"]].groupby("stress_ratio_id").sum()
    )

    # Eq 9, p4, ref [1]
    # TODO ask Tassos define LSSE
    agg_df["lsse"] = (agg_df.log10_cycles_to_failure - agg_df.ycar) ** 2
    stress_ratios_df["lsse"] = (
        agg_df[["stress_ratio_id", "lsse"]].groupby("stress_ratio_id").sum()
    )

    # TODO ask Tassos define LRSQ
    stress_ratios_df["lrsq"] = 1 - stress_ratios_df.lsse / stress_ratios_df.lsst

    # TODO ask Tassos define Q
    agg_df["q"] = agg_df.apply(
        lambda x: (
            x.stress_max - stress_ratios_df.loc[x.stress_ratio_id].avg_stress_max
        )
        ** 2,
        axis=1,
    )
    stress_ratios_df["q"] = (
        agg_df[["stress_ratio_id", "q"]].groupby("stress_ratio_id").sum()
    )

    # NOD
    stress_ratios_df["sample_count"] = (
        agg_df[["stress_ratio_id", "stress_ratio"]].groupby("stress_ratio_id").count()
    )

    # level
    stress_ratios_df["stress_cluster_count"] = (
        agg_df[["stress_ratio_id", "stress_cluster_number"]]
        .groupby("stress_ratio_id")
        .nunique()
    )

    # Eq 6, p4, ref [1]
    # Variance
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/2_S-NCurves/S-N-Curve-LinLog.for#L333
    stress_ratios_df["variance"] = stress_ratios_df.apply(
        lambda x: sqrt(x.lsse / (x.sample_count - 2)), axis=1
    )

    # Fp is given in table 2, p5, ref [1]
    stress_ratios_df["fp"] = stress_ratios_df.apply(
        lambda x: astm.get_astm_val(
            x.stress_cluster_count - 2, x.sample_count - x.stress_cluster_count
        ),
        axis=1,
    )

    # TODO ask Tassos define PP
    stress_ratios_df["pp"] = 2 * stress_ratios_df.fp * stress_ratios_df.variance**2

    # Prepare SNC output list of cycles to failure

    snc_output_json_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "confidence_interval",
            "a",
            "b",
            "lrsq",
            "fp",
        ],
    )

    snc_output_csv_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "cycles_to_failure",
            "stress",
            "stress_lowerbound",
            "stress_upperbound",
        ]
    )

    # For each group of stress_ratio
    for (stress_ratio_id, stress_ratio_df) in stress_ratios_df.iterrows():

        # Prepare CSV data for each cycles to failure
        stress = cycles_to_failure.copy()
        stress["stress"] = stress.apply(
            lambda x: get_stress_at_failure(
                stress_ratio_df.aa,
                stress_ratio_df.bb,
                x.cycles_to_failure,
            ),
            axis=1,
        )

        stress_bounds = stress.apply(
            lambda x: stress_at_failure_bounds(
                stress_ratio_df.sample_count,
                stress_ratio_df.q,
                stress_ratio_df.slope,
                stress_ratio_df.intercept,
                x.cycles_to_failure,
                stress_ratio_df.pp,
                stress_ratio_df.avg_stress_max,
            ),
            axis=1,
        )
        stress["stress_lowerbound"] = stress_bounds.apply(lambda x: x[0])
        stress["stress_upperbound"] = stress_bounds.apply(lambda x: x[1])
        if use_logarithm:
            stress["stress_lowerbound"] = 10**stress.stress_lowerbound
            stress["stress_upperbound"] = 10**stress.stress_upperbound

        stress["stress_ratio"] = stress_ratio_df.stress_ratio

        snc_output_csv_df = pd.concat([snc_output_csv_df, stress])
        a, b = get_a_b(stress_ratio_df)

        # Prepare JSON
        json_df = pd.DataFrame(
            {
                "stress_ratio": stress_ratio_df.stress_ratio,
                "confidence_interval": reliability_level,
                "a": a,
                "b": b,
                "lrsq": stress_ratio_df.lrsq,
                "fp": stress_ratio_df.fp,
            },
            index=[0],
        )
        snc_output_json_df = pd.concat([snc_output_json_df, json_df], ignore_index=True)

    # Export dataframes to files
    snc_output_json_df.to_json(output_json_file, orient="records")

    snc_output_csv_df.to_csv(
        output_csv_file,
        index=False,
    )

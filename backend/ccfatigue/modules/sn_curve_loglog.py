#!/usr/bin/env python
""" CCFatigue - Module 2 - S-N-Curve-LogLog.py
This code takes in AGG data and output SNC data
It applies a linear regression on log(number of cycles) and log(stress ratios)
as described in https://www.astm.org/stp313-eb.html pp. 20-23
DOI: 10.1520/STP313-EB
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

from astm import Astm


CONFIDENCE = 95
RELIABILITY_LEVEL = 50

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")


INPUT_FILENAME = "AGG_input.csv"
INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_JSON_FILENAME = "SNC_LogLog.json"
OUTPUT_JSON_FILE = os.path.join(DATA_DIR, OUTPUT_JSON_FILENAME)
OUTPUT_CSV_FILENAME = "SNC_LogLog.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)


def equation_ycar(
    slope: float, intercept: float, log10_stress_parameter: float
) -> float:
    """
    Calculate ycar
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/feat/fortran2py/dev/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L317
    Inputs:
    - slope: float
    - intercept: float
    - log10_stress_parameter: float
    Output:
    - ycar: float
    """
    ycar = slope + intercept * log10_stress_parameter
    return ycar


def stress_at_failure(aa: float, bb: float, nn: float) -> float:
    """
    Calculate stress at failure
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/feat/fortran2py/dev/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L398
    Inputs:
    - aa: float
    - bb: float
    - nn: float
    Outputs:
    - spn: float
    """
    spn = aa * nn ** (-bb)
    return spn


def stress_at_failure_bounds(
    sample_count: int, q: float, a: float, b: float, nn: float, pp: float, xb: float
) -> tuple[float, float]:
    """Get Stress at failure (sigma_max)
    returns (lower bound, upper bound)"""

    term1 = (
        (sample_count * q * a * b)
        - (sample_count * q * math.log10(nn) * b)
        + (pp * sample_count * xb)
    )
    term2 = math.sqrt(
        sample_count
        * pp
        * q
        * (
            2 * sample_count * a * b * xb
            - 2 * sample_count * math.log10(nn) * b * xb
            + q * b**2
            + sample_count * b**2 * xb**2
            + sample_count * math.log10(nn) ** 2
            - 2 * sample_count * math.log10(nn) * a
            - pp
            + sample_count * a**2
        )
    )
    term3 = sample_count * (b**2 * q - pp)

    stress_upperbound = 10 ** (-(term1 + term2) / term3)
    stress_lowerbound = 10 ** (-(term1 - term2) / term3)

    return (stress_lowerbound, stress_upperbound)


if __name__ == "__main__":

    astm = Astm(CONFIDENCE)

    # Import input file (SNC format)
    samples = pd.read_csv(INPUT_FILE)

    # Add columns with log10(stress_parameter) and log10(number_of_cycles)
    samples["log10_stress_parameter"] = np.log10(samples.stress_parameter)
    samples["log10_number_of_cycles"] = np.log10(samples.number_of_cycles)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    samples["stress_ratio_id"] = (
        samples.stress_ratio != samples.stress_ratio.shift()
    ).cumsum()

    # Average by stress_ratio
    stress_ratios_df = (
        samples[
            [
                "stress_ratio_id",
                "stress_ratio",
                "log10_stress_parameter",
                "log10_number_of_cycles",
                "number_of_cycles",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
    )

    cycles_to_failure = pd.DataFrame(
        chain(
            range(1, 1000, 50),
            range(1000, 1001),
            range(10000, 2000000, 10000),
            range(3000000, 20000000, 1000000),
            range(30000000, 1400000000, 100000000),
        ),
        columns=["cycles_to_failure"],
    )

    snc_output_csv_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "cycles_to_failure",
            "stress_parameter",
            "stress_lowerbound",
            "stress_upperbound",
        ]
    )

    # Calculate A (Eq 4) and B (Eq 5)
    linregress = samples.groupby("stress_ratio_id").apply(
        lambda x: stats.linregress(x.log10_stress_parameter, x.log10_number_of_cycles)
    )

    # Eq 4 and 5
    # Add A and B in avg_by_stress_ratio
    # Slope = A, Intercept = B
    stress_ratios_df["slope"] = linregress.apply(lambda x: x[1])
    stress_ratios_df["intercept"] = linregress.apply(lambda x: x[0])

    # Add AA and BB
    # https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/feat/fortran2py/dev/CCFatigue_modules/2_S-NCurves/S-N-Curve-LogLog.for#L368
    stress_ratios_df["stress_parameter_aa"] = 10 ** (
        -stress_ratios_df.slope / stress_ratios_df.intercept
    )
    stress_ratios_df["stress_parameter_bb"] = -1 / stress_ratios_df.intercept

    samples["ycar"] = samples.apply(
        lambda x: equation_ycar(
            stress_ratios_df.loc[x.stress_ratio_id].slope,
            stress_ratios_df.loc[x.stress_ratio_id].intercept,
            x.log10_stress_parameter,
        ),
        axis=1,
    )

    # LNb
    stress_ratios_df["log10_number_of_cycles"] = (
        samples[["stress_ratio_id", "log10_number_of_cycles"]]
        .groupby("stress_ratio_id")
        .mean()
    )

    # LSST
    samples["lsst"] = samples.apply(
        lambda x: (
            x.log10_stress_parameter
            - stress_ratios_df.loc[x.stress_ratio_id].log10_number_of_cycles
        )
        ** 2,
        axis=1,
    )
    stress_ratios_df["lsst"] = (
        samples[["stress_ratio_id", "lsst"]].groupby("stress_ratio_id").sum()
    )

    # Eq 9
    # LSSE
    samples["lsse"] = (samples.log10_number_of_cycles - samples.ycar) ** 2
    stress_ratios_df["lsse"] = (
        samples[["stress_ratio_id", "lsse"]].groupby("stress_ratio_id").sum()
    )

    # LRSQ
    stress_ratios_df["lrsq"] = 1 - stress_ratios_df.lsse / stress_ratios_df.lsst

    # Q
    stress_ratios_df["xb"] = (
        samples[["stress_ratio_id", "log10_stress_parameter"]]
        .groupby("stress_ratio_id")
        .mean()
    )
    samples["q"] = samples.apply(
        lambda x: (
            x.log10_stress_parameter - stress_ratios_df.loc[x.stress_ratio_id].xb
        )
        ** 2,
        axis=1,
    )
    stress_ratios_df["q"] = (
        samples[["stress_ratio_id", "q"]].groupby("stress_ratio_id").sum()
    )

    # NOD
    stress_ratios_df["sample_count"] = (
        samples[["stress_ratio_id", "stress_ratio"]].groupby("stress_ratio_id").count()
    )

    # level
    stress_ratios_df["level"] = (
        samples[["stress_ratio_id", "stress_level"]]
        .groupby("stress_ratio_id")
        .nunique()
    )

    # Eq 6
    # Variance
    stress_ratios_df["variance"] = stress_ratios_df.apply(
        lambda x: math.sqrt(x.lsse / (x.sample_count - 2)), axis=1
    )

    # Fp
    stress_ratios_df["fp"] = stress_ratios_df.apply(
        lambda x: astm.get_astm_val(x.level - 2, x.sample_count - x.level),
        axis=1,
    )

    # PP
    stress_ratios_df["pp"] = 2 * stress_ratios_df.fp * stress_ratios_df.variance**2

    # Prepare SNC output list of cycles to failure

    snc_output_json_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "RSQL",
            "A",
            "B",
            "Fp",
        ],
    )

    snc_output_csv_df = pd.DataFrame(
        columns=[
            "stress_ratio",
            "cycles_to_failure",
            "stress_parameter",
            "stress_lowerbound",
            "stress_upperbound",
        ]
    )

    # For each group of stress_ratio
    for (stress_ratio_id, stress_ratio_df) in stress_ratios_df.iterrows():

        # Prepare CSV data for each cycles to failure
        stress_parameter = cycles_to_failure.copy()
        stress_parameter["stress_parameter"] = stress_parameter.apply(
            lambda x: stress_at_failure(
                stress_ratio_df.stress_parameter_aa,
                stress_ratio_df.stress_parameter_bb,
                x,
            )
        )

        stress_bounds = stress_parameter.apply(
            lambda x: stress_at_failure_bounds(
                stress_ratio_df.sample_count,
                stress_ratio_df.q,
                stress_ratio_df.slope,
                stress_ratio_df.intercept,
                x.cycles_to_failure,
                stress_ratio_df.pp,
                stress_ratio_df.xb,
            ),
            axis=1,
        )
        stress_parameter["stress_lowerbound"] = stress_bounds.apply(lambda x: x[0])
        stress_parameter["stress_upperbound"] = stress_bounds.apply(lambda x: x[1])

        stress_parameter["stress_ratio"] = stress_ratio_df.stress_ratio

        snc_output_csv_df = pd.concat([snc_output_csv_df, stress_parameter])

        # Prepare JSON
        json_df = pd.DataFrame(
            {
                "stress_ratio": stress_ratio_df.stress_ratio,
                "RSQL": RELIABILITY_LEVEL,
                "A": stress_ratio_df.stress_parameter_aa,
                "B": stress_ratio_df.stress_parameter_bb,
                "Fp": stress_ratio_df.fp,
            },
            index=[0],
        )
        snc_output_json_df = pd.concat([snc_output_json_df, json_df], ignore_index=True)

    # Export dataframes to files
    snc_output_json_df.to_json(OUTPUT_JSON_FILE, orient="records")

    snc_output_csv_df.to_csv(
        OUTPUT_CSV_FILE,
        index=False,
    )

#!/usr/bin/env python
"""
Implementation of the Piecewise Linear Methods as described in:
Anastasios P. Vassilopoulos, Behzad D. Manshadi, Thomas Keller,
Influence of the constant life diagram formulation on the fatigue life prediction of
composite materials,
International Journal of Fatigue,
Volume 32, Issue 4, 2010, Pages 659-669, ISSN 0142-1123,
[1] https://doi.org/10.1016/j.ijfatigue.2009.09.008
    (Section 2.2)
[2] Fatigue of Fiber-reinforced Composites
    4.3.1.2 Piecewise Linear CLD (pp 107-108)
    DOI 10.1007/978-1-84996-181-3
    https://link.springer.com/content/pdf/10.1007/978-1-84996-181-3.pdf
"""

import os

import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer

import ccfatigue.analysis.utils.cld as cld
import ccfatigue.analysis.utils.piecewiselinear as piecewiselinear

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")

INPUT_FILENAME = "SNC_input.csv"
SNC_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
DEFAULT_UCS = 27.1
DEFAULT_UTS = 27.7

# Cycles for the isolines (the lines of the CLD)
CLD_CYCLES_COUNT = [10**x for x in range(3, 10)]  # = 1e3, 1e4, ..., 1e9


def execute(
    snc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_output_file: FilePath | WriteBuffer,
    ucs: float = DEFAULT_UCS,
    uts: float = DEFAULT_UTS,
) -> None:
    """
    Execute the CLD Piecewise Linear algorithm.

    Parameters
    ----------
        snc_csv_input_file
            SNC csv input file
        cld_csv_output_file
            CLD csv output file
        ucs
            Ultimate compressive stress
        uts
            Ultimate tensile stress

    Returns
    -------
        None
    """

    # Import input file (SNC format)
    snc_df = pd.read_csv(snc_csv_input_file)

    # Basic validation
    required_columns = {"stress_ratio", "cycles_to_failure", "stress_max"}
    missing_columns = required_columns - set(snc_df.columns)
    if missing_columns:
        raise ValueError(
            f"Missing required SNC columns: {', '.join(sorted(missing_columns))}"
        )

    # Keep only the cycles used to build the CLD
    snc_df = snc_df[snc_df["cycles_to_failure"].isin(CLD_CYCLES_COUNT)].copy()

    if snc_df.empty:
        raise ValueError(
            "No SNC rows found for the required CLD cycles: "
            f"{', '.join(str(x) for x in CLD_CYCLES_COUNT)}"
        )

    # Calculate stress amplitude sigma_a from SNC points
    snc_df["stress_amplitude"] = snc_df.apply(
        lambda x: piecewiselinear.calculate_stress_amplitude(
            x.stress_ratio, x.stress_max
        ),
        axis=1,
    )

    # Calculate mean stress sigma_m from SNC points
    snc_df["stress_mean"] = (
        (1 + snc_df["stress_ratio"])
        * snc_df["stress_amplitude"]
        / (1 - snc_df["stress_ratio"])
    )

    # Create output dataframe
    cld_df = pd.DataFrame()

    # Build one CLD polyline per life level
    for cycles_to_failure in CLD_CYCLES_COUNT:
        group = snc_df[snc_df["cycles_to_failure"] == cycles_to_failure].copy()

        if group.empty:
            continue

        points = []

        # Left bound: compression static strength
        points.append(
            {
                "cycles_to_failure": cycles_to_failure,
                "stress_amplitude": 0.0,
                "stress_mean": -float(ucs),
            }
        )

        # Known SNC points for this life level
        for _, row in group.iterrows():
            points.append(
                {
                    "cycles_to_failure": cycles_to_failure,
                    "stress_amplitude": float(row["stress_amplitude"]),
                    "stress_mean": float(row["stress_mean"]),
                }
            )

        # Right bound: tensile static strength
        points.append(
            {
                "cycles_to_failure": cycles_to_failure,
                "stress_amplitude": 0.0,
                "stress_mean": float(uts),
            }
        )

        # Sort from left to right on the CLD plane
        points = sorted(points, key=lambda x: x["stress_mean"])

        # Add sorted points to output
        for p in points:
            cld_df = cld.cld_add_row(
                cld_df,
                p["cycles_to_failure"],
                p["stress_amplitude"],
                p["stress_mean"],
            )

    # Final safety sort
    cld_df = cld_df.sort_values(
        by=["cycles_to_failure", "stress_mean"],
        ascending=[True, True],
    ).reset_index(drop=True)

    # Generate output file
    cld_df.to_csv(path_or_buf=cld_csv_output_file, index=False)  # type: ignore
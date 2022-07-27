""" CCFatigue - Module 5 - miner_harris.py

Harris diagram is described in Tassos red book p. 108 - the original papers are:
[1] https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub
    (https://doi.org/10.1016/0142-1123(94)90478-2)
[2] https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub
    (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import os
import sys
import math
import numpy as np
import pandas as pd
from scipy import stats

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "..", "Data")
HARRIS_MOD3_DIR = os.path.join(SRC_DIR, "..", "..", "3_CLD", "Harris")

INPUT_SNC_FILENAME = "SNC_input.csv"
INPUT_SNC_FILE = os.path.join(SRC_DIR, INPUT_SNC_FILENAME)
INPUT_CYC_FILENAME = "CYC_ccinput.csv"
INPUT_CYC_FILE = os.path.join(SRC_DIR, INPUT_CYC_FILENAME)

OUTPUT_CSV_FILENAME = "DAS_Harris.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

# staticvalue.txt => constants
UCS = 27.1
UTS = 27.7

# Factor loop
FACTOR_START = 1.0
FACTOR_STOP = 0.9
FACTOR_STEP = -1.0


def calculate_normalized_alternating_stress(
    f: float, m: float, c: float, u: float, v: float
):
    """
    Calculate the normalized alternating stress component
    Equation 1 p9 Ref [2]
    Inputs:
    - f: function of the laminate tensile strength
    - m: normalized mean stress
    - c: normalized compression stress
    - u: shape of the right (predominatly tensile) wings of bell-shaped curve
    - v: shape of the left (predominatly compressive) wings of bell-shaped curve
    Outputs:
    - a: normalized alternating stress component
    """
    a = f * (1 - m) ** u * (c + m) ** v
    return a


def calculate_damage(CYC_row, factor):
    """
    Calculate damage
    https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/CCFatigue_modules/5_DammageSummation/Harris/Miner-Harris.for#L177
    - lines 177-249
    Inputs:
    - CYC_row
    - factor
    Outpus:
    - damage
    """

    loop_params = [
        (1000, 1),
        (10e5, 10),
        (10e8, 10000),
        (10e12, 10e7),
        (10e16, 10e10),
        (10e25, 10e12),
    ]

    samp = CYC_row.stress_mean * factor + (1 - CYC_row.stress_ratio) / (
        1 + CYC_row.stress_ratio
    )

    a = 10000
    jj = 1

    for (max, inc) in loop_params:
        while jj < max and a >= samp:

            f = af * math.log10(jj) + bf
            u = au * math.log10(jj) + bu
            v = av * math.log10(jj) + bv
            a = calculate_normalized_alternating_stress(
                f, CYC_row.stress_mean * factor / UTS, UCS / UTS, u, v
            )
            jj += inc

    damage = CYC_row.n_cycles / jj
    return damage


if __name__ == "__main__":

    sys.path.insert(0, HARRIS_MOD3_DIR)
    import cld_harris

    SNC_df = pd.read_csv(INPUT_SNC_FILE)  # Known stress ratios
    CYC_df = pd.read_csv(INPUT_CYC_FILE)

    # Data are grouped by stress_ratio but one experiment
    # can have two separate groups with same stress_ratio so we need to identify
    SNC_df["stress_ratio_id"] = (
        SNC_df.stress_ratio != SNC_df.stress_ratio.shift()
    ).cumsum()

    # Average by stress_ratio
    stress_ratios_df = (
        SNC_df[
            [
                "stress_ratio_id",
                "stress_ratio",
            ]
        ]
        .groupby(["stress_ratio_id"])
        .mean()
    )

    # Error message if less than 3 R-ratios
    if len(stress_ratios_df) < 3:
        raise Exception("Input data is not enough to apply Haris' method.")

    # Calculate sigma amplitude
    SNC_df["stress_amplitude"] = SNC_df.apply(
        lambda x: cld_harris.calculate_stress_amplitude(
            x.stress_ratio, x.stress_parameter
        ),
        axis=1,
    )

    # Calculate mean sigma
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: cld_harris.calculate_stress_mean(x.stress_ratio, x.stress_parameter),
        axis=1,
    )

    # Apply bounds to mean sigma (applied to module 3, not to module 5)
    SNC_df["stress_mean"] = SNC_df.apply(
        lambda x: cld_harris.bounds_stress_mean(
            x.stress_mean, cld_harris.UTS, cld_harris.UCS, cld_harris.BOUNDS_MARGIN
        ),
        axis=1,
    )

    SNC_df["y"] = SNC_df.apply(
        lambda x: math.log10(x.stress_amplitude / cld_harris.UTS), axis=1
    )
    SNC_df["x1"] = SNC_df.apply(
        lambda x: math.log10(1 - (x.stress_mean / cld_harris.UTS)), axis=1
    )
    SNC_df["x2"] = SNC_df.apply(
        lambda x: math.log10(
            (cld_harris.UCS / cld_harris.UTS) + (x.stress_mean / cld_harris.UTS)
        ),
        axis=1,
    )

    cycles_df = pd.DataFrame(
        SNC_df.cycles_to_failure.unique(), columns=["cycles_to_failure"]
    )

    # Log10(N)
    cycles_df["log10_cycles_to_failure"] = np.log10(cycles_df.cycles_to_failure)

    for cycles_count in SNC_df.cycles_to_failure.unique():

        cycle_df = SNC_df.loc[SNC_df.cycles_to_failure == cycles_count]

        f, u, v = cld_harris.calculate_fuv(
            cycle_df.x1.to_numpy(), cycle_df.x2.to_numpy(), cycle_df.y.to_numpy()
        )

        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "f"] = f
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "u"] = u
        cycles_df.loc[cycles_df.cycles_to_failure == cycles_count, "v"] = v

    # Linear Regression to calculate slope A and intercept B
    linregress_f = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.f)
    linregress_u = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.u)
    linregress_v = stats.linregress(cycles_df.log10_cycles_to_failure, cycles_df.v)
    af, bf = linregress_f[1], linregress_f[0]
    au, bu = linregress_u[1], linregress_u[0]
    av, bv = linregress_v[1], linregress_v[0]

    factors = pd.DataFrame(
        np.arange(FACTOR_START, FACTOR_STOP, FACTOR_STEP), columns=["factor"]
    )
    for factor in factors.factor:

        CYC_df["damage"] = CYC_df.apply(lambda x: calculate_damage(x, factor), axis=1)

        CYC_df["stress"] = (CYC_df.stress_mean + CYC_df.stress_amplitude / 2) * factor

        factors.loc[factors.factor == factor, "damage"] = 1 / CYC_df.damage.sum()
        factors.loc[factors.factor == factor, "max_stress"] = CYC_df.stress.max()

    # Generate output file
    factors[["max_stress", "damage"]].to_csv(
        OUTPUT_CSV_FILE,
        index=False,
    )

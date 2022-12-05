#!/usr/bin/env python

"""
original author scottausor, revised by sbancal
"""

import os
import re

import numpy as np
import pandas as pd
from scipy import stats

from preprocessing.tst_data_lib import RAW_EXPERIMENT_FP_FOLDERS, Experiment, Logger


def process_hysteresis(df, test_meta):

    """
    Arguments:
        df: dataframe with raw data in standard format

    Returns: {
        "hyst_df": hyst_df,
        "stress_at_failure": stress_at_failure,
        "strain_at_failure": strain_at_failure,
        "n_fail": n_fail,
    }
    """

    def poly_area(x, y):
        """
        Definition of polyarea function
        Arguments:
            x: values measured along the x axis (strain)
            y: values measured along the y axis (stress)

        Returns:
            Single value for the area of the loop

        Description:
            The PolyArea function computes the area of each hysteresis loops
            using a shoelace algorithm
        """
        return 0.5 * np.abs(np.dot(x, np.roll(y, 1)) - np.dot(y, np.roll(x, 1)))

    hyst_df = pd.DataFrame(
        columns=["n_cycles", "hysteresis_area", "stiffness", "creep"]
    )

    # NB CYCLES
    # Extract number of cycles without repeating values in other table,
    # and store number of measurements per cycle
    if "Machine_N_cycles" in df.columns and df.count().Machine_N_cycles > 0:
        n_cycles = np.sort(df["Machine_N_cycles"].unique())
        cycle_field = "Machine_N_cycles"
        load_field = "Machine_Load"
        displacement_field = "Machine_Displacement"
    else:
        n_cycles = np.sort(df["MD_N_cycles--1"].unique())
        cycle_field = "MD_N_cycles--1"
        load_field = "MD_Load--1"
        displacement_field = "MD_Displacement--1"

    # calculate Stress and Strain
    df = df.assign(
        stress=df[load_field] / (test_meta["width"] * test_meta["thickness"]),
        strain=df[displacement_field] / test_meta["length"],
    )

    hyst_df.n_cycles = n_cycles
    n_fail = np.max(n_cycles)

    last_cycle = df[cycle_field].unique()[-1]

    hysteresis_area = np.full(n_cycles.size, np.nan)
    stiffness = np.full(n_cycles.size, np.nan)
    creep = np.full(n_cycles.size, np.nan)
    stress_at_failure = np.max(df[df[cycle_field] == last_cycle].stress)
    strain_at_failure = np.max(df[df[cycle_field] == last_cycle].strain)
    for i in range(n_cycles.size):
        cycle_mask = df[cycle_field] == n_cycles[i]
        cycle_stress = df[cycle_mask].stress
        cycle_strain = df[cycle_mask].strain
        if i < (n_cycles.size - 1):
            hysteresis_area[i] = poly_area(cycle_stress, cycle_strain) * (
                n_cycles[i + 1] - n_cycles[i]
            )
        else:
            hysteresis_area[i] = poly_area(cycle_stress, cycle_strain) * (
                n_fail - n_cycles[i]
            )

        # Stiffness & Creep
        if i > 0:
            slope, _, _, _, _ = stats.linregress(cycle_strain, cycle_stress)
            stiffness[i] = slope
            creep[i] = (np.max(cycle_strain) + np.min(cycle_strain)) / 2

    hyst_df.hysteresis_area = hysteresis_area
    hyst_df.stiffness = stiffness
    hyst_df.creep = creep
    answer = {
        "hyst_df": hyst_df,
        "stress_at_failure": stress_at_failure,
        "strain_at_failure": strain_at_failure,
        "n_fail": n_fail,
    }
    return answer


def main():
    with Logger(None) as logger:
        for experiment_raw_fp_folder in RAW_EXPERIMENT_FP_FOLDERS:
            if not experiment_raw_fp_folder.endswith("_FA"):
                continue
            experiment_metadata = Experiment(experiment_raw_fp_folder, logger)
            logger.write(f"parsing {experiment_raw_fp_folder}")
            for measures in experiment_metadata.exp_meta_meta["measures"]:
                file_number = int(re.search(r"(\d+).csv", measures["raw_fp"]).group(1))
                test_meta = experiment_metadata.tests[
                    experiment_metadata.tests["specimen number"] == file_number
                ].to_dict(orient="list")
                for k in test_meta:
                    # only one value, so take it directly
                    test_meta[k] = test_meta[k][0]
                try:
                    logger.info(
                        f"Read measures {os.path.basename(measures['preprocessed_fp'])}"
                    )
                    with logger.indent:
                        df = pd.read_csv(measures["preprocessed_fp"], low_memory=False)

                    hyst_df = process_hysteresis(df, test_meta)["hyst_df"]
                    # Drop last line
                    # The value that is calculated is invalid
                    hyst_df = hyst_df[:-1]
                    hys_fp = "HYS_" + os.path.basename(measures["preprocessed_fp"])
                    hyst_df.round(
                        {"hysteresis_area": 5, "stiffness": 10, "creep": 10}
                    ).to_csv(
                        os.path.join(
                            experiment_metadata.exp_meta_meta["preprocessed_folder"],
                            hys_fp,
                        ),
                        index=False,
                    )
                except (ValueError, AttributeError):
                    pass  # missing data to produce HYS -> skip for now


if __name__ == "__main__":
    main()

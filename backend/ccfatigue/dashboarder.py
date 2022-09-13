import os
from datetime import date
from typing import Any, Dict, List

import numpy as np
import pandas as pd
from bokeh import palettes
from pandas.core.frame import DataFrame
from pydantic import BaseModel

from ccfatigue import plotter
from ccfatigue.plotter import DataKey, Line, Plot

DATA_DIRECTORY: str = "../Data/"

INTERVAL: int = 10
LOOP_SPACING: int = 1000
MAGNITUDE: int = -3


class Test(BaseModel):
    number: int
    color: str
    total_dissipated_energy: int


class Dashboard(BaseModel):
    tests: List[Test]
    stress_strain: Any
    creep: Any
    hysteresis_area: Any
    stiffness: Any


def get_dataframe(
    data_in: str,
    laboratory: str,
    researcher: str,
    experiment_type: str,
    date: date,
    test_number: int,
) -> DataFrame:
    formatted_date = date.isoformat()
    filename = f"{data_in}_{formatted_date}_{experiment_type}_{test_number:03d}.csv"
    filepath = os.path.join(
        DATA_DIRECTORY,
        laboratory,
        researcher,
        experiment_type,
        formatted_date,
        data_in,
        filename,
    )
    return pd.read_csv(os.path.abspath(filepath))


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """Compute subset of index used for plotting curves."""
    n_cycles_max = np.max(df.n_cycles)

    sub_index_intermediate = np.geomspace(
        start=LOOP_SPACING, stop=n_cycles_max, num=INTERVAL
    )
    sub_index = np.round(sub_index_intermediate, MAGNITUDE)
    sub_index[0] = 1

    return sub_index


def create_sub_hystloops(
    df: DataFrame, sub_index: List[int]
) -> List[Dict[DataKey, List]]:
    # Conditional plotting of hysteresis loops
    # we only plot the loops specified by sub_index
    sub_hystloops = []
    for j in range(len(sub_index)):
        sub_hystloops_strain = []
        sub_hystloops_stress = []
        sub_hystloops_ncycles = []
        for i in range(len(df)):
            if df.Machine_N_cycles[i] == sub_index[j]:
                sub_hystloops_strain.append(df.Machine_Displacement[i])
                sub_hystloops_stress.append(df.Machine_Load[i])
                sub_hystloops_ncycles.append(df.Machine_N_cycles[i])
        # make curve closed // Optional
        if len(sub_hystloops_ncycles) != 0:
            sub_hystloops_strain.append(sub_hystloops_strain[0])
            sub_hystloops_stress.append(sub_hystloops_stress[0])
            sub_hystloops_ncycles.append(sub_hystloops_ncycles[0])
        sub_hystloops.append(
            {
                DataKey.N_CYCLES: sub_hystloops_ncycles,
                DataKey.STRAIN: sub_hystloops_strain,
                DataKey.STRESS: sub_hystloops_stress,
            }
        )
    return sub_hystloops


def generate_stress_strain(
    tests: List[Test], std_dfs: List[DataFrame], hyst_dfs: List[DataFrame]
) -> Plot:
    sub_hystloops = [
        create_sub_hystloops(std_df, compute_sub_indexes(hyst_df))
        for std_df, hyst_df in zip(std_dfs, hyst_dfs)
    ]
    lines = [
        Line(
            data=loop,
            legend_label=test.number,
            color=test.color,
        )
        for test, loops in zip(tests, sub_hystloops)
        for loop in loops
    ]
    return Plot(
        title="Stress - Strain",
        x_axis=DataKey.STRAIN,
        y_axis=DataKey.STRESS,
        tooltips=[DataKey.STRESS, DataKey.STRAIN, DataKey.N_CYCLES],
        lines=lines,
    )


def generate_creep(tests: List[Test], hyst_dfs: List[DataFrame]) -> Plot:
    lines = [
        Line(
            data={
                DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
                DataKey.CREEP: hyst_df["creep"].to_list(),
            },
            legend_label=test.number,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Creep evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.CREEP,
        tooltips=[DataKey.CREEP, DataKey.N_CYCLES],
        lines=lines,
    )


def generate_hyst_area(tests: List[Test], hyst_dfs: List[DataFrame]) -> Plot:
    lines = [
        Line(
            data={
                DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
                DataKey.HYST_AREA: hyst_df["hysteresis_area"].to_list(),
            },
            legend_label=test.number,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Hysteresis loop area evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.HYST_AREA,
        tooltips=[DataKey.HYST_AREA, DataKey.N_CYCLES],
        lines=lines,
    )


def generate_stiffness(tests: List[Test], hyst_dfs: List[DataFrame]) -> Plot:
    lines = [
        Line(
            data={
                DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
                DataKey.STIFNESS: hyst_df["stiffness"].to_list(),
            },
            legend_label=test.number,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Stiffness evolution under cyclic loading",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STIFNESS,
        tooltips=[DataKey.STIFNESS, DataKey.N_CYCLES],
        lines=lines,
    )


def get_total_dissipated_energy(hyst_df: DataFrame) -> int:
    return np.sum(hyst_df["hysteresis_area"])


def generate_dashboard(
    laboratory: str,
    researcher: str,
    experiment_type: str,
    date: date,
    test_numbers: List[int],
) -> Dashboard:
    colors = list(palettes.Category10_10)[: len(test_numbers)]
    std_dfs = [
        get_dataframe("STD", laboratory, researcher, experiment_type, date, test_number)
        for test_number in test_numbers
    ]
    hyst_dfs = [
        get_dataframe("HYS", laboratory, researcher, experiment_type, date, test_number)
        for test_number in test_numbers
    ]
    tests = [
        Test(
            number=test_number,
            color=color,
            total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        )
        for test_number, color, hyst_df in zip(test_numbers, colors, hyst_dfs)
    ]
    return Dashboard(
        tests=tests,
        stress_strain=plotter.export_plot(
            generate_stress_strain(tests, std_dfs, hyst_dfs)
        ),
        creep=plotter.export_plot(generate_creep(tests, hyst_dfs)),
        hysteresis_area=plotter.export_plot(generate_hyst_area(tests, hyst_dfs)),
        stiffness=plotter.export_plot(generate_stiffness(tests, hyst_dfs)),
    )


def main():
    dashboard = generate_dashboard(
        laboratory="CCLAB",
        researcher="Vahid",
        experiment_type="FA",
        date=date(year=2021, month=4, day=20),
        test_numbers=[2, 5],
    )
    plotter.save_json(dashboard.stress_strain, "stress_strain.json")
    plotter.save_json(dashboard.creep, "creep.json")
    plotter.save_json(dashboard.hysteresis_area, "hyst_area.json")
    plotter.save_json(dashboard.stiffness, "stiffness.json")


if __name__ == "__main__":
    main()

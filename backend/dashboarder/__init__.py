import json
import os
from datetime import date
from enum import Enum
from typing import Any, Dict, List

import numpy as np
import pandas as pd
from bokeh import palettes
from bokeh.embed import json_item
from bokeh.io import show
from bokeh.models.sources import ColumnDataSource
from bokeh.models.tools import HoverTool
from bokeh.plotting import figure
from pandas.core.frame import DataFrame
from pydantic import BaseModel

DATA_DIRECTORY: str = "../data/"
OUTPUT_DIRECTORY: str = "./output"

INTERVAL: int = 10
LOOP_SPACING: int = 1000
MAGNITUDE: int = -3


class DataKey(Enum):
    CREEP = ("creep", "Creep")
    HYST_AREA = ("hyst_area", "Hysteresis area")
    N_CYCLES = ("n_cycles", "Number of cycles")
    STIFNESS = ("stiffness", "Stiffness")
    STRAIN = ("strain", "Strain")
    STRESS = ("stress", "Stress")

    def __init__(self, key: str, label: str):
        self.key = key
        self.label = label


class Line(BaseModel):
    color: str
    data: Dict[DataKey, List[Any]]


class LinePlot(BaseModel):
    title: str
    x_axis: DataKey
    y_axis: DataKey
    tooltips: List[DataKey]
    lines: List[Line]


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


def get_dataframe(data_in: str,
                  laboratory: str,
                  researcher: str,
                  experience_type: str,
                  date: date,
                  test_number: int) -> DataFrame:
    formatted_date = date.isoformat()
    filename = f"{data_in}_{formatted_date}_{experience_type}_{test_number:03d}.csv"
    filepath = os.path.join(DATA_DIRECTORY,
                            laboratory,
                            researcher,
                            experience_type,
                            formatted_date,
                            data_in,
                            filename)
    return pd.read_csv(os.path.abspath(filepath))


def save_json(json_data: Any, filename: str) -> None:
    os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
    with open(os.path.join(OUTPUT_DIRECTORY, filename), "w") as file:
        json.dump(json_data, file)


def export_plot(plot: LinePlot, display=False) -> Any:
    fig = figure(title=plot.title,
                 x_axis_label=plot.x_axis.label,
                 y_axis_label=plot.y_axis.label,
                 sizing_mode="stretch_both")
    fig.add_tools(HoverTool(
        tooltips=[(key.label, "@" + key.key) for key in plot.tooltips]
    ))
    for line in plot.lines:
        data = {k.key: v for k, v in line.data.items()}
        source = ColumnDataSource(data=data)
        fig.line(x=plot.x_axis.key, y=plot.y_axis.key,
                 source=source, color=line.color)
    if display:
        show(fig)
    return json_item(fig)


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """Compute subset of index used for plotting curves."""
    n_cycles_max = np.max(df.n_cycles)

    sub_index_intermediate = np.geomspace(
        start=LOOP_SPACING, stop=n_cycles_max, num=INTERVAL)
    sub_index = np.round(sub_index_intermediate, MAGNITUDE)
    sub_index[0] = 1

    return sub_index


def create_sub_hystloops(df: DataFrame,
                         sub_index: List[int]) -> List[Dict[DataKey, List]]:
    # Conditional plotting of hysteresis loops - we only plot the loops specified by sub_index
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
        sub_hystloops.append({DataKey.N_CYCLES: sub_hystloops_ncycles,
                              DataKey.STRAIN: sub_hystloops_strain,
                              DataKey.STRESS: sub_hystloops_stress})
    return sub_hystloops


def generate_stress_strain(colors: List[str],
                           std_dfs: List[DataFrame],
                           hyst_dfs: List[DataFrame]) -> LinePlot:
    sub_hystloops = [create_sub_hystloops(std_df, compute_sub_indexes(hyst_df))
                     for std_df, hyst_df in zip(std_dfs, hyst_dfs)]
    lines = [
        Line(
            color=color,
            data=loop
        )
        for color, loops in zip(colors, sub_hystloops)
        for loop in loops
    ]
    return LinePlot(
        title="Stress - Strain",
        x_axis=DataKey.STRAIN,
        y_axis=DataKey.STRESS,
        tooltips=[DataKey.STRESS, DataKey.STRAIN, DataKey.N_CYCLES],
        lines=lines
    )


def generate_creep(colors: List[str],
                   hyst_dfs: List[DataFrame]) -> LinePlot:
    lines = [Line(
        color=color,
        data={
            DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
            DataKey.CREEP: hyst_df["creep"].to_list(),
        },
    ) for color, hyst_df in zip(colors, hyst_dfs)]
    return LinePlot(
        title="Creep evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.CREEP,
        tooltips=[DataKey.CREEP, DataKey.N_CYCLES],
        lines=lines,
    )


def generate_hyst_area(colors: List[str],
                       hyst_dfs: List[DataFrame]) -> LinePlot:
    lines = [Line(
        color=color,
        data={
            DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
            DataKey.HYST_AREA: hyst_df["hysteresis_area"].to_list(),
        },
    ) for color, hyst_df in zip(colors, hyst_dfs)]
    return LinePlot(
        title="Hysteresis loop area evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.HYST_AREA,
        tooltips=[DataKey.HYST_AREA, DataKey.N_CYCLES],
        lines=lines,
    )


def generate_stiffness(colors: List[str],
                       hyst_dfs: List[DataFrame]) -> LinePlot:
    lines = [Line(
        color=color,
        data={
            DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
            DataKey.STIFNESS: hyst_df["stiffness"].to_list(),
        },
    ) for color, hyst_df in zip(colors, hyst_dfs)]
    return LinePlot(
        title="Stiffness evolution under cyclic loading",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STIFNESS,
        tooltips=[DataKey.STIFNESS, DataKey.N_CYCLES],
        lines=lines,
    )


def get_total_dissipated_energy(hyst_df: DataFrame) -> int:
    return np.sum(hyst_df["hysteresis_area"])


def generate_dashboard(laboratory: str,
                       researcher: str,
                       experience_type: str,
                       date: date,
                       test_numbers: List[int]) -> Dashboard:
    colors = list(palettes.Category10_10)[:len(test_numbers)]
    std_dfs = [get_dataframe("STD", laboratory, researcher, experience_type,
                             date, test_number)
               for test_number in test_numbers]
    hyst_dfs = [get_dataframe("HYS", laboratory, researcher, experience_type,
                              date, test_number)
                for test_number in test_numbers]
    tests = [
        Test(
            number=test_number,
            color=color,
            total_dissipated_energy=get_total_dissipated_energy(hyst_df)
        )
        for test_number, color, hyst_df in zip(test_numbers, colors, hyst_dfs)
    ]
    return Dashboard(
        tests=tests,
        stress_strain=export_plot(
            generate_stress_strain(colors, std_dfs, hyst_dfs)),
        creep=export_plot(generate_creep(colors, hyst_dfs)),
        hysteresis_area=export_plot(generate_hyst_area(colors, hyst_dfs)),
        stiffness=export_plot(generate_stiffness(colors, hyst_dfs)),
    )


def main():
    dashboard = generate_dashboard(laboratory="CCLAB",
                                   researcher="Vahid",
                                   experience_type="FA",
                                   date=date(year=2021, month=4, day=20),
                                   test_numbers=[2, 5])
    save_json(dashboard.stress_strain, "stress_strain.json")
    save_json(dashboard.creep, "creep.json")
    save_json(dashboard.hysteresis_area, "hyst_area.json")
    save_json(dashboard.stiffness, "stiffness.json")


if __name__ == "__main__":
    main()

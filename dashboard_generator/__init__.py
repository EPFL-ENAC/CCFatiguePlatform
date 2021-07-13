import json
import os
from enum import Enum
from sys import path
from typing import Dict, List

import numpy as np
import pandas as pd
from bokeh.embed import json_item
from bokeh.io import show
from bokeh.models.sources import ColumnDataSource
from bokeh.models.tools import HoverTool
from bokeh.plotting import figure
from pandas.core.frame import DataFrame

OUTPUT_DIRECTORY: str = "./output"

DATA_DIRECTORY: str = "../data/"
LAB: str = "CCLAB"
RESEARCHER: str = "Vahid"

DATE: str = "2021-04-20"
TEST_TYPE: str = "FA"
TEST_NUMBER: str = "002"

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


class LinePlot:
    def __init__(self,
                 title: str,
                 x_axis: DataKey,
                 y_axis: DataKey,
                 tooltips: List[DataKey],
                 data: List[Dict[DataKey, List]],
                 ) -> None:
        self.title = title
        self.x_axis = x_axis
        self.y_axis = y_axis
        self.tooltips = tooltips
        self.data = data


def get_dataframe(data_in: str) -> DataFrame:
    filename: str = "{}_{}_{}_{}.csv".format(
        data_in, DATE, TEST_TYPE, TEST_NUMBER)
    filepath: path = os.path.join(
        DATA_DIRECTORY, LAB, RESEARCHER, TEST_TYPE, DATE, data_in, filename)
    return pd.read_csv(os.path.abspath(filepath))


def save_json(filename: str, json_data: Dict) -> None:
    os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
    with open(os.path.join(OUTPUT_DIRECTORY, filename), "w") as file:
        json.dump(json_data, file)


def save_plot(plot: LinePlot, filename: str, display=False) -> None:
    fig = figure(title=plot.title,
                 x_axis_label=plot.x_axis.label,
                 y_axis_label=plot.y_axis.label,
                 sizing_mode="stretch_both")
    fig.add_tools(HoverTool(
        tooltips=[(key.label, "@" + key.key) for key in plot.tooltips]
    ))
    for line in plot.data:
        source = ColumnDataSource(data={k.key: v for k, v in line.items()})
        fig.line(x=plot.x_axis.key, y=plot.y_axis.key, source=source)
    json_data = json_item(fig)
    save_json(filename, json_data)
    if display:
        show(fig)


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """Compute subset of index used for plotting curves."""
    n_cycles_max = np.max(df.n_cycles)

    sub_index_intermediate = np.geomspace(
        start=LOOP_SPACING, stop=n_cycles_max, num=INTERVAL)
    sub_index = np.round(sub_index_intermediate, MAGNITUDE)
    sub_index[0] = 1

    return sub_index


def create_sub_hystloops(df: DataFrame, sub_index: List[int]) -> List[Dict[DataKey, List]]:
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


def generate_select_stress_strain(std_df: DataFrame, hyst_df: DataFrame) -> LinePlot:
    sub_indexes = compute_sub_indexes(hyst_df)
    sub_hystloops = create_sub_hystloops(std_df, sub_indexes)

    return LinePlot(
        title="Stress - Strain",
        x_axis=DataKey.STRAIN,
        y_axis=DataKey.STRESS,
        tooltips=[DataKey.STRESS, DataKey.STRAIN, DataKey.N_CYCLES],
        data=sub_hystloops
    )


def generate_creep(hyst_df: DataFrame) -> LinePlot:
    line = {
        DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
        DataKey.CREEP: hyst_df["creep"].to_list(),
    }
    return LinePlot(
        title="Creep evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.CREEP,
        tooltips=[DataKey.CREEP, DataKey.N_CYCLES],
        data=[line],
    )


def generate_hyst_area(hyst_df: DataFrame) -> LinePlot:
    line = {
        DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
        DataKey.HYST_AREA: hyst_df["hysteresis_area"].to_list(),
    }
    return LinePlot(
        title="Hysteresis loop area evolution",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.HYST_AREA,
        tooltips=[DataKey.HYST_AREA, DataKey.N_CYCLES],
        data=[line],
    )


def generate_stiffness(hyst_df: DataFrame) -> LinePlot:
    line = {
        DataKey.N_CYCLES: hyst_df["n_cycles"].to_list(),
        DataKey.STIFNESS: hyst_df["stiffness"].to_list(),
    }
    return LinePlot(
        title="Stiffness evolution under cyclic loading",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STIFNESS,
        tooltips=[DataKey.STIFNESS, DataKey.N_CYCLES],
        data=[line],
    )


def generate_all():
    std_df = get_dataframe("STD")
    hyst_df = get_dataframe("HYS")

    save_plot(generate_select_stress_strain(std_df, hyst_df),
              "select_stress_strain.json")
    save_plot(generate_creep(hyst_df),
              "creep.json")
    save_plot(generate_hyst_area(hyst_df),
              "hyst_area.json")
    save_plot(generate_stiffness(hyst_df),
              "stiffness.json")


def main():
    generate_all()


if __name__ == "__main__":
    main()

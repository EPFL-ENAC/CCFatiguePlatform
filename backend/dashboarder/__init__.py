import json
import os
from datetime import date
from enum import Enum
from typing import Any, Dict, List

import numpy as np
import pandas as pd
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


class LinePlot:
    def __init__(self,
                 title: str,
                 x_axis: DataKey,
                 y_axis: DataKey,
                 tooltips: List[DataKey],
                 data: List[Dict[DataKey, List[Any]]],
                 ) -> None:
        self.title = title
        self.x_axis = x_axis
        self.y_axis = y_axis
        self.tooltips = tooltips
        self.data = data


class Dashboard(BaseModel):
    total_dissipated_energy: int
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
    for line in plot.data:
        source = ColumnDataSource(data={k.key: v for k, v in line.items()})
        fig.line(x=plot.x_axis.key, y=plot.y_axis.key, source=source)
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


def generate_select_stress_strain(std_df: DataFrame,
                                  hyst_df: DataFrame) -> LinePlot:
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


def get_total_dissipated_energy(hyst_df: DataFrame) -> int:
    return np.sum(hyst_df["hysteresis_area"])


def generate_dashboard(laboratory: str,
                       researcher: str,
                       experience_type: str,
                       date: date,
                       test_number: int) -> Dashboard:
    std_df = get_dataframe("STD", laboratory, researcher,
                           experience_type, date, test_number)
    hyst_df = get_dataframe("HYS", laboratory, researcher,
                            experience_type, date, test_number)
    return Dashboard(
        total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        stress_strain=export_plot(
            generate_select_stress_strain(std_df, hyst_df)),
        creep=export_plot(generate_creep(hyst_df)),
        hysteresis_area=export_plot(generate_hyst_area(hyst_df)),
        stiffness=export_plot(generate_stiffness(hyst_df)),
    )


def main():
    dashboard = generate_dashboard(laboratory="CCLAB",
                                   researcher="Vahid",
                                   experience_type="FA",
                                   date=date(year=2021, month=4, day=20),
                                   test_number=2)
    save_json(dashboard.stress_strain, "stress_strain.json")
    save_json(dashboard.creep, "creep.json")
    save_json(dashboard.hysteresis_area, "hyst_area.json")
    save_json(dashboard.stiffness, "stiffness.json")


if __name__ == "__main__":
    main()

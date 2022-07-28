import os
import numpy as np
import pandas as pd
from enum import Enum
from typing import List, Dict, Any, Optional
from pydantic import BaseModel
from bokeh import palettes
from pandas.core.frame import DataFrame
from sqlalchemy.future import select
from sqlalchemy.ext.asyncio import AsyncSession
from ccfatigue.models.database import Experiment, Test

DATA_DIRECTORY: str = "../fake_data/"

INTERVAL: int = 10
LOOP_SPACING: int = 1000
MAGNITUDE: int = -3


class TestPlot(BaseModel):
    """
    Defines Test metadata related to a plot
    """

    test_id: int
    specimen_id: int
    color: str
    total_dissipated_energy: int


class Plots(BaseModel):
    """
    Defines all plots types returned
    """

    stress_strain: Any
    creep: Any
    hysteresis_area: Any
    stiffness: Any


class DashboardPlots(BaseModel):
    """
    Defines data returned to the dashboard
    """

    tests: List[TestPlot]
    plots: Plots


class TopicName(Enum):
    """
    Defines matching between each topic and it's end user name
    """

    CREEP = ("creep", "Creep")
    HIGH = ("high", "High")
    HYST_AREA = ("hyst_area", "Hysteresis area")
    LOW = ("low", "Low")
    N_CYCLES = ("n_cycles", "Number of cycles")
    R_RATIO = ("r_ratio", "R ratio")
    STIFNESS = ("stiffness", "Stiffness")
    STRAIN = ("strain", "Strain")
    STRESS = ("stress", "Stress")
    STRESS_PARAM = ("stress_parameter", "Maximum Cyclic Stress")

    def __init__(self, key: str, label: str):
        self.key = key
        self.label = label


class Line(BaseModel):
    """
    Defines how each plot's line is described
    """

    data: Dict[str, List[Any]]
    legend_label: Optional[str]
    color: Optional[str]


class Plot(BaseModel):
    """
    Defines how each plot is described
    """

    title: str
    x_axis: TopicName
    y_axis: TopicName
    tooltips: List[TopicName]
    lines: List[Line] = []
    x_axis_type: str = "auto"
    y_axis_type: str = "auto"

    class Config:
        use_enum_values = True


def encoderNp(obj):
    if isinstance(obj, np.integer):
        return int(obj)
    if isinstance(obj, np.floating):
        return float(obj)
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    else:
        return obj


def get_dataframe(
    data_in: str,
    exp: Dict,
    specimen_id: int,
) -> DataFrame:
    """
    return extracted DataFrame related to that test from CSV
    """
    filename = f"{data_in}_{exp['date']}_{exp['experiment_type']}_{specimen_id:03d}.csv"
    filepath = os.path.join(
        DATA_DIRECTORY,
        exp["laboratory"],
        exp["researcher"],
        exp["experiment_type"],
        exp["date"],
        data_in,
        filename,
    )
    return pd.read_csv(os.path.abspath(filepath))


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """
    Compute subset of index used for plotting curves
    """
    n_cycles_max = np.max(df.n_cycles)

    sub_index_intermediate = np.geomspace(
        start=LOOP_SPACING, stop=n_cycles_max, num=INTERVAL
    )
    sub_index = np.round(sub_index_intermediate, MAGNITUDE)
    sub_index[0] = 1

    return sub_index


def create_sub_hystloops(
    df: DataFrame, sub_index: List[int]
) -> List[Dict[TopicName, List]]:
    """
    Conditional plotting of hysteresis loops
    we only plot the loops specified by sub_index
    """
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
                TopicName.N_CYCLES.key: sub_hystloops_ncycles,
                TopicName.STRAIN.key: sub_hystloops_strain,
                TopicName.STRESS.key: sub_hystloops_stress,
            }
        )
    return sub_hystloops


def generate_data_to_plot_stress_strain(
    tests: List[TestPlot], std_dfs: List[DataFrame], hyst_dfs: List[DataFrame]
) -> Plot:
    """
    return data used to generated plot "Stress - Strain"
    """
    sub_hystloops = [
        create_sub_hystloops(std_df, compute_sub_indexes(hyst_df))
        for std_df, hyst_df in zip(std_dfs, hyst_dfs)
    ]
    lines = [
        Line(
            data=loop,
            legend_label=test.specimen_id,
            color=test.color,
        )
        for test, loops in zip(tests, sub_hystloops)
        for loop in loops
    ]
    return Plot(
        title="Stress - Strain",
        x_axis=TopicName.STRAIN,
        y_axis=TopicName.STRESS,
        tooltips=[TopicName.STRESS, TopicName.STRAIN, TopicName.N_CYCLES],
        lines=lines,
    )


def generate_data_to_plot_creep(
    tests: List[TestPlot], hyst_dfs: List[DataFrame]
) -> Plot:
    """
    return data used to generated plot "Creep evolution"
    """
    lines = [
        Line(
            data={
                TopicName.N_CYCLES.key: hyst_df["n_cycles"].to_list(),
                TopicName.CREEP.key: hyst_df["creep"].to_list(),
            },
            legend_label=test.specimen_id,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Creep evolution",
        x_axis=TopicName.N_CYCLES,
        y_axis=TopicName.CREEP,
        tooltips=[TopicName.CREEP, TopicName.N_CYCLES],
        lines=lines,
    )


def generate_data_to_plot_hyst_area(
    tests: List[TestPlot], hyst_dfs: List[DataFrame]
) -> Plot:
    """
    return data used to generated plot "Hysteresis loop area evolution"
    """
    lines = [
        Line(
            data={
                TopicName.N_CYCLES.key: hyst_df["n_cycles"].to_list(),
                TopicName.HYST_AREA.key: hyst_df["hysteresis_area"].to_list(),
            },
            legend_label=test.specimen_id,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Hysteresis loop area evolution",
        x_axis=TopicName.N_CYCLES,
        y_axis=TopicName.HYST_AREA,
        tooltips=[TopicName.HYST_AREA, TopicName.N_CYCLES],
        lines=lines,
    )


def generate_data_to_plot_stiffness(
    tests: List[TestPlot], hyst_dfs: List[DataFrame]
) -> Plot:
    """
    return data used to generated plot "Stiffness evolution under cyclic loading"
    """
    lines = [
        Line(
            data={
                TopicName.N_CYCLES.key: hyst_df["n_cycles"].to_list(),
                TopicName.STIFNESS.key: hyst_df["stiffness"].to_list(),
            },
            legend_label=test.specimen_id,
            color=test.color,
        )
        for test, hyst_df in zip(tests, hyst_dfs)
    ]
    return Plot(
        title="Stiffness evolution under cyclic loading",
        x_axis=TopicName.N_CYCLES,
        y_axis=TopicName.STIFNESS,
        tooltips=[TopicName.STIFNESS, TopicName.N_CYCLES],
        lines=lines,
    )


def get_total_dissipated_energy(hyst_df: DataFrame) -> int:
    """
    return calculated Total Dissipated Energy (TDE)
    """
    return np.sum(hyst_df["hysteresis_area"])


async def generate_data_tests_dashboard_plots(
    session: AsyncSession, experiment_id: int, test_ids: List[int]
) -> DashboardPlots:
    """
    Generate  data for the 4 plots displayed in the Tests Dashboard view
    """
    colors = list(palettes.Category10_10)[: len(test_ids)]
    exp = dict(
        zip(
            ("laboratory", "researcher", "experiment_type", "date"),
            list(
                await session.execute(
                    select(
                        Experiment.laboratory,
                        Experiment.researcher,
                        Experiment.experiment_type,
                        Experiment.date,
                    ).where(Experiment.id == experiment_id)
                )
            )[0],
        )
    )
    test_2_specimen = {
        entry.id: entry.specimen_number
        for entry in list(
            await session.execute(
                select(Test.id, Test.specimen_number).where(
                    Test.experiment_id == experiment_id
                )
            )
        )
    }
    specimen_ids = [int(test_2_specimen[test_id]) for test_id in test_ids]
    std_dfs = [get_dataframe("STD", exp, specimen_id) for specimen_id in specimen_ids]
    hyst_dfs = [get_dataframe("HYS", exp, specimen_id) for specimen_id in specimen_ids]
    tests = [
        TestPlot(
            test_id=test_id,
            specimen_id=specimen_id,
            color=color,
            total_dissipated_energy=get_total_dissipated_energy(hyst_df),
        )
        for test_id, specimen_id, color, hyst_df in zip(
            test_ids, specimen_ids, colors, hyst_dfs
        )
    ]
    return DashboardPlots(
        tests=tests,
        plots=Plots(
            stress_strain=generate_data_to_plot_stress_strain(tests, std_dfs, hyst_dfs),
            creep=generate_data_to_plot_creep(tests, hyst_dfs),
            hysteresis_area=generate_data_to_plot_hyst_area(tests, hyst_dfs),
            stiffness=generate_data_to_plot_stiffness(tests, hyst_dfs),
        ),
    )

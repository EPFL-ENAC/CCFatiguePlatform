import json
import os
from enum import Enum
from typing import Any, Dict, List, Optional

from bokeh import palettes
from bokeh.embed import json_item
from bokeh.models.sources import ColumnDataSource
from bokeh.models.tools import HoverTool
from bokeh.plotting import figure, output_file, save
from pydantic.main import BaseModel

OUTPUT_DIRECTORY: str = './output'


class DataKey(Enum):
    CREEP = ('creep', 'Creep')
    HIGH = ('high', 'High')
    HYST_AREA = ('hyst_area', 'Hysteresis area')
    LOW = ('low', 'Low')
    N_CYCLES = ('n_cycles', 'Number of cycles')
    R_RATIO = ('r_ratio', 'R ratio')
    STIFNESS = ('stiffness', 'Stiffness')
    STRAIN = ('strain', 'Strain')
    STRESS = ('stress', 'Stress')
    STRESS_PARAM = ('stress_parameter', 'Maximum Cyclic Stress')

    def __init__(self, key: str, label: str):
        self.key = key
        self.label = label


class Line(BaseModel):
    data: Dict[DataKey, List[Any]]
    legend_label: Optional[str]
    color: Optional[str]


class Plot(BaseModel):
    title: str
    x_axis: DataKey
    y_axis: DataKey
    tooltips: List[DataKey]
    lines: List[Line] = []
    x_axis_type: str = 'auto'
    y_axis_type: str = 'auto'


def save_json(json_data: Any, filename: str) -> None:
    os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
    with open(os.path.join(OUTPUT_DIRECTORY, filename), 'w') as file:
        json.dump(json_data, file)


def export_plot(plot: Plot, save_html=False) -> Any:
    fig = figure(title=plot.title,
                 x_axis_label=plot.x_axis.label,
                 y_axis_label=plot.y_axis.label,
                 x_axis_type=plot.x_axis_type,
                 y_axis_type=plot.y_axis_type,
                 sizing_mode='stretch_both')
    fig.add_tools(HoverTool(
        tooltips=[(key.label, '@' + key.key) for key in plot.tooltips]
    ))
    for i, line in enumerate(plot.lines):
        data = {k.key: v for k, v in line.data.items()}
        source = ColumnDataSource(data=data)
        fig.line(x=plot.x_axis.key,
                 y=plot.y_axis.key,
                 source=source,
                 legend_label=line.legend_label
                 or plot.title,
                 color=line.color
                 or palettes.Category10_10[i % 10],
                 )
    if save_html:
        os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
        output_file(filename=os.path.join(OUTPUT_DIRECTORY,
                                          plot.title.lower() + '.html'))
        save(fig)
    return json_item(fig)

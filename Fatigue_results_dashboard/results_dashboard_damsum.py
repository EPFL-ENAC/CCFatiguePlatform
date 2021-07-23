import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.io import show
from bokeh.models import ColumnDataSource, HoverTool

DATA_DIRECTORY = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/CCFatigue/5_DammageSummation'
file_toplot = 'output_copied.csv'


def read_DS_piecewiseLinear(DATA_DIRECTORY, file_toplot):
    method = 'Piecewise-Linear'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    piecewiseLinear = pd.read_csv(filepath, header = None, sep = ';')
    piecewiseLinear.columns = ['max_stress_lev', 'n_cycles_fail']
    piecewiseLinear = piecewiseLinear.fillna('')
    return piecewiseLinear

def plot_damsum(piecewiseLinear):



    plotDS = figure(title = 'Dammage Summation', plot_width=1200, plot_height=800,
                  x_axis_label = "Number of passes to failure", y_axis_label = "Maximum stress level", x_axis_type = "log")
    plotDS.title.text_font_size = '20pt'
    plotDS.xaxis.axis_label_text_font_size = "18pt"
    plotDS.yaxis.axis_label_text_font_size = "18pt"
    plotDS.add_tools(HoverTool(tooltips=[("Number of cycles to failure", "@n_cycles_fail"), ("Maximum stress level", "@max_stress_lev")]))


    plotDS.line(x = 'n_cycles_fail', y = 'max_stress_lev', source = ColumnDataSource(data = piecewiseLinear), line_color = 'blue')

    show(plotDS)
    return





piecewiseLinear = read_DS_piecewiseLinear(DATA_DIRECTORY, file_toplot)
plot_damsum(piecewiseLinear)

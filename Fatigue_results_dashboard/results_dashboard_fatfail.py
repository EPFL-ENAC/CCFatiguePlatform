import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.io import show
from bokeh.models import ColumnDataSource, HoverTool

DATA_DIRECTORY = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/CCFatigue/4_FatigueFailure'
file_toplot = 'output.txt'
R_RATIO = -1.0


def read_FF_ST(DATA_DIRECTORY, file_toplot):
    method = 'ST'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    st = pd.read_fwf(filepath, widths = [13, 16, 13], index_col = False)
    st.columns = ['r_ratio', 'n_cycles', 'stress_param']
    st = st.fillna('')
    return st

def read_FF_kawai(DATA_DIRECTORY, file_toplot):
    method = 'Kawai'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    kawai = pd.read_fwf(filepath, widths = [13, 16, 12], index_col = False)
    kawai.columns = ['r_ratio', 'n_cycles', 'stress_param']
    kawai = kawai.fillna('')
    return kawai


def sub_r_ratio_ST(st, R_RATIO):
    sub_r_ratio_ST = []
    sub_n_cycles_ST = []
    sub_stress_ST = []
    for i in range(len(st)):
        if st.r_ratio[i] == R_RATIO:
            sub_r_ratio_ST.append(st.r_ratio[i])
            sub_n_cycles_ST.append(st.n_cycles[i])
            sub_stress_ST.append(st.stress_param[i])

    sub_ST = {'r_ratio': sub_r_ratio_ST,
    'n_cycles': sub_n_cycles_ST,
    'stress_param': sub_stress_ST}

    return sub_ST


def sub_r_ratio_kawai(kawai, R_RATIO):
    sub_r_ratio_kawai = []
    sub_n_cycles_kawai = []
    sub_stress_kawai = []
    for i in range(len(kawai)):
        if kawai.r_ratio[i] == R_RATIO:
            sub_r_ratio_kawai.append(kawai.r_ratio[i])
            sub_n_cycles_kawai.append(kawai.n_cycles[i])
            sub_stress_kawai.append(kawai.stress_param[i])

    sub_kawai = {'r_ratio': sub_r_ratio_kawai,
    'n_cycles': sub_n_cycles_kawai,
    'stress_param': sub_stress_kawai}

    return sub_kawai

def plot_FF(sub_ST, sub_kawai):
    plotFF = figure(title = 'Fatigue Failure', plot_width=1200, plot_height=800,
                  x_axis_label = "Number of cycles", y_axis_label = "Stress Amplitude", x_axis_type = "log")
    plotFF.add_tools(HoverTool(tooltips=[("R", "@r_ratio"), ("Number of cycles", "@n_cycles"), ("Stress", "@stress_param")]))
    plotFF.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_ST), line_color = 'blue')
    plotFF.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_kawai), line_color = 'red')
    show(plotFF)
    return


st = read_FF_ST(DATA_DIRECTORY, file_toplot)
kawai = read_FF_kawai(DATA_DIRECTORY, file_toplot)
sub_ST = sub_r_ratio_ST(st, R_RATIO)
sub_kawai = sub_r_ratio_kawai(kawai, R_RATIO)
plot_FF(sub_ST, sub_kawai)

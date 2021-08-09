import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.io import show
from bokeh.models import ColumnDataSource, HoverTool

DATA_DIRECTORY = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/CCFatigue/2_S-NCurves'
file_toplot = 'output.txt'
R_RATIO = -1.0

def read_SN_linlog(DATA_DIRECTORY, file_toplot):
    method = 'LinLog'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    linlog = pd.read_fwf(filepath, widths = [17, 12, 12, 12, 12], header = None)
    linlog.columns = ['Col1', 'N_cycles', 'stress_param', 'low', 'high']
    linlog = linlog.fillna('')
    return linlog


def read_SN_loglog(DATA_DIRECTORY, file_toplot):
    method = 'LogLog'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    loglog = pd.read_fwf(filepath, widths = [17, 12, 12, 12, 12], header = None)
    loglog.columns = ['Col1', 'N_cycles', 'stress_param', 'low', 'high']
    loglog = loglog.fillna('')
    return loglog


def read_SN_sendeckyj(DATA_DIRECTORY, file_toplot):
    method = 'Sendeckyj'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    sendeckyj = pd.read_fwf(filepath, widths = [17, 12, 12], header = None)
    sendeckyj.columns = ['Col1', 'N_cycles', 'stress_param']
    sendeckyj = sendeckyj.fillna('')
    return sendeckyj


def read_SN_whitney(DATA_DIRECTORY, file_toplot):
    method = 'Whitney'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    whitney = pd.read_fwf(filepath, widths = [17, 12, 12], header = None)
    whitney.columns = ['Col1', 'N_cycles', 'stress_param']
    whitney = whitney.fillna('')
    return whitney

def sub_r_ratio_linlog(linlog, R_RATIO):
    sub_stress_linlog = []
    sub_n_cycles_linlog = []
    param_linlog = []
    for i in range(len(linlog)):
        if linlog.Col1[i] == R_RATIO:
            sub_stress_linlog.append(linlog.stress_param[i])
            sub_n_cycles_linlog.append(linlog.N_cycles[i])
        elif (linlog.Col1[i] == 0 and linlog.Col1[i+1] == R_RATIO):
            for j in range(12):
                param_linlog.append(loglog.Col1[j])

    return sub_stress_linlog, sub_n_cycles_linlog, param_linlog

def sub_r_ratio_loglog(loglog, R_RATIO):
    sub_stress_loglog = []
    sub_n_cycles_loglog = []
    param_loglog = []
    for i in range(len(loglog)):
        if loglog.Col1[i] == R_RATIO:
            sub_stress_loglog.append(loglog.stress_param[i])
            sub_n_cycles_loglog.append(loglog.N_cycles[i])
        elif (loglog.Col1[i] == 0 and loglog.Col1[i+1] == R_RATIO):
            for j in range(12):
                param_loglog.append(loglog.Col1[j])

    return sub_stress_loglog, sub_n_cycles_loglog, param_loglog

def sub_r_ratio_sendeckyj(sendeckyj, R_RATIO):
    sub_stress_sendeckyj = []
    sub_n_cycles_sendeckyj = []
    param_sendeckyj = []
    for i in range(len(sendeckyj)):
        if sendeckyj.Col1[i] == R_RATIO:
            sub_stress_sendeckyj.append(sendeckyj.stress_param[i])
            sub_n_cycles_sendeckyj.append(sendeckyj.N_cycles[i])
        elif (sendeckyj.Col1[i] == 0 and sendeckyj.Col1[i+1] == R_RATIO):
            for j in range(12):
                param_sendeckyj.append(sendeckyj.Col1[j])
    return sub_stress_sendeckyj, sub_n_cycles_sendeckyj, param_sendeckyj

def sub_r_ratio_whitney(whitney, R_RATIO):
    sub_stress_whitney = []
    sub_n_cycles_whitney = []
    param_whitney = []
    for i in range(len(whitney)):
        if whitney.Col1[i] == R_RATIO:
            sub_stress_whitney.append(whitney.stress_param[i])
            sub_n_cycles_whitney.append(whitney.N_cycles[i])
        elif (whitney.Col1[i] == 0 and whitney.Col1[i+1] == R_RATIO):
            for j in range(12):
                param_whitney.append(whitney.Col1[j])
    return sub_stress_whitney, sub_n_cycles_whitney, param_whitney

def plot_sub_SN(sub_stress_linlog, sub_n_cycles_linlog, sub_stress_loglog, sub_n_cycles_loglog, sub_stress_sendeckyj, sub_n_cycles_sendeckyj, sub_stress_whitney, sub_n_cycles_whitney):

    sub_linlog_sn = {'n_cycles': sub_n_cycles_linlog,
                 'stress_param': sub_stress_linlog}
    sub_loglog_sn = {'n_cycles': sub_n_cycles_loglog,
                 'stress_param': sub_stress_loglog}
    sub_sendeckyj_sn = {'n_cycles': sub_n_cycles_sendeckyj,
                 'stress_param': sub_stress_sendeckyj}
    sub_whitney_sn = {'n_cycles': sub_n_cycles_whitney,
                 'stress_param': sub_stress_whitney}


    plotSN = figure(title = 'S-N curves', plot_width=1200, plot_height=800,
                  x_axis_label = "N", y_axis_label = "Maximum Cyclic Stress", x_axis_type = "log")
    plotSN.title.text_font_size = '20pt'
    plotSN.xaxis.axis_label_text_font_size = "18pt"
    plotSN.yaxis.axis_label_text_font_size = "18pt"
    plotSN.add_tools(HoverTool(tooltips=[("N", "@n_cycles"), ("Maximum cyclic stress", "@stress_param")]))
    plotSN.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_linlog_sn), line_color = 'blue')
    plotSN.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_loglog_sn), line_color = 'red')
    plotSN.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_sendeckyj_sn), line_color = 'green')
    plotSN.line(x = 'n_cycles', y = 'stress_param', source = ColumnDataSource(data = sub_whitney_sn), line_color = 'yellow')

    show(plotSN)
    return


linlog = read_SN_linlog(DATA_DIRECTORY, file_toplot)
loglog = read_SN_loglog(DATA_DIRECTORY, file_toplot)
sendeckyj = read_SN_sendeckyj(DATA_DIRECTORY, file_toplot)
whitney = read_SN_whitney(DATA_DIRECTORY, file_toplot)
sub_stress_linlog, sub_n_cycles_linlog, param_linlog = sub_r_ratio_linlog(linlog, R_RATIO)
sub_stress_loglog, sub_n_cycles_loglog, param_loglog = sub_r_ratio_loglog(loglog, R_RATIO)
sub_stress_sendeckyj, sub_n_cycles_sendeckyj, param_sendeckyj = sub_r_ratio_sendeckyj(sendeckyj, R_RATIO)
sub_stress_whitney, sub_n_cycles_whitney, param_whitney = sub_r_ratio_whitney(whitney, R_RATIO)
plot_sub_SN(sub_stress_linlog, sub_n_cycles_linlog, sub_stress_loglog, sub_n_cycles_loglog, sub_stress_sendeckyj, sub_n_cycles_sendeckyj, sub_stress_whitney, sub_n_cycles_whitney)

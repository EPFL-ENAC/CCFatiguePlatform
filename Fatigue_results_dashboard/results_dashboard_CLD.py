import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.io import show
from bokeh.models import ColumnDataSource, HoverTool

DATA_DIRECTORY = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/CCFatigue/3_CLD'
file_toplot = 'output.txt'


def read_CLD_Linear(DATA_DIRECTORY, file_toplot):
    method = 'Linear'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    linear = pd.read_fwf(filepath, widths = [17, 13, 16], index_col = False)
    linear.columns = ['n_cycles', 'stress_range', 'stress_mean']
    linear = linear.fillna('')
    return linear

def read_CLD_Piecewise_Linear(DATA_DIRECTORY, file_toplot):
    method = 'Piecewise-Linear'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    piecewise_linear = pd.read_fwf(filepath, widths = [17, 13, 16], index_col = False)
    piecewise_linear.columns = ['n_cycles', 'stress_range', 'stress_mean']
    piecewise_linear = piecewise_linear.fillna('')
    return piecewise_linear

def read_CLD_Piecewise_Nonlinear(DATA_DIRECTORY, file_toplot):
    method = 'Piecewise-Nonlinear'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    piecewise_nonlinear = pd.read_fwf(filepath, widths = [17, 13, 16], index_col = False)
    piecewise_nonlinear.columns = ['n_cycles', 'stress_range', 'stress_mean']
    piecewise_nonlinear = piecewise_nonlinear.fillna('')
    return piecewise_nonlinear

def read_CLD_Boerstra(DATA_DIRECTORY, file_toplot):
    method = 'Boerstra'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    boerstra = pd.read_fwf(filepath, widths = [17, 17, 12], index_col = False)
    boerstra.columns = ['n_cycles', 'stress_range', 'stress_mean']
    boerstra = boerstra.fillna('')
    return boerstra

def read_CLD_Kawai(DATA_DIRECTORY, file_toplot):
    method = 'Kawai'
    filepath = os.path.join(DATA_DIRECTORY, method, file_toplot)
    kawai = pd.read_fwf(filepath, widths = [17, 16, 13], index_col = False)
    kawai.columns = ['n_cycles', 'stress_range', 'stress_mean']
    kawai = kawai.fillna('')
    return kawai

def sub_n_linear(linear):
    sub_range_linear = []
    sub_mean_linear = []
    sub_n_cycles_linear = []
    n = linear.n_cycles[0]
    for i in range(len(linear)):
        if linear.n_cycles[i] == n:
            sub_range_linear.append(linear.stress_range[i])
            sub_mean_linear.append(linear.stress_mean[i])
            sub_n_cycles_linear.append(linear.n_cycles[i])
        else:
            n = linear.n_cycles[i]
            sub_range_linear.append(linear.stress_range[i])
            sub_mean_linear.append(linear.stress_mean[i])
            sub_n_cycles_linear.append(linear.n_cycles[i])

    return sub_range_linear, sub_mean_linear, sub_n_cycles_linear


def sub_n_piecewise_linear(piecewise_linear):
    sub_range_piecewise_linear = []
    sub_mean_piecewise_linear = []
    sub_n_cycles_piecewise_linear = []
    n = piecewise_linear.n_cycles[0]
    for i in range(len(piecewise_linear)):
        if piecewise_linear.n_cycles[i] == n:
            sub_range_piecewise_linear.append(piecewise_linear.stress_range[i])
            sub_mean_piecewise_linear.append(piecewise_linear.stress_mean[i])
            sub_n_cycles_piecewise_linear.append(piecewise_linear.n_cycles[i])
        else:
            n = piecewise_linear.n_cycles[i]
            sub_range_piecewise_linear.append(piecewise_linear.stress_range[i])
            sub_mean_piecewise_linear.append(piecewise_linear.stress_mean[i])
            sub_n_cycles_piecewise_linear.append(piecewise_linear.n_cycles[i])

    return sub_range_piecewise_linear, sub_mean_piecewise_linear, sub_n_cycles_piecewise_linear

def sub_n_piecewise_nonlinear(piecewise_nonlinear):
    sub_range_piecewise_nonlinear = []
    sub_mean_piecewise_nonlinear = []
    sub_n_cycles_piecewise_nonlinear = []
    n = piecewise_nonlinear.n_cycles[0]
    for i in range(len(piecewise_nonlinear)):
        if piecewise_nonlinear.n_cycles[i] == n:
            sub_range_piecewise_nonlinear.append(piecewise_nonlinear.stress_range[i])
            sub_mean_piecewise_nonlinear.append(piecewise_nonlinear.stress_mean[i])
            sub_n_cycles_piecewise_nonlinear.append(piecewise_nonlinear.n_cycles[i])
        else:
            n = piecewise_nonlinear.n_cycles[i]
            sub_range_piecewise_nonlinear.append(piecewise_nonlinear.stress_range[i])
            sub_mean_piecewise_nonlinear.append(piecewise_nonlinear.stress_mean[i])
            sub_n_cycles_piecewise_nonlinear.append(piecewise_nonlinear.n_cycles[i])

    return sub_range_piecewise_nonlinear, sub_mean_piecewise_nonlinear, sub_n_cycles_piecewise_nonlinear

def sub_n_boerstra(boerstra):
    sub_range_boerstra = []
    sub_mean_boerstra = []
    sub_n_cycles_boerstra = []
    n = boerstra.n_cycles[0]
    for i in range(len(boerstra)):
        if boerstra.n_cycles[i] == n:
            sub_range_boerstra.append(boerstra.stress_range[i])
            sub_mean_boerstra.append(boerstra.stress_mean[i])
            sub_n_cycles_boerstra.append(boerstra.n_cycles[i])
        else:
            n = boerstra.n_cycles[i]
            sub_range_boerstra.append(boerstra.stress_range[i])
            sub_mean_boerstra.append(boerstra.stress_mean[i])
            sub_n_cycles_boerstra.append(boerstra.n_cycles[i])

    return sub_range_boerstra, sub_mean_boerstra, sub_n_cycles_boerstra


def sub_n_kawai(kawai):
    sub_range_kawai = []
    sub_mean_kawai = []
    sub_n_cycles_kawai = []
    n = kawai.n_cycles[0]
    for i in range(len(kawai)):
        if kawai.n_cycles[i] == n:
            sub_range_kawai.append(kawai.stress_range[i])
            sub_mean_kawai.append(kawai.stress_mean[i])
            sub_n_cycles_kawai.append(kawai.n_cycles[i])
        else:
            n = kawai.n_cycles[i]
            sub_range_kawai.append(kawai.stress_range[i])
            sub_mean_kawai.append(kawai.stress_mean[i])
            sub_n_cycles_kawai.append(kawai.n_cycles[i])

    return sub_range_kawai, sub_mean_kawai, sub_n_cycles_kawai


def plot_CLD(sub_range_linear, sub_mean_linear, sub_n_cycles_linear, sub_range_piecewise_linear, sub_mean_piecewise_linear, sub_n_cycles_piecewise_linear, sub_range_piecewise_nonlinear, sub_mean_piecewise_nonlinear, sub_n_cycles_piecewise_nonlinear, sub_range_boerstra, sub_mean_boerstra, sub_n_cycles_boerstra, sub_range_kawai, sub_mean_kawai, sub_n_cycles_kawai):

    sub_linear_cld = {'n_cycles': sub_n_cycles_linear,
                 'stress_range': sub_range_linear,
                 'stress_mean': sub_mean_linear}

    sub_piecewise_linear_cld = {'n_cycles': sub_n_cycles_piecewise_linear,
                 'stress_range': sub_range_piecewise_linear,
                 'stress_mean': sub_mean_piecewise_linear}

    sub_piecewise_nonlinear_cld = {'n_cycles': sub_n_cycles_piecewise_nonlinear,
                 'stress_range': sub_range_piecewise_nonlinear,
                 'stress_mean': sub_mean_piecewise_nonlinear}

    sub_boerstra_cld = {'n_cycles': sub_n_cycles_boerstra,
                 'stress_range': sub_range_boerstra,
                 'stress_mean': sub_mean_boerstra}

    sub_kawai_cld = {'n_cycles': sub_n_cycles_kawai,
                 'stress_range': sub_range_kawai,
                 'stress_mean': sub_mean_kawai}



    plotCLD = figure(title = 'Constant Life Diagram (CLD)', plot_width=1200, plot_height=800,
                  x_axis_label = "Mean Stress", y_axis_label = "Stress Amplitude")
    plotCLD.add_tools(HoverTool(tooltips=[("N", "@n_cycles"), ("Stress range", "@stress_range"), ("Stress mean", "@stress_mean")]))
    plotCLD.line(x = 'stress_mean', y = 'stress_range', source = ColumnDataSource(data = sub_linear_cld), line_color = 'blue')
    plotCLD.line(x = 'stress_mean', y = 'stress_range', source = ColumnDataSource(data = sub_piecewise_linear_cld), line_color = 'red')
    plotCLD.line(x = 'stress_mean', y = 'stress_range', source = ColumnDataSource(data = sub_piecewise_nonlinear_cld), line_color = 'green')
    #plotCLD.line(x = 'stress_mean', y = 'stress_range', source = ColumnDataSource(data = sub_boerstra_cld))
    #plotCLD.line(x = 'stress_mean', y = 'stress_range', source = ColumnDataSource(data = sub_kawai_cld))
    print(sub_kawai_cld)
    show(plotCLD)
    return

linear = read_CLD_Linear(DATA_DIRECTORY, file_toplot)
piecewise_linear = read_CLD_Piecewise_Linear(DATA_DIRECTORY, file_toplot)
piecewise_nonlinear = read_CLD_Piecewise_Nonlinear(DATA_DIRECTORY, file_toplot)
boerstra = read_CLD_Boerstra(DATA_DIRECTORY, file_toplot)
kawai = read_CLD_Kawai(DATA_DIRECTORY, file_toplot)


sub_range_linear, sub_mean_linear, sub_n_cycles_linear = sub_n_linear(linear)
sub_range_piecewise_linear, sub_mean_piecewise_linear, sub_n_cycles_piecewise_linear = sub_n_piecewise_linear(piecewise_linear)
sub_range_piecewise_nonlinear, sub_mean_piecewise_nonlinear, sub_n_cycles_piecewise_nonlinear = sub_n_piecewise_nonlinear(piecewise_nonlinear)
sub_range_boerstra, sub_mean_boerstra, sub_n_cycles_boerstra = sub_n_boerstra(boerstra)
sub_range_kawai, sub_mean_kawai, sub_n_cycles_kawai = sub_n_kawai(kawai)



plot_CLD(sub_range_linear, sub_mean_linear, sub_n_cycles_linear, sub_range_piecewise_linear, sub_mean_piecewise_linear, sub_n_cycles_piecewise_linear, sub_range_piecewise_nonlinear, sub_mean_piecewise_nonlinear, sub_n_cycles_piecewise_nonlinear, sub_range_boerstra, sub_mean_boerstra, sub_n_cycles_boerstra, sub_range_kawai, sub_mean_kawai, sub_n_cycles_kawai)

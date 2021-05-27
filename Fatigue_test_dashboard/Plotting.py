#!/usr/bin/env python
# coding: utf-8

# In[1]:


import matplotlib.pyplot as plt
import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.palettes import Spectral6
from bokeh.io import export_png, output_file, show
from bokeh.models import ColumnDataSource, HoverTool, LinearColorMapper
import bokeh.plotting as bk
from bokeh.transform import linear_cmap
#from bokeh.sampledata.stocks import AAPL


# In[2]:


## Inputs
data_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs'
filename = 'Vahid_std.csv'
filepath = os.path.join(data_dir,filename)

df = pd.read_csv(filepath)
print(df)


# ## Plotting plot Stress - Strain
# 
# def plot_stress_strain(df):
#     plt.figure(0, figsize = (10,7))
#     plt.title("Stress - Strain")
#     plt.xlabel("Strain")
#     plt.ylabel("Stress")
#     plt.plot(df.Machine_Displacement, df.Machine_Load)
#     save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/Stress_strain.png'
#     plt.savefig(save_dir)
# print(plot_stress_strain(df))

# In[3]:


### Conditional plotting of hysteresis loops

sub_hystloops_strain = []
sub_hystloops_stress = []
sub_hystloops_ncycles = []
sub_index = [0, 1, 5, 10, 100, 200, 1000, 2000, 10000, 50000, 100000, 500000, 1000000]

for i in range(len(df)):
    for j in range(len(sub_index)):
        if df.Machine_N_cycles[i] == sub_index[j]:
            sub_hystloops_strain.append(df.Machine_Displacement[i])
            sub_hystloops_stress.append(df.Machine_Load[i])
            sub_hystloops_ncycles.append(df.Machine_N_cycles[i])
    


# In[4]:


sub_hystloops = {'n_cycles': sub_hystloops_ncycles,
                 'strain': sub_hystloops_strain,
                 'stress': sub_hystloops_stress}

def plot_select_stress_strain(sub_hystloops):
    stressStrain = figure(title = 'Stress - Strain', plot_width=1200, plot_height=800,
                          x_axis_label = "Strain", y_axis_label = "Stress")
    
    
    stressStrain.add_tools(HoverTool(tooltips=[("Stress", "@stress"), ("Strain", "@strain"),
                                               ("Nb. cycles", "@n_cycles")]))
    stressStrain.line(x = 'strain', y = 'stress', source = ColumnDataSource(data = sub_hystloops))
    show(stressStrain)    
print(plot_select_stress_strain(sub_hystloops))


# In[ ]:





# In[ ]:





# In[5]:


## Plotting Stress - Strain with Bokeh library

total_strain = df.Machine_Displacement
total_stress = df.Machine_Load


total_stress_strain = {'strain': total_strain,
             'stress': total_stress}

def plot_total_stress_strain(df):
    stress_strain = figure(title = 'Stress - Strain', plot_width=1200, plot_height=800,
                           x_axis_label = "Strain", y_axis_label = "Stress")
    stress_strain.add_tools(HoverTool(tooltips=[("Stress", "@stress"), ("Strain", "@strain")]))
    stress_strain.line(x = 'strain', y = 'stress', source = ColumnDataSource(data = total_stress_strain))
    show(stress_strain)
    
print(plot_total_stress_strain(df))


# ## Plotting plot Load curve
# 
# def plot_Load_curve(df):
#     plt.figure(1,figsize = (10,7))
#     plt.title("Load curve")
#     plt.xlabel("Number of cycles")
#     plt.ylabel("Stress")
#     plt.plot(df.Machine_N_cycles, df.Machine_Load)
#     save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/Load_curve.png'
#     plt.savefig(save_dir)
# print(plot_Load_curve(df))

# In[6]:


def plot_Load_curve(df):
    loadCurve = figure(title = 'Load curve', plot_width=1200, plot_height=800, x_axis_label = "Number of cycles", y_axis_label = "Stress")
    loadCurve.line(df.Machine_N_cycles, df.Machine_Load)
    show(loadCurve)
    
print(plot_Load_curve(df))


# In[7]:


#save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/Load_curve.png'

#plt.savefig(save_dir)

##function to save plots!


# ## Plotting plot Strain - N_cycles
# 
# def plot_Strain_evolution(df):
#     plt.figure(1, figsize = (10,7))
#     plt.title("Strain evolution with N cycles")
#     plt.xlabel("Number of cycles")
#     plt.ylabel("Strain")
#     plt.plot(df.Machine_N_cycles, df.Machine_Displacement)
#     save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/Strain_curve.png'
#     plt.savefig(save_dir)
# print(plot_Strain_evolution(df))

# In[8]:


def plot_Strain_envelope(df):
    strainEnvelope = figure(title = 'Strain envelope', plot_width=1200, plot_height=800,
                            x_axis_label = "Number of cycles", y_axis_label = "Strain")
    strainEnvelope.line(df.Machine_N_cycles, df.Machine_Displacement)
    show(strainEnvelope)
    
print(plot_Strain_envelope(df))


# In[ ]:





# In[9]:


### Importing Hysteresis loops analysis file

filename = 'Vahid_hysteresis_analysis.csv'
filepath = os.path.join(data_dir, filename)

hyst_df = pd.read_csv(filepath, sep = ',', header = 0)


# In[10]:


### Importing Hysteresis loops areas

filename = 'Vahid_hystarea.csv'
filepath = os.path.join(data_dir,filename)

hystarea = pd.read_csv(filepath, sep='\t',header=0)


# In[11]:


### Importing N_cycles without duplicates

filename = 'Vahid_n_cycles.csv'
filepath = os.path.join(data_dir,filename)

n_cycles = pd.read_csv(filepath, sep='\t',header=0)


# In[12]:


### Importing Stiffness values

filename = 'Vahid_stiff.csv'
filepath = os.path.join(data_dir,filename)

stiffness = pd.read_csv(filepath, sep='\t',header=0)


# In[13]:


### Creep envelope
creep_strain = hyst_df.creep
creep_n_cycles = hyst_df.n_cycles


sub_creep = {'n_cycles': creep_n_cycles,
             'creep': creep_strain}

def plot_creep(hyst_df):
    creep = figure(title = 'Creep evolution', plot_width=1200, plot_height=800,
                   x_axis_label = "N_cycles", y_axis_label = "Creep")
    creep.add_tools(HoverTool(tooltips=[("Creep", "@creep"), ("Nb. cycles", "@n_cycles")]))
    creep.line(x = 'n_cycles', y = 'creep', source = ColumnDataSource(data = sub_creep))
    show(creep)
    
print(plot_creep(hyst_df))


# ### Hysteresis loops area evolution plots
# 
# def plot_hysteresis_area(hyst_df):
#     plt.figure(1, figsize = (10,7))
#     plt.title("Hysteresis loops area evolution")
#     plt.xlabel("Number of cycles")
#     plt.ylabel("Area")
#     plt.plot(hyst_df.n_cycles, hyst_df.hysteresis_area)
#     save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/hysteresis_area_curve.png'
#     plt.savefig(save_dir)
# print(plot_hysteresis_area(hyst_df))

# In[18]:


### Hysteresis area plot

hyst_area = hyst_df.hysteresis_area
hyst_n_cycles = hyst_df.n_cycles


sub_hyst = {'n_cycles': hyst_n_cycles,
             'area': hyst_area}

TDE = np.sum(hyst_area)

def plot_hystarea(hyst_df):
    area = figure(title = 'Hysteresis loop area evolution', plot_width=1200, plot_height=800,
                  x_axis_label = "N_cycles", y_axis_label = "Hysteresis area")
    area.add_tools(HoverTool(tooltips=[("area", "@area"), ("Nb. cycles", "@n_cycles")]))
    area.line(x = 'n_cycles', y = 'area', source = ColumnDataSource(data = sub_hyst))
    show(area)
    
print(plot_hystarea(hyst_df))


# In[19]:


print(TDE)


# def plot_stiffness(hyst_df):
#     plt.figure(1, figsize = (10,7))
#     plt.title("Stiffness evolution")
#     plt.xlabel("Number of cycles")
#     plt.ylabel("Stiffness")
#     plt.plot(hyst_df.n_cycles, hyst_df.stiffness)
#     save_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/saved_plots/stiffness.png'
#     plt.savefig(save_dir)
# print(plot_stiffness(hyst_df))

# In[16]:


### Stiffness evolution plot

hyst_stiff = hyst_df.stiffness
hyst_n_cycles = hyst_df.n_cycles


sub_hyst = {'n_cycles': hyst_n_cycles,
             'stiffness': hyst_stiff}


def plot_stiffness(hyst_df):
    stiff = figure(title = 'Stiffness evolution under cyclic loading', plot_width=1200, plot_height=800,
                   x_axis_label = "N_cycles", y_axis_label = "Stiffness")
    stiff.add_tools(HoverTool(tooltips=[("stiffness", "@stiffness"), ("Nb. cycles", "@n_cycles")]))
    stiff.line(x = 'n_cycles', y = 'stiffness', source = ColumnDataSource(data = sub_hyst))
    show(stiff)
    
print(plot_stiffness(hyst_df))


# In[17]:


print(hyst_df)


# In[ ]:





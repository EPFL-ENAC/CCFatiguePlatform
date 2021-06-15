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



## Importing input file
input_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example/CCLab/Vahid/Fatigue/210420/STD'
filename='STD_VAH_210420_FA_002.txt'
filepath = os.path.join(input_path,filename)

df = pd.read_csv(filepath)
### Importing Hysteresis loops analysis file

input_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example/CCLab/Vahid/Fatigue/210420/HYS'
filename='HYS_VAH_210420_FA_002.txt'
filepath = os.path.join(input_path,filename)

hyst_df = pd.read_csv(filepath, sep = ',', header = 0)


#print(df)


### Conditional plotting of hysteresis loops

sub_hystloops_strain = []
sub_hystloops_stress = []
sub_hystloops_ncycles = []

### We select loops for plotting on an arbitrary basis (subject to modifications)
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
plot_select_stress_strain(sub_hystloops)



### Plotting Stress - Strain with Bokeh library

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

plot_total_stress_strain(df)



##* Plotting Load curve with Bokeh library


def plot_Load_curve(df):
    loadCurve = figure(title = 'Load curve', plot_width=1200, plot_height=800, x_axis_label = "Number of cycles", y_axis_label = "Stress")
    loadCurve.line(df.Machine_N_cycles, df.Machine_Load)
    show(loadCurve)

plot_Load_curve(df)



### Plotting strain envelope with Bokeh library


def plot_Strain_envelope(df):
    strainEnvelope = figure(title = 'Strain envelope', plot_width=1200, plot_height=800,
                            x_axis_label = "Number of cycles", y_axis_label = "Strain")
    strainEnvelope.line(df.Machine_N_cycles, df.Machine_Displacement)
    show(strainEnvelope)

plot_Strain_envelope(df)






### Importing Hysteresis loops areas



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

plot_creep(hyst_df)



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

plot_hystarea(hyst_df)


# In[19]:


print(TDE)




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

plot_stiffness(hyst_df)








# %%

import os
import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.io import show
from bokeh.models import ColumnDataSource, HoverTool

# Constantes

DATA_DIRECTORY = '../data/'

DATE = '2021-04-20'
TEST_TYPE = 'FA'
TEST_NUMBER = '002'
LAB = "CCLAB"
RESEARCHER = 'Vahid'

INTERVAL = 10
LOOP_SPACING = 1000
MAGNITUDE = -3


# Importing input File_size

# Data in standard format


data_in = 'STD'
filename = data_in+'_'+DATE+'_'+TEST_TYPE+'_'+TEST_NUMBER+'.csv'


filepath = os.path.join(DATA_DIRECTORY, LAB, RESEARCHER, TEST_TYPE, DATE, data_in, filename)

df = pd.read_csv(filepath)
### Importing Hysteresis loops analysis file

# Data treated in hysteresis analysis

data_in = 'HYS'
filename = data_in+'_'+DATE+'_'+TEST_TYPE+'_'+TEST_NUMBER+'.csv'


filepath = os.path.join(DATA_DIRECTORY, LAB, RESEARCHER, TEST_TYPE, DATE, data_in, filename)

hyst_df = pd.read_csv(filepath, sep = ',', header = 0)


data_in = 'STD'
filename = data_in+'_'+DATE+'_'+TEST_TYPE+'_'+'005'+'.csv'


filepath = os.path.join(DATA_DIRECTORY, LAB, RESEARCHER, TEST_TYPE, DATE, data_in, filename)

df2 = pd.read_csv(filepath)
### Importing Hysteresis loops analysis file

# Data treated in hysteresis analysis

data_in = 'HYS'
filename = data_in+'_'+DATE+'_'+TEST_TYPE+'_'+'005'+'.csv'


filepath = os.path.join(DATA_DIRECTORY, LAB, RESEARCHER, TEST_TYPE, DATE, data_in, filename)

hyst_df2 = pd.read_csv(filepath, sep = ',', header = 0)




# Plotting Stress Strain curves, we use the stress - strain values from the data in standard format

### We select loops for plotting on an arbitrary basis (subject to modifications)
#sub_index = [0, 1, 5, 10, 100, 200, 1000, 2000, 10000, 50000, 100000, 500000, 1000000]
def calculate_sub_index(hyst_df, INTERVAL, LOOP_SPACING, MAGNITUDE):
    '''
    Arguments: N_cycles column of the hyst df

    Returns: Subset of index used for plotting curves

    Description:

    '''
    n_cycles_max = np.max(hyst_df.n_cycles)
    n_cycles_min = np.min(hyst_df.n_cycles)

    sub_index_intermediate = np.geomspace(start = LOOP_SPACING, stop = n_cycles_max, num = INTERVAL)
    sub_index = np.round(sub_index_intermediate, MAGNITUDE)
    sub_index[0] = 1

    return sub_index
    # create range max_cycles - min cycles (n points log spaced)
    # find closest value from previous range in dataset
    #return sub_index

#def select_loops(df, sub_index):
#    '''
#    Arguments:
#
#    Returns:
#
#    Description:
#    '''

def create_sub_hystloops(df, sub_index):
    nb_curve = len(sub_index)
    ### Conditional plotting of hysteresis loops - we only plot the loops specified by sub_index
    sub_hystloops = []
    for j in range(nb_curve):
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

        sub_hystloops.append({'n_cycles': sub_hystloops_ncycles,
                                 'strain': sub_hystloops_strain,
                                 'stress': sub_hystloops_stress})

    print(sub_hystloops)
    return sub_hystloops

def plot_select_stress_strain(sub_hystloops):
    stressStrain = figure(title = 'Stress - Strain', plot_width=1200, plot_height=800,
                          x_axis_label = "Strain", y_axis_label = "Stress")


    stressStrain.add_tools(HoverTool(tooltips=[("Stress", "@stress"), ("Strain", "@strain"),
                                               ("Nb. cycles", "@n_cycles")]))
    for curve in sub_hystloops:
        stressStrain.line(x = 'strain', y = 'stress', source = ColumnDataSource(data = curve))
    #for curve in sub_hystloops2:
    #    stressStrain.line(x = 'strain', y = 'stress', source = ColumnDataSource(data = curve), line_color = 'red')

    show(stressStrain)





### Plotting Stress - Strain (all loops) using data in standard format



def plot_total_stress_strain(df):
    total_strain = df.Machine_Displacement
    total_stress = df.Machine_Load


    total_stress_strain = {'strain': total_strain,
                 'stress': total_stress}
    stress_strain = figure(title = 'Stress - Strain', plot_width=1200, plot_height=800,
                           x_axis_label = "Strain", y_axis_label = "Stress")
    stress_strain.add_tools(HoverTool(tooltips=[("Stress", "@stress"), ("Strain", "@strain")]))
    stress_strain.line(x = 'strain', y = 'stress', source = ColumnDataSource(data = total_stress_strain))
    show(stress_strain)




##* Plotting Load curve with Bokeh library using standard format data


def plot_Load_curve(df):
    loadCurve = figure(title = 'Load curve', plot_width=1200, plot_height=800, x_axis_label = "Number of cycles", y_axis_label = "Stress")
    loadCurve.line(df.Machine_N_cycles, df.Machine_Load)
    show(loadCurve)





### Plotting strain envelope with Bokeh library usiing data in standard format


def plot_Strain_envelope(df):
    strainEnvelope = figure(title = 'Strain envelope', plot_width=1200, plot_height=800,
                            x_axis_label = "Number of cycles", y_axis_label = "Strain")
    strainEnvelope.line(df.Machine_N_cycles, df.Machine_Displacement)
    #strainEnvelope.line(df2.Machine_N_cycles, df2.Machine_Displacement, line_color = 'red')
    show(strainEnvelope)








### Creep evolution - using data from hysteresis analysis
creep_strain = hyst_df.creep
creep_n_cycles = hyst_df.n_cycles

#creep_strain2 = hyst_df2.creep
#creep_n_cycles2 = hyst_df2.n_cycles

sub_creep = {'n_cycles': creep_n_cycles,
             'creep': creep_strain}
#sub_creep2 = {'n_cycles': creep_n_cycles2,
#             'creep': creep_strain2}

def plot_creep(hyst_df):
    creep = figure(title = 'Creep evolution', plot_width=1200, plot_height=800,
                   x_axis_label = "N_cycles", y_axis_label = "Creep")
    creep.add_tools(HoverTool(tooltips=[("Creep", "@creep"), ("Nb. cycles", "@n_cycles")]))
    creep.line(x = 'n_cycles', y = 'creep', source = ColumnDataSource(data = sub_creep))
    #creep.line(x = 'n_cycles', y = 'creep', source = ColumnDataSource(data = sub_creep2), line_color = 'red')
    show(creep)





### Hysteresis area plot - using data from hysteresis analysis

hyst_area = hyst_df.hysteresis_area
#hyst_area2 = hyst_df2.hysteresis_area
hyst_n_cycles = hyst_df.n_cycles
#hyst_n_cycles2 = hyst_df2.n_cycles


sub_hyst = {'n_cycles': hyst_n_cycles,
             'area': hyst_area}

#sub_hyst2 = {'n_cycles': hyst_n_cycles2,
#             'area': hyst_area2}

def calculate_tde(hyst_area):
    tde = np.sum(hyst_area)
    #tde2 = np.sum(hyst_area2)
    return tde #, tde2

def plot_hystarea(hyst_df):



    area = figure(title = 'Hysteresis loop area evolution', plot_width=1200, plot_height=800,
                  x_axis_label = "N_cycles", y_axis_label = "Hysteresis area")
    area.add_tools(HoverTool(tooltips=[("area", "@area"), ("Nb. cycles", "@n_cycles")]))
    area.line(x = 'n_cycles', y = 'area', source = ColumnDataSource(data = sub_hyst))
    #area.line(x = 'n_cycles', y = 'area', source = ColumnDataSource(data = sub_hyst2), line_color = 'red')

    show(area)








### Stiffness evolution plot - data from hysteresis analysis




def plot_stiffness(hyst_df):
    hyst_stiff = hyst_df.stiffness
    hyst_n_cycles = hyst_df.n_cycles

    #hyst_stiff2 = hyst_df2.stiffness
    #hyst_n_cycles2 = hyst_df2.n_cycles

    sub_hyst = {'n_cycles': hyst_n_cycles,
                 'stiffness': hyst_stiff}

    #sub_hyst2 = {'n_cycles': hyst_n_cycles2,
    #             'stiffness': hyst_stiff2}

    stiff = figure(title = 'Stiffness evolution under cyclic loading', plot_width=1200, plot_height=800,
                   x_axis_label = "N_cycles", y_axis_label = "Stiffness")
    stiff.add_tools(HoverTool(tooltips=[("stiffness", "@stiffness"), ("Nb. cycles", "@n_cycles")]))
    stiff.line(x = 'n_cycles', y = 'stiffness', source = ColumnDataSource(data = sub_hyst))
    #stiff.line(x = 'n_cycles', y = 'stiffness', source = ColumnDataSource(data = sub_hyst2), line_color = 'red')

    show(stiff)


def get_r_ratio():

    return


def main():

    sub_index = calculate_sub_index(hyst_df, INTERVAL, LOOP_SPACING, MAGNITUDE)
    #sub_index2 = calculate_sub_index(hyst_df2, INTERVAL, LOOP_SPACING, MAGNITUDE)

    sub_hystloops = create_sub_hystloops(df, sub_index)
    #sub_hystloops2 = create_sub_hystloops(df2, sub_index2)

    plot_select_stress_strain(sub_hystloops)
    #plot_select_stress_strain(sub_hystloops, sub_hystloops2)

    #plot_total_stress_strain(df)
    #plot_Load_curve(df)
    #plot_Strain_envelope(df, df2)
    plot_creep(hyst_df)
    #plot_creep(hyst_df, hyst_df2)

    plot_hystarea(hyst_df)
    #plot_hystarea(hyst_df, hyst_df2)

    plot_stiffness(hyst_df)
    #plot_stiffness(hyst_df, hyst_df2)

    TDE = calculate_tde(hyst_area)
    #TDE = calculate_tde(hyst_area, hyst_area2)

    print(TDE)
    return


if __name__ == "__main__":
    main()



# %%

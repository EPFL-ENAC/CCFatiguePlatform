#!/usr/bin/env python
# coding: utf-8

# In[1]:


import matplotlib.pyplot as plt
import os
from numpy.core.numeric import NaN
import pandas as pd
import numpy as np
import bokeh as bk
from collections import Counter
from scipy import stats

### Importing data from csv file
def read_std(data_directory, data_type, res, date, test_type, test_number, lab, researcher, loading):
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'_'+test_number+'.txt'
    filepath = os.path.join(data_directory, lab, researcher, loading, date, data_type, filename)
    return pd.read_csv(filepath, header = 0)


### Importing meta data from csv
def read_met(data_directory, data_type, res, date, test_type, test_number, lab, researcher, loading):
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'_'+test_number+'.txt'
    filepath = os.path.join(data_directory, lab, researcher, loading, date, data_type, filename)
    return pd.read_csv(filepath, header = 0)

### Writing output file (HYS)
def write_hys(hyst_df, data_directory, data_type, res, date, test_type, test_number, lab, researcher, loading):
    '''
    arguments
    valeur de retour (void si return rien)
    Description:
    '''
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'_'+test_number+'.txt'
    filepath = os.path.join(data_directory, lab, researcher, loading, date, data_type, filename)
    hyst_df.to_csv(path_or_buf=filepath, index=False)
    return



### Create dataframe function
def create_hyst_df():
    hyst_col = ['n_cycles', 'hysteresis_area', 'stiffness', 'creep']
    return pd.DataFrame(columns = hyst_col)


### Isolate Stress/Strain values for each cycles
def get_stress_strain(df, n_cycles):
    Stress_N = []
    Strain_N = []
    df0=np.array(df)
    for k in range(len(n_cycles)):
        Stress_N.append([df0[i,1] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])
        Strain_N.append([df0[i,2] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])

    return Stress_N, Strain_N

def fill_hyst_df(df, meta_df, hyst_df):

    # NB CYCLES
    ### Extract number of cycles without repeating values in other table, and store number of measurements per cycle
    n_cycles = sorted(set(df.Machine_N_cycles))
    hyst_df.n_cycles = n_cycles
    #n_measurements = Counter(df.Machine_N_cycles)
    #n_cycles_df = pd.DataFrame(n_cycles)


    # HYSTERESIS & STIFFNESS
    ### Definition of polyarea function
    def PolyArea(x,y):
        return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

    Hysteresis_Area = []
    Stiffness = []
    n = 0
    Stress_N, Strain_N = get_stress_strain(df, n_cycles)
    for j in range(len(n_cycles)):
        x=Stress_N[j]
        y=Strain_N[j]
        points=(x, y)
        if j<(len(n_cycles)-1):
            Hysteresis_Area.append(PolyArea(x, y)*(n_cycles[j+1]-n_cycles[j]))
        else: Hysteresis_Area.append(PolyArea(x, y)*(int(meta_df.N_fail[0])-n_cycles[j]))
        if j > 0:
            #slope, intercept, r_value, p_value, std_err = stats.linregress(y, x)
            slope, _, _, _, _ = stats.linregress(y, x)
            Stiffness.append(slope)
    hyst_df.hysteresis_area = Hysteresis_Area
    Stiffness.insert(0, np.nan)

    # CREEP
    creep = []
    ### Creep computation
    #Stress_max = [np.max(Stress_N[i]) for i in range(1,len(n_cycles))]
    #Stress_min = [np.min(Stress_N[i]) for i in range(1,len(n_cycles))]
    Strain_max = [np.max(Strain_N[i]) for i in range(1,len(n_cycles))]
    Strain_min = [np.min(Strain_N[i]) for i in range(1,len(n_cycles))]

    for i in range(len(n_cycles)-1):
        #dStress = Stress_max[i]-Stress_min[i]
        #dStrain = Strain_max[i]-Strain_min[i]
        #Stiffness.append(dStress/dStrain)
        creep.append((Strain_max[i]+Strain_min[i])/2)
    creep.insert(0, np.nan)
    hyst_df.stiffness = Stiffness
    hyst_df.creep = creep

    return


def main():

    res = 'VAH'
    date = '210420'
    test_type = 'FA'
    test_number = '002'
    lab = "CCLab"
    researcher = 'Vahid'
    loading = 'Fatigue'
    data_directory = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example'

    df = read_std(data_directory, 'STD', res, date, test_type, test_number, lab, researcher, loading)
    meta = read_met(data_directory, 'MET',  res, date, test_type, test_number, lab, researcher, loading)
    hyst = create_hyst_df()
    fill_hyst_df(df, meta, hyst)

    write_hys(hyst, data_directory, 'HYS', res, date, test_type, test_number, lab, researcher, loading)

    return


if __name__ == "__main__":
    main()

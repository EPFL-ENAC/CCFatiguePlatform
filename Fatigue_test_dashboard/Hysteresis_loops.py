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

data_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs'
filename = 'Vahid_std.csv'
filepath = os.path.join(data_dir,filename)

df = pd.read_csv(filepath)


### Creating dataframe structure

hyst_col = ['n_cycles', 'hysteresis_area', 'stiffness', 'creep']
hyst_df = pd.DataFrame(columns = hyst_col)



### Extract number of cycles without repeating values in other table, and store number of measurements per cycle

n_cycles = sorted(set(df.Machine_N_cycles))
n_measurements = Counter(df.Machine_N_cycles)  


n_cycles_df = pd.DataFrame(n_cycles)
hyst_df.n_cycles = n_cycles

### Isolate Stress/Strain values for each cycles


Stress_N = []
Strain_N = []
df0=np.array(df)
for k in range(len(n_cycles)):
    Stress_N.append([df0[i,1] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])
    Strain_N.append([df0[i,2] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])
    

### Definition of polyarea function
def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))


### Hysteresis loops area computation

Hysteresis_Area = []
Stiffness = []
creep = []
n = 0


for j in range(len(n_cycles)):
    x=Stress_N[j]
    y=Strain_N[j]
    points=(x, y)
    Hysteresis_Area.append(PolyArea(x, y)*(n_cycles[i+1]-n_cycles[i]))
    if j > 0:
        slope, intercept, r_value, p_value, std_err = stats.linregress(y, x)
        Stiffness.append(slope)
    
hyst_df.hysteresis_area = Hysteresis_Area



print(Stiffness)



### Creep computation


Stress_max = [np.max(Stress_N[i]) for i in range(1,len(n_cycles))]
Stress_min = [np.min(Stress_N[i]) for i in range(1,len(n_cycles))]
Strain_max = [np.max(Strain_N[i]) for i in range(1,len(n_cycles))]
Strain_min = [np.min(Strain_N[i]) for i in range(1,len(n_cycles))]


for i in range(len(n_cycles)-1):
    #dStress = Stress_max[i]-Stress_min[i]
    #dStrain = Strain_max[i]-Strain_min[i]
    #Stiffness.append(dStress/dStrain)
    creep.append(Strain_min[i])
    

Stiffness.append(np.nan)
creep.append(np.nan)
hyst_df.stiffness = Stiffness
hyst_df.creep = creep



print(hyst_df)


# In[11]:


hyst_df.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_hysteresis_analysis.csv', index=False)


# In[ ]:





#!/usr/bin/env python
# coding: utf-8

# In[1]:


import matplotlib.pyplot as plt
import os
import pandas as pd
import numpy as np
import bokeh as bk
from collections import Counter
from scipy import stats


# In[2]:


### Import data

data_dir = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs'
filename = 'Vahid_std.csv'
filepath = os.path.join(data_dir,filename)

df = pd.read_csv(filepath)
#print(df)


# In[3]:


hyst_col = ['n_cycles', 'hysteresis_area', 'stiffness', 'creep']
hyst_df = pd.DataFrame(columns = hyst_col)
#hyst_df.n_cycles = n_cycles


# In[4]:


### Extract number of cycles without repeating values in other table, and store number of measurements per cycle

n_cycles = sorted(set(df.Machine_N_cycles))
n_measurements = Counter(df.Machine_N_cycles)  


#print(n_cycles)
#print(n_measurements)
n_cycles_df = pd.DataFrame(n_cycles)
#n_cycles_df.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_n_cycles.csv', index=False)

hyst_df.n_cycles = n_cycles


# In[5]:


### Isolate Stress/Strain values for each cycles


Stress_N = []
Strain_N = []
df0=np.array(df)
for k in range(len(n_cycles)):
    Stress_N.append([df0[i,1] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])
    Strain_N.append([df0[i,2] for i in range(df0.shape[0]) if df0[i,0]==n_cycles[k]])
    
    
print(Stress_N)
#print(Strain_N)
#print(len(Stress_N))


# In[6]:


### Definition of polyarea and stiffness
def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))


# In[7]:


### Hysteresis loops area computation

Hysteresis_Area = []
Stiffness = []
creep = []


for j in range(len(n_cycles)):
    x=Stress_N[j]
    y=Strain_N[j]
    points=(x, y)
    Hysteresis_Area.append(PolyArea(x, y))
    if j > 0:
        slope, intercept, r_value, p_value, std_err = stats.linregress(y, x)
        Stiffness.append(slope)
    
    #Stress_min = np.min


hyst_df.hysteresis_area = Hysteresis_Area
#hyst_df.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_hystarea.csv', index=False)





# In[8]:


print(Stiffness)


# In[9]:


### Stiffness computation


Stress_max = [np.max(Stress_N[i]) for i in range(1,len(n_cycles))]
Stress_min = [np.min(Stress_N[i]) for i in range(1,len(n_cycles))]
Strain_max = [np.max(Strain_N[i]) for i in range(1,len(n_cycles))]
Strain_min = [np.min(Strain_N[i]) for i in range(1,len(n_cycles))]
#creep = []
                    
#Stiffness = []

for i in range(len(n_cycles)-1):
    #dStress = Stress_max[i]-Stress_min[i]
    #dStrain = Strain_max[i]-Strain_min[i]
    #Stiffness.append(dStress/dStrain)
    creep.append(Strain_min[i])
    

Stiffness.append(6331.946706)
creep.append(0.008375)
hyst_df.stiffness = Stiffness
hyst_df.creep = creep
#stif_df.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_stiff.csv', index=False)


# In[10]:


print(hyst_df)


# In[11]:


hyst_df.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_hysteresis_analysis.csv', index=False)


# In[ ]:





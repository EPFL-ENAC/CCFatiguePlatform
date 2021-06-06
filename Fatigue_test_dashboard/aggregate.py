# %% Environment setup
import pandas as pd
import numpy as np
from pathlib import Path
import glob
import os

path = '/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Data_tests/Aggregated_test'

os.chdir(path)

# get data file names
filenames = [i for i in glob.glob("*.txt")]
df = [pd.read_csv(file, sep = "\t", header=0, decimal=',') for file in filenames]

#setup df
cols = ['Stress_Ratio', 'Reliability_Level', 'Stress_Level_No', 'Stress_Parameter', 'Number_of_Cycles', 'Residual_Strength']
agg_stress = []
agg_n_cycles = []



#select max stress for all tests

for i in range(len(df)):
    stress = df[i].Stress
    max_stress = max(stress)
    agg_stress.append(max_stress)

    n_cycles = df[i].N
    max_n_cycles = max(n_cycles)
    agg_n_cycles.append(max_n_cycles)


# %%

print(stress)
print(agg_stress)
print(agg_n_cycles)


# %% declaring other parameters
R_ratio = 0.1
rel_level = 50
percent = 0.05
stressLev = []
stressLevNo = 1



# %% Stress level no

sortStress, sortNCycles = zip(*sorted(zip(agg_stress, agg_n_cycles), reverse = True))
maxValue = sortStress[0]
for i in range (0,len(sortStress)):
    if sortStress[i] >= (1 - percent) * maxValue:
        stressLev.append(stressLevNo)
    else:
        maxValue = sortStress[i]
        stressLevNo = stressLevNo + 1
        stressLev.append(stressLevNo)





# %% Checkpoint
print(stressLev)
print(len(stressLev))
print(len(sortStress))

# %% Constructing standard format for CCFatigue S-N curve

SNdf = pd.DataFrame(columns = cols)


SNdf.Stress_Ratio = R_ratio * np.ones(len(sortStress))
SNdf.Reliability_Level = rel_level * np.ones(len(sortStress))
SNdf.Stress_Level_No = stressLev * np.ones(len(sortStress))
SNdf.Stress_Parameter = sortStress
SNdf.Number_of_Cycles = sortNCycles
SNdf.Residual_Strength = sortStress

print(SNdf)

# %%
SNdf.to_csv(path_or_buf='/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Outputs/Vahid_std_CCFat.csv', index=False)

# %%

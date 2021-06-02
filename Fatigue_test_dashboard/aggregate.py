# %%
import pandas as pd
import numpy as np
from pathlib import Path
import glob
import os

os.chdir(r'/Users/scottmatthewssalmon/Desktop/github/CCFatigue/Data_tests/Aggregated_test')

# get data file names
filenames = [i for i in glob.glob("*.txt")]
df = [pd.read_csv(file, sep = "\t", header=0, decimal=',') for file in filenames]

#print(filenames)

columns = {'Stress Ratio', 'Reliability Level', 'Stress Level No.', 'Stress Parameter', 'Number of Cycles', 'Residual Strength'}
agg_stress = []
agg_n_cycles = []



#maxStress = max(df[j].Stress)

for i in range (0, 11):
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


# %%
R_ratio = 0.1
rel_level = 50




# %%

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


#maxStress = max(df[j].Stress)

for i in range (0, 14):
    stress = df[i].Stress
    max_stress = max.stress









# %%

print(max_stress)


# %%

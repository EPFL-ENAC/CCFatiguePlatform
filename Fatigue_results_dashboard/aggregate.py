# %% Environment setup
import pandas as pd
import numpy as np
from pathlib import Path
import glob
import os


### Importing all STD files
def read_df_list():
    data_directory = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example'

    data_type = 'STD'
    res = 'VAH'
    date = '210420'
    test_type = 'FA'
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'.txt'

    lab = "CCLab"
    researcher = 'Vahid'
    loading = 'Fatigue'


    filepath = os.path.join(data_directory, lab, researcher, loading, date, data_type)


    os.chdir(filepath)

    # get data file names
    filenames = [i for i in glob.glob("*.txt")]
    df = [pd.read_csv(file, sep = ",", header=0, decimal='.') for file in filenames]

    return df


### Importing meta data from csv
def read_met():
    data_directory = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example'

    data_type = 'MET'
    res = 'VAH'
    date = '210420'
    test_type = 'FA'
    test_number = '002'
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'_'+test_number+'.txt'

    lab = "CCLab"
    researcher = 'Vahid'
    loading = 'Fatigue'

    filepath = os.path.join(data_directory, lab, researcher, loading, date, data_type)


    os.chdir(filepath)

    # get data file names
    filenames = [i for i in glob.glob("*.txt")]
    meta_df = [pd.read_csv(file, sep = ",", header=0, decimal='.') for file in filenames]
    return meta_df


def write_agg(SNdf):
    data_directory = '/Volumes/GoogleDrive/.shortcut-targets-by-id/306/FatigueDataPlatform files & data/Data Description/File directory example'
    data_type = 'AGG'
    res = 'VAH'
    date = '210420'
    test_type = 'FA'
    filename = data_type+'_'+res+'_'+date+'_'+test_type+'.txt'

    lab = "CCLab"
    researcher = 'Vahid'
    loading = 'Fatigue'

    output_path = os.path.join(data_directory, lab, researcher, loading, date, data_type)
    filepath = os.path.join(output_path,filename)

    SNdf.to_csv(path_or_buf=filepath, index=False)
    return


#print(df)

#setup df
def stress_n_cycles(df, meta_df):
    agg_stress = []
    agg_n_cycles = []

    #select max stress for all tests

    for i in range(len(df)):
        stress = df[i].Machine_Load
        max_stress = max(stress)
        agg_stress.append(max_stress)

        n_cycles = meta_df[i].N_fail[0]
        #max_n_cycles = max(n_cycles)
        agg_n_cycles.append(n_cycles)

    return agg_stress, agg_n_cycles


# %%

#print(stress)
#print(agg_stress)
#print(agg_n_cycles)

def calculate_stress_lev(meta_df, agg_stress, agg_n_cycles):
# %% declaring other parameters
    #R_ratio = float(meta_df.R_Ratio[0])
    R_ratio = []
    rel_level = []
    for i in range(len(meta_df)):
        r_ratio = float(meta_df[i].R_Ratio[0])
        #rel_level = float(meta_df.Rel_level[0])
        r_level = float(meta_df[i].Rel_level[0])

        R_ratio.append(r_ratio)
        rel_level.append(r_level)


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
    return sortStress, sortNCycles, stressLev, R_ratio, rel_level





# %% Checkpoint
#print(stressLev)
#print(len(stressLev))
#print(len(sortStress))

# %% Constructing standard format for CCFatigue S-N curve


def create_agg(R_ratio, rel_level, stressLev, sortStress, sortNCycles):
    cols = ['Stress_Ratio', 'Reliability_Level', 'Stress_Level_No', 'Stress_Parameter', 'Number_of_Cycles', 'Residual_Strength']
    SNdf = pd.DataFrame(columns = cols)

    SNdf.Stress_Ratio = R_ratio
    SNdf.Reliability_Level = rel_level
    SNdf.Stress_Level_No = stressLev
    SNdf.Stress_Parameter = sortStress
    SNdf.Number_of_Cycles = sortNCycles
    SNdf.Residual_Strength = sortStress
    print(SNdf)
    return SNdf




def main ():
    df = read_df_list()
    meta_df = read_met()
    agg_stress, agg_n_cycles = stress_n_cycles(df, meta_df)
    sortStress, sortNCycles, stressLev, R_ratio, rel_level = calculate_stress_lev(meta_df, agg_stress, agg_n_cycles)
    SNdf = create_agg(R_ratio, rel_level, stressLev, sortStress, sortNCycles)
    write_agg(SNdf)
    return

if __name__ == "__main__":
    main()

# %

# %%

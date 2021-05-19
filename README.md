# CCFatigue Platform

CCFatiguePlatform is an initiative from CCLab that aims to develop a web application to faciltate manipulation and harmonized storage of composite materials testing datasets. 

# Data
## Standard data format
>>>> HERE NEEDS A LINK TO SAMPLE DATA IN STANDARD FORMAT.


>>>> HERE ADD METADATA SCHEME (FIGURE 1 WHEN FINAL)

|Directory naming convention|
|---------------------------|
|\Laboratory\Researcher\Test Type\Date\ *filename
|Laboratory: Full accronym (i.e. CCLab, RESSLab, IBeton,...)
|Researcher: Last name
|Test type: Standard Fatigue, Combined Fatigue Fracture, Standard Quasi-Static, Combined Quasi-Static Fracture
|Date: YYYY-MM-DD


|Filenaming conventions|
|-----------------------|
|{Researcher} _ {Date} _ {Test type} _ {status code} _ {###}|
|Researcher: 3 first letters of last name {i.e. VAS, WEI, MAT,...}
|Date: YYYYMMDD
|Test type: {FAT} = standard fatigue, {QST} = standard quasi-static, {FAF} = combined fatigue/fracture, {QSF} = combined quasi-static/fracture
|Status code: {RAW} = raw data, {TRE} = treated
|###: numerical identifier
|Example: SHA _ 20210420 _ FAF _ RAW _ 001

# Contents

### Standardizing data
Notebook Standarsizing_data.ipynb converts raw data from use cases to the standard format
Raw data format supported: SCOTT PUT SOME DETAILS HERE!
Usage (SCOTT: DO PEOPLE NEED TO USE THE NOTEBOOK, OR CAN THEY CALL DIRECTLY A PYTHON SCRIPT TO STANDARDIZE DATA?)

### Plots
Notebook Plotting.ipynb creates the following plots:
>>>> SCOTT LIST THE PLOTS (with inputs ideally)

### Modules
Using the notebooks in the modules folder, one can compute the right parameters for plotting i.e. the notebook 'Hysteresis loops.ipynb' uses stress/strain info to compute the TDE and evolution of stiffness


# Reproduce

environment.yml file --> SCOTT YOU NEED TO EXPORT YOUR ENVIRONMENT FILE AND PUT IT HERE!!

# License
TBD.

# Authors
Charlotte Weil, Scott M. Salmon, Samuel Bancal.

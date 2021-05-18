# CCFatigue Platform

CCFatigue platform is an initiative from CCLab that aims at facilitating manipulation and storage of new and pre-existing data. It is composed of two main sections, the database is where all the data is stored, whereas the modules section is used for manipulating data.

# Standard data format
|Directory naming convention|
|---------------------------|
|\Laboratory\Researcher\Test Type\Date\ *filename*
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
Use notebook Standarsizing_data.ipynb to convert raw data from use cases to the standard format


### Plotting
Use the notebook Plotting.ipynb to create plots using the outputs of various modules or raw data

### Modules
Using the notebooks in the modules folder, one can compute the right parameters for plotting i.e. the notebook 'Hysteresis loops.ipynb' uses stress/strain info to compute the TDE and evolution of stiffness



# Reproduce

environment.yml file 

# License

#Authors


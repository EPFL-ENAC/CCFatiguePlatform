# TST Standard Data format

### Structure

Each experiment is composed of several tests.
```
└───Experiment folder
	   │   Metadata file .xls
	   │   Test data 01 .csv
	   │   Test data 02 .csv
	   │   ...

```


### Directory/file naming conventions

* Experiment folder:  **TST _ {Researcher's lastname}_{Date} _ {Test type}** 
* Metadata file: **TST_{Date} _ {Test type}_metadata.xls** 
* Test data: **TST _ {Date} _ {Test type} _ {###}.csv** 

with:

* date: YYYY-MM
* Test type: {FA} = standard fatigue or {QS} = standard quasi-static.
* \###: Specimen number

	

### TST CSV files standards (column names must be exact) :


| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|---|
| Machine\_N_cycles     | Number of cycles counted by the machine            | [-]   | int       | y  |
| Machine_Load         | Load measured by the machine                       | [kN] | double    |  y |
| Machine_Displacement | Displacement measured by the machine               | [mm]   | double    |  y |
| index            | Image number                                       | [-]   | int       |   |
| Camera\_N_cycles         | Number of cycles counted by the camera             | [-]   | int       |   |
| exx              | Strain measured along main axis              | [-]  | double    |   |
| eyy              | Strain measured along secondary axis         | [-]  | double    |   |
| exy              | Strain measured along a specified axis       | [-]  | double    |   |
| crack_length     | Crack length measurement (for fracture testings)   | [mm]  | double    |   |
| Th_time              | Time as counted by temperature monitoring          | [sec] | int       |   |
| Th\_N_cycles          | Number of cycles counted by temperature monitoring | [-]   | int       |   |
| Th\_specimen_max      | Maximum temperature monitored                      | [°C]  | single    |   |
| Th\_specimen_mean     | Mean temperature                                   | [°C]  | single    |   |
| Th_chamber           | Temperature of the test environment                | [°C]  | single    |   |
| Th_uppergrips        | Temperature of the upper grips                     | [°C]  | single    |   |
| Th_lowergrips        | Temperature of the lower grips                     | [°C]  | single    |   |


# AGG Standard Data format

### File naming conventions

<!-- * Experiment folder:  `AGG_{Researcher's lastname}_{Date}_{Test type}`
* Metadata file: `AGG_{Date}_{Test type}_metadata.xls` -->
* File: `AGG_{Date}_{Test type}_{###}.csv`

with:

* date: `YYYY-MM` (starting month of the experiment)



### AGG CSV files standards (column names must be exact) :


| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|------------|
| Stress_ratio         | Stress ratio (R)                                   | [-]   | double    | y(?)       |
| Reliability_level    | (input parameter)                                  | ?     | int       | ?(?)       |
| Stress_level         | Stress_level no.                                   | ?     | int       | ?(?)       |
| Stress_parameter     | ?                                                  | [MPa] | int       | ?(?)       |
| Number_of_cycles     | ?                                                  | [-]   | int       | ?(?)       |
| Residual_strength    | ?                                                  | [MPa] | int       | ?(?)       |


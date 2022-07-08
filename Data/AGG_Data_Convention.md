# AGG Standard Data format

### File naming conventions

* File: `AGG_{Researcher's lastname?}_{Date?}_{Test type?}_{###?}.csv`

with:

* date: `YYYY-MM` (starting month of the experiment)



### AGG CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name          | Description                            | Symbol | Unit  | Data type | Mandatory     |
|----------------------|----------------------------------------|--------|-------|-----------|---------------|
| stress_ratio         | Stress ratio                           | R      | [-]   | double    | y             |
| reliability_level    | Reliability level                      | P(N)   | %     | double    | y             |
| stress_level         | Index of the cluster of stress level   |        | [-]   | int       | y for withney |
| stress_parameter     | Cyclic max stress                      | sigma_max | [MPa] | double    | y             |
| number_of_cycles     | Number of cycles at failure         | N      | [-]   | double    | y             |
| residual_strength    | Residual strength                      | sigma_r | [MPa] | double    | y for Sendeckyj |


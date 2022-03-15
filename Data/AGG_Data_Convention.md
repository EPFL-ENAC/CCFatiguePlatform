# AGG Standard Data format

### File naming conventions

* File: `AGG_{Researcher's lastname?}_{Date?}_{Test type?}_{###?}.csv`

with:

* date: `YYYY-MM` (starting month of the experiment)



### AGG CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|------------|
| Stress_ratio         | Stress ratio (R)                                   | [-]   | double    | y       |
| Reliability_level    | Reliability_level P(N)                             | %     | double    | y       |
| Stress_level         | Index of the cluster of stress level               | [-]   | int       | y for withney      |
| Stress_parameter     | Cyclic max stress Sigma_max                        | [MPa] | double       | y       |
| Number_of_cycles     | Number of cycles at failure N                       | [-]   | double      | y       |
| Residual_strength    | Residual_strength   Sigma_e                        | [MPa] | double       | y for Sendeckyj  |


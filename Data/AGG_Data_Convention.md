# AGG Standard Data format

### File naming conventions

* File: `AGG_{Researcher's lastname?}_{Date?}_{Test type?}_{###?}.csv`

with:

* date: `YYYY-MM` (starting month of the experiment)



### AGG CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)


| Variable name         | Description                            | Symbol    | Unit    | Data type | Mandatory          |
|-----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| stress_ratio          | Stress ratio                           | R         | [-]     | double    | y                  |
| reliability_level     | Reliability level                      | rsql      | %       | double    | y                  |
| stress_cluster_number | Index of the cluster of stress level for Whitney method |  | [-] | int   | y for Whitney ?    |
| stress_max            | Max stress                             | sigma_max | [MPa]   | double    | y                  |
| cycles_to_failure     | Number of cycles to failure            | N         | [-]     | int       | y                  |
| residual_strength     | Residual strength                      | sigma_r   | [MPa]   | double    | y for Sendeckyj    |

# AGG Standard Data format

AGG Data constitutes input of S-N Curve module, together with

- ASTM.csv, available from https://www.astm.org/stp313-eb.html pp 20-23
- Reliability level parameter (RELIABILITY_LEVEL P(N) with value \[0,1\], default value : 0.5)

### File naming conventions

- File: `AGG_{Researcher's lastname}_{Date}_{Test type}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

### AGG CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name         | Description                                             | Symbol    | Unit    | Data type | Mandatory       |
| --------------------- | ------------------------------------------------------- | --------- | ------- | --------- | --------------- |
| stress_ratio          | Stress ratio wijer                                      | R         | `[-]`   | double    | y               |
| stress_cluster_number | Index of the cluster of stress level for Whitney method |           | `[-]`   | int       | y               |
| stress_max            | Max stress                                              | sigma_max | `[MPa]` | double    | y               |
| cycles_to_failure     | Number of cycles to failure                             | N         | `[-]`   | int       | y               |
| residual_strength     | Residual strength                                       | sigma_r   | `[MPa]` | double    | y for Sendeckyj |

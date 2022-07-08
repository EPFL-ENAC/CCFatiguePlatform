# FAF Standard Data format

## File naming conventions

- Files: `FAF_{Date?}_{Test type}_{###?}.json`
- Files: `FAF_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## FAF CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name       | Description                    | Unit  | Data type | Mandatory |
| ----------------- | ------------------------------ | ----- | --------- | --------- |
| stress_ratio      | Stress ratio: R                | [-]   | double    | y         |
| cycles_to_failure | Number of cycles to failure: N | [-]   | int       | y         |
| stress_parameter  | Cyclic max stress: sigma_max   | [MPa] | double    | y         |

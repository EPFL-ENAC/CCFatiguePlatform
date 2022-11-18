# DAS Standard Data format

Note: Currently module 5 re-runs module 3 inside module 5. Needs to be updated.
DAS Data corresponds to the output of Damage summation module.

## File naming conventions

- Files: `DAS_{Researcher's lastname}_{Date}.json`
- Files: `DAS_{Researcher's lastname}_{Date}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## DAS CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name | Description | Symbol    | Unit    | Data type | Mandatory |
| ------------- | ----------- | --------- | ------- | --------- | --------- |
| stress_max    | Max stress  | sigma_max | `[MPa]` | double    | y         |
| damage        | Damage      | D         | `[-]`   | double    | y         |

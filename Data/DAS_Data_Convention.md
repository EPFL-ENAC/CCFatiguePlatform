# DAS Standard Data format

Note: Currently module 5 re-runs module 3 inside module 5. Needs to be updated.
DAS Data corresponds to the output of Damage summation module.

## File naming conventions

- Files: `DAS_{Researcher's lastname}_{date}[_{methods}].json`
- Files: `DAS_{Researcher's lastname}_{date}[_{methods}].csv`

with:

- date: `YYYY-MM` (starting month of the experiment)
- methods (optional): lists the methods used successively in the different modules

## DAS CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name | Description | Symbol    | Unit    | Data type | Mandatory |
| ------------- | ----------- | --------- | ------- | --------- | --------- |
| stress_max    | Max stress  | sigma_max | `[MPa]` | double    | y         |
| damage        | Damage      | D         | `[-]`   | double    | y         |

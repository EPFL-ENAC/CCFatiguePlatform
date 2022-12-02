# CLD Standard Data format

Note: in all the Fortran codes, the output range of mean stress needs to be checked and tuned!
CLD Data is the output of the Constant-Life Diagram module.

## File naming conventions

- Files: `CLD_{Researcher's lastname}_{date}[_{methods}].json`
- Files: `CLD_{Researcher's lastname}_{date}[_{methods}].csv`

with:

- date: `YYYY-MM` (starting month of the experiment)
- methods (optional): lists the methods used successively in the different modules

## CLD CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name     | Description                 | Symbol     | Unit    | Data type | Mandatory |
| ----------------- | --------------------------- | ---------- | ------- | --------- | --------- |
| cycles_to_failure | Number of cycles to failure | N          | `[-]`   | int       | y         |
| stress_amplitude  | Stress amplitude            | sigma_a    | `[MPa]` | double    | y         |
| stress_mean       | Mean stress                 | sigma_mean | `[MPa]` | double    | y         |

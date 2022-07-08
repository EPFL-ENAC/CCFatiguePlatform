# CLD Standard Data format

Note: in all the Fortran codes, the output range of mean stress needs to be checked and tuned!

## File naming conventions

- Files: `CLD_{Date?}_{Test type}_{###?}.json`
- Files: `CLD_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## CLD CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)


| Variable name        | Description                            | Symbol    | Unit    | Data type | Mandatory          |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| cycles_to_failure    | Number of cycles to failure            | N         | [-]     | int       | y                |
| stress_amplitude     | Stress amplitude                       | sigma_a   | [MPa]   | double    | y                |
| mean_stress          | Mean stress                            | ?         | [MPa]   | double    | y                |

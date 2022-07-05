# DAS Standard Data format

Note: in all the Fortran codes, the output range of mean stress needs to be checked and tuned!

## File naming conventions

- Files: `DAS_{Date?}_{Test type}_{###?}.json`
- Files: `DAS_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## DAS CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name        | Description                 | Unit    | Data type | Mandatory |
| ------------------ | --------------------------- | ------- | --------- | --------- |
| max_stress_level ? | ?                           | [MPa] ? | double    | y         |
| damage ?           | ?                           | [-] ?   | double    | y         |

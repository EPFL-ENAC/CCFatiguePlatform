# DAS Standard Data format

## File naming conventions

- Files: `DAS_{Date?}_{Test type}_{###?}.json`
- Files: `DAS_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## DAS CSV files standards (column names must be exact) :

One line per factor.

Encoding format: UTF-8, Separator: ',' (comma)

| Column name        | Description                 | Unit    | Data type | Mandatory |
| ------------------ | --------------------------- | ------- | --------- | --------- |
| max_stress_level ? | ?                           | [MPa] ? | double    | y         |
| damage ?           | ?                           | [-] ?   | double    | y         |

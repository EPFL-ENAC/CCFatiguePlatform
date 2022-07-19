# DAS Standard Data format

Note: Currently module 5 re-runs module 3 inside module 5. Needs to be updated.

## File naming conventions

- Files: `DAS_{Date?}_{Test type}_{###?}.json`
- Files: `DAS_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## Module 5 inputs

CCInput.txt >
Factor.txt > bounds for plot

Output : 2 columns

## CLD CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name        | Description                            | Symbol    | Unit    | Data type | Mandatory          |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| max_stress           | Max stress                             | sigma_max | [MPa]   | double    | y                  |
| damage               | 1/Damage                               | D         | [-]     | double    | y                  |

# CYC Standard Data format

### File naming conventions

- File: `CYC_{Researcher's lastname?}_{Date?}_{Test type?}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

### CYC CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)


| Variable name        | Description                            | Symbol      | Unit    | Data type | Mandatory          |
|----------------------|----------------------------------------|-------------|---------|-----------|--------------------|
| stress_range         | Stress range (= 2 * sigma_a)           | delta_sigma | [MPa]   | double    | y                  |
| stress_mean          | ?                                      | sigma_mean  |         | double    | y                  |
| stress_ratio         | Stress ratio                           | R           | [-]     | double    | y                  |
| n_cycles?            | ?                                      | ?           |         | double    | y                  |
| cum_n_cycles?        | percentage?                            | ?           |         | double    | y                  |

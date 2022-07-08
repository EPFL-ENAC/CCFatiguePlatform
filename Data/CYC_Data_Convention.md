# CYC Standard Data format

### File naming conventions

- File: `CYC_{Researcher's lastname?}_{Date?}_{Test type?}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

### CYC CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name  | Description           | Unit | Data type | Mandatory |
| ------------ | --------------------- | ---- | --------- | --------- |
| stress_range | sigma_max - sigma_min | ?    | double    | y         |
| stress_mean  | (sigma_m)             | ?    | double    | y         |
| R_ratio      | ?                     | ?    | double    | y         |
| N_cycles     | ?                     | ?    | double    | y         |
| Cum_N_cycles | ?                     | ?    | double    | y         |

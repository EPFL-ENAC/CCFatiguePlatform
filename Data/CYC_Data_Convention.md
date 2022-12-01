# CYC Standard Data format

CYC Data is the output of the Cycle Counting module.

### File naming conventions

- File: `CYC_{Researcher's lastname}_{Date}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

### CYC CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name | Description                                                               | Symbol      | Unit    | Data type | Mandatory |
| ------------- | ------------------------------------------------------------------------- | ----------- | ------- | --------- | --------- |
| stress_range  | Stress range (= `2 * sigma_a`)                                            | delta_sigma | `[MPa]` | double    | y         |
| stress_mean   | Mean stress                                                               | sigma_mean  | `[MPa]` | double    | y         |
| stress_ratio  | Stress ratio                                                              | R           | `[-]`   | double    | y         |
| n_cycles      | Number of cycles                                                          | n           |         | double    | y         |
| cum_n_cycles  | cumulative number of cycles for a given bin of sigma_a, sigma_mean and R. |             |         | double    | y         |

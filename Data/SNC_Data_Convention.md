# SNC Standard Data format

SNC Data corresponds to the output of S-N Curve module and is composed of 2 files :

- one json file containing the different R-ratio with some parameters reserved for the software
- one csv file with data

## File naming conventions

- Files: `SNC_{Researcher's lastname}_{date}[_{axis}][_{methods}].json`
- Files: `SNC_{Researcher's lastname}_{date}[_{axis}][_{methods}].csv`

with:

- date: `YYYY-MM` (starting month of the experiment)
- axis: to be defined, seems to depends on the method used, for instance for TFTP:
  - X = longitudinal
  - Y = transverse
  - F = under shear loading
- methods (optional): lists the methods used successively in the different modules

## SNC JSON files standards :

Encoding format: UTF-8

```javascript
[
	{
		"stress_ratio": double, // Stress ratio (R) [-]
		"confidence_interval": double, // confidence bounds, usually 5, 95% (rsql) [-]
		"a": double, // S-N Curve parameter [-]
		"b": double, // S-N Curve parameter [1/MPa]
		"lrsq": double, // [?], optional
		"fp": double, // Fp Linearity criterion [?], optional
	},
	...
]
```

## SNC CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name     | Description                     | Symbol    | Unit    | Data type | Mandatory |
| ----------------- | ------------------------------- | --------- | ------- | --------- | --------- |
| stress_ratio      | Stress ratio                    | R         | `[-]`   | double    | y         |
| cycles_to_failure | Number of cycles to failure     | N         | `[-]`   | int       | y         |
| stress_max        | Max stress                      | sigma_max | `[MPa]` | double    | y         |
| stress_lowerbound | Stress at failure (lower bound) | sigma_max | `[MPa]` | double    |           |
| stress_upperbound | Stress at failure (upper bound) | sigma_max | `[MPa]` | double    |           |

### SNC example file

- [SNC_sample_2022-08.json](/Data/samples/SSNC_sample_2022-08.json)
- [SNC_sample_2022-08.csv](/Data/samples/SNC_sample_2022-08.csv)

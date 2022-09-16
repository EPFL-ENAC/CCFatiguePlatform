# SNC Standard Data format
SNC Data corresponds to the output of S-N Curve module and is composed of 2 files :

- one json file containing the different R-ratio with some parameters reserved for the software
- one csv file with data

## File naming conventions

- Files: `SNC_{Researcher's lastname}_{Date}.json`
- Files: `SNC_{Researcher's lastname}_{Date}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

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

| Variable name     | Description                     | Symbol    | Unit  | Data type | Mandatory |
| ----------------- | ------------------------------- | --------- | ----- | --------- | --------- |
| stress_ratio      | Stress ratio                    | R         | [-]   | double    | y         |
| cycles_to_failure | Number of cycles to failure     | N         | [-]   | int       | y         |
| stress_max        | Max stress                      | sigma_max | [MPa] | double    | y         |
| stress_lowerbound | Stress at failure (lower bound) | sigma_max | [MPa] | double    |           |
| stress_upperbound | Stress at failure (upper bound) | sigma_max | [MPa] | double    |           |

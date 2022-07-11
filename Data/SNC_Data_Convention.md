# SNC Standard Data format

Each module 2 (S-N curve) produce two files:
* one json file containing the different R-ratio with some parameters reserved for the software
* one csv file with data

## File naming conventions

* Files: `SNC_{Date?}_{Test type}_{###?}.json`
* Files: `SNC_{Date?}_{Test type}_{###?}.csv`

with:

* date: `YYYY-MM` (starting month of the experiment)



## SNC JSON files standards :

Encoding format: UTF-8

```javascript
[
	{
		"stress_ratio": double, // Stress ratio (R) [-]
		"reliability_level": int, // Reliability level (rsql) [-]
		"a": double, // S-N Curve parameter [-]
		"b": double, // S-N Curve parameter [1/MPa]
		"lrsq": double, // [?]
		"fp": double, // Fp Linearity criterion [?]
	},
	...
]
```

## SNC CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)


| Variable name        | Description                            | Symbol    | Unit    | Data type | Mandatory          |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| stress_ratio         | Stress ratio                           | R         | [-]     | double    | y                  |
| cycles_to_failure    | Number of cycles to failure            | N         | [-]     | int       | y                  |
| stress               | Stress at failure (median)             | sigma_max | [MPa]   | double    | y                  |
| stress_lowerbound    | Stress at failure (lower bound)        | sigma_max | [MPa]   | double    |                    |
| stress_upperbound    | Stress at failure (upper bound)        | sigma_max | [MPa]   | double    |                    |

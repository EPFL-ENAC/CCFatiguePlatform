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

```json
[
	{
		"Stress-ratio": double, // R-Ratio (R) [-]
		"RSQL": int, // Reliability level [-]
		"A": double, // S-N Curve parameter [-]
		"B": double, // S-N Curve parameter [1/MPa]
		"LRSQ": double, // [?]
		"Fp": double, // Fp Linearity criterion [?]
		"Linearity": double, // Linearity index [?]
		"RMSE": double, // Root mean square error [?]
		"SSE": double, // Sum of squares due to errors [?]
		"SST": double, // Sum of squares about the mean [?]
		"RSQ": double // R-square [?]
	},
	...
]
```

## SNC CSV files standards (column names must be exact) :


| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|------------|
| Stress_ratio         | Stress ratio (R)                                   | [-]   | double    | y(?)       |
| Cycles_to_failure    | Number of cycles to failure | (input parameter)    | [-]   | int       | ?(?)       |
| Stress_parameter     | Stress at failure                                  | [MPa] | double    | ?(?)       |
| ?                    | ?                                                  | ?     | double    | ?(?)       |
| ?                    | ?                                                  | ?     | double    | ?(?)       |


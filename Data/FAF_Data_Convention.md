# FAF Standard Data format

## File naming conventions

- Files: `FAF_{Date?}_{Test type}_{###?}.json`
- Files: `FAF_{Date?}_{Test type}_{###?}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## SNC JSON files standards :

Encoding format: UTF-8

```javascript
[
	{
		"stress-ratio": double, // Stress ratio (R) [-]
		"rsql": int, // Reliability level (?) [-]
		"a1": double, // S-N Curve parameter (-) [-]
		"b1": double, // S-N Curve parameter (-) [1/MPa]
		"a2": double, // (optional) S-N Curve parameter (-) [-]
		"b2": double, // (optional)S-N Curve parameter (-) [1/MPa]
        "off_axis_angle1": double, // (optional) ? (-) [°]
        "off_axis_angle2": double, // (optional) [°]
	},
	...
]
```

## FAF CSV files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)


| Variable name        | Description                            | Symbol    | Unit    | Data type | Mandatory          |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| stress_ratio         | Stress ratio                           | R         | [-]     | double    | y                  |
| cycles_to_failure    | Number of cycles to failure            | N         | [-]     | int       | y                  |
| stress_max           | Max stress                             | sigma_max | [MPa]   | double    | y                  |

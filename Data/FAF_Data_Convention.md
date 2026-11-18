# FAF Standard Data format

FAF Data corresponds to the output of Fatigue failure module.

## File naming conventions

- Files: `FAF_{Researcher's lastname}_{date}[_{methods}].json`
- Files: `FAF_{Researcher's lastname}_{date}[_{methods}].csv`

with:

- date: `YYYY-MM` (starting month of the experiment)
- methods (optional): lists the methods used successively in the different modules

## FAF JSON file standard

Encoding format: UTF-8

```javascript
[
	{
		"stress-ratio": double, // Stress ratio (R) [-]
		"confidence_interval": double, // Confidence bounds, usually 5, 95% (rsql) [-]
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

| Variable name     | Description                 | Symbol    | Unit    | Data type | Mandatory |
| ----------------- | --------------------------- | --------- | ------- | --------- | --------- |
| stress_ratio      | Stress ratio                | R         | `[-]`   | double    | y         |
| cycles_to_failure | Number of cycles to failure | N         | `[-]`   | int       | y         |
| stress_max        | Max stress                  | sigma_max | `[MPa]` | double    | y         |

## Kawai master-curve JSON

The Kawai method fits one analytical master curve to all valid AGG rows. The
rows must describe fatigue tests at one common reference angle, but they may
use different stress ratios. Each row is normalized using its own stress ratio
and the orientation factor calculated at the reference angle.

There is no point-to-point interpolation of the master curve. It is evaluated
directly as:

```text
Sigma(N) = A * N**(-1/nstar)
```

Here `Sigma` is the dimensionless Kawai normalized stress, `N` is
`cycles_to_failure`, and `A` (also called `Sigma0`) is stored as `a`. The
`nstar` field is the positive fatigue exponent. The implementation obtains both
parameters from a free-intercept least-squares regression of `log10(2*N)`
against `log10(Sigma)`.

The target physical stress is calculated from the analytical value of `Sigma`
using the requested stress ratio and the target-angle orientation factor
`omega`.

Example:

```json
[
  {
    "reference_angle": 15.0,
    "reference_stress_ratio": 0.1,
    "target_stress_ratio": 0.5,
    "desirable_angle": 45.0,
    "a": 0.917,
    "nstar": 8.0,
    "omega": 0.0182
  }
]
```

The pair `a` and `nstar` completely defines the normalized master curve. The
other fields document the transformation used to produce the physical-stress
CSV output.

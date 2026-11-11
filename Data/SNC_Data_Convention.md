# SNC Standard Data format

SNC Data corresponds to the output of the S-N Curve module and is composed of
two complementary files:

- one JSON file containing the fitted curves in the canonical `SN_curves`
  representation;
- one CSV file containing sampled points for plotting and tabular interchange.

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

## SNC JSON file standard

Encoding format: UTF-8. The document is a partial material document and can be
combined with a `static_strengths` array when preparing a complete material
file. Every curve generated from one AGG file has the loading type and angle
selected in the S-N tab.

```json
{
  "SN_curves": [
    {
      "type": "tensile",
      "angle": 0.0,
      "stress_ratio": 0.1,
      "model": "Log-Log",
      "a": 1740.0,
      "b": 0.0786,
      "confidence_interval": 50,
      "lrsq": 0.98,
      "fp": 1.2
    }
  ]
}
```

`type` is `tensile`, `compressive`, or `shear`. `angle` is mandatory for
tensile and compressive curves and is omitted for shear curves.

For `Lin-Log`, the fitted law is `stress_max = a + b log10(N)`. For
`Log-Log`, it is `stress_max = a N^(-b)`. Sendeckyj and Whitney use the same
curve metadata but retain their method-specific parameters (`sstar`/`cstar`
and `alpha_f`/`q0`/`sigma0`/`power`, respectively).

## SNC CSV file standard (column names must be exact)

Encoding format: UTF-8, Separator: ',' (comma). The CSV is a sampled,
self-describing projection of the fitted curves in `SN_curves`. Curve metadata
is repeated for every sampled point so the CSV remains meaningful when used
without its companion JSON file.

| Variable name      | Description                              | Symbol    | Unit    | Data type | Mandatory |
| ------------------ | ---------------------------------------- | --------- | ------- | --------- | --------- |
| type               | Loading type                             |           | `[-]`   | string    | y         |
| angle              | Material angle; empty for shear          |           | `[deg]` | double    | conditional |
| stress_ratio       | Stress ratio                             | R         | `[-]`   | double    | y         |
| model              | Fitted S-N model                         |           | `[-]`   | string    | y         |
| confidence_interval | Reliability/survival probability        |           | `[%]`   | double    | y         |
| cycles_to_failure  | Number of cycles to failure              | N         | `[-]`   | int       | y         |
| stress_max         | Max stress                               | sigma_max | `[MPa]` | double    | y         |
| stress_lowerbound  | Stress at failure (lower bound)          | sigma_max | `[MPa]` | double    |           |
| stress_upperbound  | Stress at failure (upper bound)          | sigma_max | `[MPa]` | double    |           |

`type`, `angle`, `model`, and `confidence_interval` must equal the values of
the `SN_curves` object from which the row was sampled. The model coefficients
remain exclusive to JSON and are not repeated in CSV. An SNC file used by CLD
must contain only one `(type, angle)` material direction.

Legacy SNC CSV files containing only the former sampled-data columns remain
accepted as input. Newly generated CSV files use the enriched format above.

`confidence_interval` is the probability used to select the central S-N curve.
It is distinct from the confidence level used to calculate optional
`stress_lowerbound` and `stress_upperbound` values.

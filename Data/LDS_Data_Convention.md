# LDS Standard Data format

Load spectrum data corresponds to machine loads - it's a serie of measured "Peaks and valleys" : extremum of stress.

## File naming convention

- Files: `LDS_{Researcher's lastname}_{Date}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)

## LDS csv series

Encoding format: UTF-8, Separator: ',' (comma)

| Variable name | Description | Symbol    | Unit  | Data type | Mandatory |
| ------------- | ----------- | --------- | ----- | --------- | --------- |
| stress_max    | Max stress  | sigma_max | [MPa] | double    | y         |

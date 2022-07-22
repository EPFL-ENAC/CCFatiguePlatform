# TST Standard Data format

## Structure

Each experiment is composed of several tests.

```
raw
└───Experiment folder
	   │   Metadata file .xls
	   │   Test data 001 .csv
	   │   Test data 002 .csv
	   │   ...
```

## Directory/file naming conventions

- Experiment folder: `TST_{Researcher's lastname}_{Date}_{Test type}`
- Metadata file: `TST_{Date}_{Test type}_metadata.xls`
- Test data: `TST_{Date}_{Test type}_{###}.csv`

with:

- date: `YYYY-MM` (starting month of the experiment)
- Test type:
  - `FA` = standard fatigue
  - `QS` = standard quasi-static
  - `TM` = DMA or temperature
- `###`: Specimen number

## TST CSV files standards (column names must be exact) :

| Column name            | Description                                           | Unit     | Data type | Mandatory | Multiple measurement points |
| ---------------------- | ----------------------------------------------------- | -------- | --------- | --------- | --------------------------- |
| `Machine_Time`         | Absolute or relative time recorded by the machine     | [-]      | int       |           |                             |
| `Machine_N_cycles`     | Number of cycles counted by the machine               | [-]      | int       |           |                             |
| `Machine_Displacement` | Displacement measured by the machine                  | [mm]     | double    |           |                             |
| `Machine_Load`         | Load measured by the machine                          | [kN]/[N] | double    |           |                             |
| `MD_index--#`          | index for the measurement, e.g. image number          | [-]      | int       |           | y                           |
| `MD_N_cycles--#`       | Number of cycles from measuring device                | [-]      | int       |           | y                           |
| `MD_Displacement--#`   | Displacement from measuring device                    | [mm]     | double    |           | y                           |
| `MD_Load--#`           | Load from measuring device                            | [kN]/[N] | double    |           | y                           |
| `u--#`                 | Displacement measured along main axis at point #      | [mm]     | double    |           | y                           |
| `v--#`                 | Displacement measured along secondary axis at point # | [mm]     | double    |           | y                           |
| `exx--#`               | Strain measured along main axis at point #            | [-]      | double    |           | y                           |
| `eyy--#`               | Strain measured along secondary axis at point #       | [-]      | double    |           | y                           |
| `exy--#`               | Strain measured along a specified axis at point #     | [-]      | double    |           | y                           |
| `Crack_length`         | Crack length measurement (for fracture testings)      | [mm]     | double    |           |                             |
| `Crack_N_cycles`       | Number of cycles at measured crack length             | [mm]     | double    |           |                             |
| `Crack_Displacement`   | Displacement at measured crack length                 | [mm]     | double    |           |                             |
| `Crack_Load`           | Load at measured crack length                         | [kN]/[N] | double    |           |                             |
| `Th_time`              | Time as counted by temperature monitoring             | [sec]    | int       |           |                             |
| `Th_N_cycles`          | Number of cycles counted by temperature monitoring    | [-]      | int       |           |                             |
| `Th_specimen_max`      | Maximum temperature monitored                         | [°C]     | double    |           |                             |
| `Th_specimen_mean`     | Mean temperature                                      | [°C]     | double    |           |                             |
| `Th_chamber`           | Temperature of the test environment                   | [°C]     | double    |           |                             |
| `Th_uppergrips`        | Temperature of the upper grips                        | [°C]     | double    |           |                             |
| `Th_lowergrips`        | Temperature of the lower grips                        | [°C]     | double    |           |                             |
| `T--#`                 | Temperature at point #                                | [°C]     | double    |           | y                           |
| `Storage_modulus`      | Storage modulus measured from DMA                     | [GPa]    | double    |           |                             |
| `Tan_delta`            | Tan delta measured from DMA                           | [-]      | double    |           |                             |
| `Specimen_name`        | Name of the specimen                                  | [-]      | string    |           |                             |

### `FA` without fracture specifities :

Mandatory data columns

- one of :
  - `Machine_N_cycles`
  - `MD_N_cycles--#`
- one of :
  - `Machine_Displacement`
  - `MD_Displacement--#`
  - `exx--#`
- one of :
  - `Machine_Load`
  - `MD_Load--#`

### `FA` with fracture specifities :

Mandatory data columns

- `Crack_N_cycles`
- `Crack_length`

### `QS` without fracture specifities :

Mandatory data columns

- one of :
  - `Machine_Displacement`
  - `MD_Displacement--#`
  - `exx--#`
- one of :
  - `Machine_Load`
  - `MD_Load--#`

### `QS` with fracture specifities :

Mandatory data columns

- one of :
  - `Machine_Displacement`
  - `MD_Displacement--#`
  - `Crack_length`
  - `Crack_Displacement`
- one of :
  - `Machine_Load`
  - `MD_Load--#`
  - `Crack_Load`

### `TM` with and without fracture specifities:

Mandatory data columns

- `T--#`
- one of :
  - `Storage_modulus`
  - `Tan_delta`
  - `Machine_Load`
  - `MD_Load--#`

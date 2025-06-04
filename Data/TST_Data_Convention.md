# TST Standard Data format

## Structure

Each experiment is composed of several tests.
When sent to Dataset checker, the ZIP file must match the following structure:

```
Experiment folder
│   Metadata file .xls
│   Test data 001 .csv
│   Test data 002 .csv
│   ...
```

When added to the project, it is unzipped under Data/raw/ folder.

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

Example:

```
TST_Doe_2020-01_FA
│   TST_2020-01_FA_metadata.xls
│   TST_2020-01_FA_001.csv
│   TST_2020-01_FA_002.csv
│   ...
```

## TST XLS file standards

A ready to use metadata file is inside each downloadable folder in the Data Upload page. 

## TST CSV files standards (column names must be exact, where # is replaced by a number) :

A dedicated guide about CSV columns is inside each downloadable folder in the Data Upload page. 

Each experiment type have different mandatory columns so make sure to download the right folder.
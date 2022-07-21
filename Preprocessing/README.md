# Preprocessing files received from researchers

## Setup

```bash
make install
```

## Run the preprocessing

```bash
make run
```

# Preprocessing Pipeline

- Source is `/Data/raw`
  - Folder is named `TST_ResearcherName_YYY-MM_Type` e.g. `TST_Srinivasan_2021-10_QS`
  - contains following files
    - `TST_YYYY-MM_Type_metadata.xls` e.g. `TST_2021-10_QS_metadata.xls` (could also be `.xlsx`)
    - `TST_YYYY-MM_Type_###.csv` e.g. `TST_2021-10_QS_001.csv`, `TST_2021-10_QS_002.csv`, ...
- Processing `tst_preprocessing.py` does
  - Parse the raw files
  - cleanup what can be done automatically
    - whitespace + unexpected return characters in columns
    - remove unexpected columns
  - Validation
    - Warning messages when not conforming to convention (non-blocking)
    - Error messages when not conforming to convention (blocking)
  - Write to destination if no error occurred
- Destination is `Data/preprocessed`
  - Folder is named `TST_ResearcherName_YYY-MM_Type` e.g. `TST_Srinivasan_2021-10_QS`
  - contains following files
    - `experiment.json` (produced from Experiment tab of raw `xls` file)
    - `tests.json` (produced from Tests tab of raw `xls` file)
    - `measures_###.csv` (produced from raw `csv` files)

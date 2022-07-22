"""
According to ./README.md

- Source is `/Data/raw`
  - Folder is named `TST_ResearcherName_YYY-MM_Type` e.g. `TST_Srinivasan_2021-10_QS`
  - contains following files
    - `TST_YYYY-MM_Type_metadata.xls`
      e.g. `TST_2021-10_QS_metadata.xls` (could also be `.xlsx`)
    - `TST_YYYY-MM_Type_###.csv`
      e.g. `TST_2021-10_QS_001.csv`, `TST_2021-10_QS_002.csv`, ...
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
    - `Experiment.json` (produced from Experiment tab of raw `xls` file)
    - `Tests.json` (produced from Tests tab of raw `xls` file)
    - `measures_###.csv` (produced from raw `csv` files)
"""

from tst_data import Logger, Experiment, NotAnExperimentFolder, get_tst_folders_to_parse

if __name__ == "__main__":
    with Logger(None) as logger:
        for experiment_raw_fp_folder in get_tst_folders_to_parse(
            description="CCFatigue TST Preprocessing"
        ):

            logger.reset_counts()
            logger.write()
            logger.write("-" * 10 + " " + "=" * 10 + " " + "-" * 10)
            logger.write()
            logger.info(f"Parsing experiment {experiment_raw_fp_folder}")
            with logger.indent:
                try:
                    experiment = Experiment(experiment_raw_fp_folder, logger)
                    # experiment.measures_list[0]
                    if logger.error_count == 0:
                        experiment.save_preprocessed_data()
                    else:
                        logger.warning(
                            "Experiment encountered errors -> "
                            "cannot save any preprocessed data"
                        )
                except NotAnExperimentFolder:
                    logger.error("This is not recognized as an Experiment folder")

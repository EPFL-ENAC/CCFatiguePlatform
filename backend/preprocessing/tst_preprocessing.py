"""
- Source is `/Data/raw`
  see [/Data/TST_Data_Convention.md](/Data/TST_Data_Convention.md)
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
  - Write to destination if no error occurred and `--save` cli argument is given
  - return a exit code (0=OK 1=Data error found 2=Other error)
- Destination is `Data/preprocessed`
  - Folder is named `TST_ResearcherName_YYY-MM_Type` e.g. `TST_Srinivasan_2021-10_QS`
  - contains following files
    - `Experiment.json` (produced from Experiment tab of raw `xls` file)
    - `Tests.json` (produced from Tests tab of raw `xls` file)
    - `measures_###.csv` (produced from raw `csv` files)
"""

import sys
import argparse
from tst_data_lib import (
    Logger,
    Experiment,
    NotAnExperimentFolder,
    EXPERIMENTS_FOLDER,
    RAW_EXPERIMENT_FP_FOLDERS,
)


def get_cli_options():
    """
    Read arguments. If tst_folders are given, then return it.
    Otherwise return default RAW_EXPERIMENT_FP_FOLDERS
    """
    parser = argparse.ArgumentParser(description="CCFatigue TST Preprocessing")
    parser.add_argument(
        "--save",
        help="will save preprocessed data (if there is no error)",
        action="store_true",
    )
    parser.add_argument(
        "tst_folders",
        nargs="*",
        help=f"Folder containing the TST dataset. "
        "if none provided, then will parse all dataset "
        f"under {EXPERIMENTS_FOLDER}",
    )
    args = parser.parse_args()
    if args.tst_folders != []:
        return {
            "tst_folders": args.tst_folders,
            "save": args.save,
        }
    else:
        return {
            "tst_folders": RAW_EXPERIMENT_FP_FOLDERS,
            "save": args.save,
        }


if __name__ == "__main__":
    exit_code = 0
    # 0=OK
    # 1=Data error found
    # 2=Other error
    with Logger(None) as logger:
        cli_options = get_cli_options()
        for experiment_raw_fp_folder in cli_options["tst_folders"]:

            logger.reset_counts()
            logger.write()
            logger.write("-" * 10 + " " + "=" * 10 + " " + "-" * 10)
            logger.write()
            logger.info(f"Parsing experiment {experiment_raw_fp_folder}")
            with logger.indent:
                try:
                    experiment = Experiment(experiment_raw_fp_folder, logger)
                    if logger.error_count == 0:
                        if cli_options["save"]:
                            experiment.save_preprocessed_data()
                    else:
                        logger.warning(
                            "Experiment encountered errors -> "
                            "cannot save any preprocessed data"
                        )
                        exit_code = 1
                except NotAnExperimentFolder:
                    logger.error("This is not recognized as an Experiment folder")
                    exit_code = 2
    sys.exit(exit_code)

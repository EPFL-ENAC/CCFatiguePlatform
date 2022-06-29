"""
Validate data quality in TST experiment folders
  + check XLS metadata file and show :
    + ERROR when file is not found

  + check CSV TST files and show :
    + ERROR when :
      + mandatory column is not found
      + unexpected type is found in a column
    + WARNING when :
      + unexpected column found
"""

#
# TODO :
# + check JSON files (from XLS)
#

import os
import glob
import pandas as pd
import simplejson as json
import common


def check_experiment(experiment_fp_folder):
    """
    check requirements for an experiment data + experiment metadata (incl tests)
    """
    FIELD_TYPE_CHECK = {
        str: common.check_str_in_dict,
        bool: common.check_bool_in_dict,
    }

    exp = common.get_experiment_meta(experiment_fp_folder)

    logger.write("")
    logger.write(f"Checking experiment {experiment_fp_folder}")
    with logger.indent:
        # Check XLS file
        if exp["metadata_xls_fp"] is not None:
            logger.info(f"metadata found : {exp['metadata_xls_fp']}")
        else:
            logger.error("metadata file not found !")

        # Check experiment metadata from JSON
        logger.info(f"Checking {exp['metadata_json_fp']}")
        with logger.indent:
            with open(exp["metadata_json_fp"], "r") as f:
                exp_metadata = json.load(f)
            # Mandatory fields
            for field in (
                {"path": ("Experiment", "General", "Laboratory"), "type": str},
                {"path": ("Experiment", "General", "Researcher"), "type": str},
                {"path": ("Experiment", "General", "Experiment Type"), "type": str},
                {"path": ("Experiment", "General", "Fracture"), "type": bool},
            ):
                if not common.check_exist_in_dict(exp_metadata, field["path"]):
                    logger.error(
                        f"Mandatory field {'>'.join(field['path'])} is missing"
                    )
                elif not FIELD_TYPE_CHECK[field["type"]](exp_metadata, field["path"]):
                    logger.error(f"Wrong type for field {'>'.join(field['path'])}")

        # check experiment data CSV files
        for test_data_fp in sorted(
            glob.glob(
                os.path.join(
                    experiment_fp_folder, f"TST_{exp['date']}_{exp['test_type']}_*.csv"
                )
            )
        ):
            check_test_data_csv(test_data_fp)


def check_test_data_csv(test_data_fp):
    """
    check requirements for an experiment test data csv file
    """
    EXPECTED_COLUMNS = {
        "Machine_N_cycles": {
            "type": int,
            "mandatory": True,
        },
        "Machine_Load": {
            "type": float,
            "mandatory": True,
        },
        "Machine_Displacement": {
            "type": float,
            "mandatory": True,
        },
        "index": {
            "type": int,
            "mandatory": False,
        },
        "Camera_N_cycles": {
            "type": int,
            "mandatory": False,
        },
        "exx": {
            "type": float,
            "mandatory": False,
        },
        "eyy": {
            "type": float,
            "mandatory": False,
        },
        "exy": {
            "type": float,
            "mandatory": False,
        },
        "crack_length": {
            "type": float,
            "mandatory": False,
        },
        "Th_time": {
            "type": int,
            "mandatory": False,
        },
        "Th_N_cycles": {
            "type": int,
            "mandatory": False,
        },
        "Th_specimen_max": {
            "type": float,
            "mandatory": False,
        },
        "Th_specimen_mean": {
            "type": float,
            "mandatory": False,
        },
        "Th_chamber": {
            "type": float,
            "mandatory": False,
        },
        "Th_uppergrips": {
            "type": float,
            "mandatory": False,
        },
        "Th_lowergrips": {
            "type": float,
            "mandatory": False,
        },
        "Specimen Name": {
            "type": str,
            "mandatory": False,
        },
    }
    COLUMN_TYPE_CHECK = {
        int: common.check_int_column,
        float: common.check_float_column,
        str: common.check_str_column,
    }
    TYPE_NAMES = {
        int: "integer",
        float: "float",
        str: "string",
    }
    tst = {
        "fp": test_data_fp,
        "filename": os.path.basename(test_data_fp),
    }

    logger.write(f"Checking {tst['filename']}")
    with logger.indent:
        dataset = pd.read_csv(
            test_data_fp, sep=",", header=0, dtype={"Specimen_name": "string"}
        )

        found_columns = dataset.columns
        for mandatory_col in filter(
            lambda c: EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS
        ):
            if mandatory_col not in found_columns:
                logger.error(f"mandatory column not found: '{mandatory_col}'")
            else:
                if dataset[mandatory_col].isnull().values.any():
                    logger.error(
                        f"mandatory column has empty values: '{mandatory_col}'"
                    )
        for optional_col in filter(
            lambda c: not EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS
        ):
            if optional_col not in found_columns:
                logger.info(f"optional column not found: '{optional_col}'")
        for found_col in found_columns:
            if found_col not in EXPECTED_COLUMNS:
                logger.warning(f"unexpected column found: '{found_col}'")

        for col in EXPECTED_COLUMNS:
            if col in found_columns:
                if not COLUMN_TYPE_CHECK[EXPECTED_COLUMNS[col]["type"]](dataset, col):
                    logger.error(
                        f"column '{col}' is expected to be"
                        f" of type '{TYPE_NAMES[EXPECTED_COLUMNS[col]['type']]}'"
                    )

        if dataset.isnull().all(axis=1).any():
            logger.error("found empty rows")


if __name__ == "__main__":
    for experiment_fp_folder in common.EXPERIMENT_FP_FOLDERS:
        try:
            with common.Logger(
                os.path.join(
                    experiment_fp_folder,
                    os.path.basename(experiment_fp_folder) + "_check_output.txt",
                )
            ) as logger:
                check_experiment(experiment_fp_folder)
        except common.NotAnExperimentFolder:
            pass

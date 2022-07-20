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
import re
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
            check_test_data_csv(
                test_data_fp,
                exp_metadata["Experiment"]["General"]["Experiment Type"],
                exp_metadata["Experiment"]["General"]["Fracture"],
            )


def check_test_data_csv(test_data_fp, test_type, fracture):
    """
    check requirements for an experiment test data csv file
    """
    """
    EXPECTED_COLUMNS = {
        r"pattern": {
            "type": int|float|string,
            mandatory: True|False,
        }
    """
    EXPECTED_COLUMNS = {
        "Machine_Time": {
            "type": int,
            "mandatory": False,
        },
        "Machine_N_cycles": {
            "type": int,
            "mandatory": False,
        },
        "Machine_Displacement": {
            "type": float,
            "mandatory": False,
        },
        "Machine_Load": {
            "type": float,
            "mandatory": False,
        },
        r"MD_index--\d": {
            "type": int,
            "mandatory": False,
        },
        r"MD_N_cycles--\d": {
            "type": int,
            "mandatory": False,
        },
        r"MD_Displacement--\d": {
            "type": float,
            "mandatory": False,
        },
        r"MD_Load--\d": {
            "type": float,
            "mandatory": False,
        },
        r"u--\d": {
            "type": float,
            "mandatory": False,
        },
        r"v--\d": {
            "type": float,
            "mandatory": False,
        },
        r"exx--\d": {
            "type": float,
            "mandatory": False,
        },
        r"eyy--\d": {
            "type": float,
            "mandatory": False,
        },
        r"exy--\d": {
            "type": float,
            "mandatory": False,
        },
        "Crack_length": {
            "type": float,
            "mandatory": False,
        },
        "Crack_N_cycles": {
            "type": float,
            "mandatory": False,
        },
        "Crack_Displacement": {
            "type": float,
            "mandatory": False,
        },
        "Crack_Load": {
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
        r"T--\d": {
            "type": float,
            "mandatory": False,
        },
        "Storage_modulus": {
            "type": float,
            "mandatory": False,
        },
        "Tan_delta": {
            "type": float,
            "mandatory": False,
        },
        "Specimen_name": {
            "type": str,
            "mandatory": False,
        },
    }
    MANDATORY_TEST_TYPE_SPECIFIC = {
        ("FA", False): [
            [
                "Machine_N_cycles",
                r"MD_N_cycles--\d",
            ],
            [
                "Machine_Displacement",
                r"MD_Displacement--\d",
                r"exx--\d",
            ],
            [
                "Machine_Load",
                r"MD_Load--\d",
            ],
        ],
        ("FA", True): [
            [
                "Crack_N_cycles",
            ],
            [
                "Crack_length",
            ],
        ],
        ("QS", False): [
            [
                "Machine_Displacement",
                r"MD_Displacement--\d",
                r"exx--\d",
            ],
            [
                "Machine_Load",
                r"MD_Load--\d",
            ],
        ],
        ("QS", True): [
            [
                "Machine_Displacement",
                r"MD_Displacement--\d",
                "Crack_length",
                "Crack_Displacement",
            ],
            [
                "Machine_Load",
                r"MD_Load--\d",
                "Crack_Load",
            ],
        ],
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
        for mandatory_col_pattern in filter(
            lambda c: EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS
        ):
            mandatory_col_found = list(
                common.grep_matching_columns(mandatory_col_pattern, found_columns)
            )
            if len(mandatory_col_found) == 0:
                logger.error(f"mandatory column not found: '{mandatory_col_pattern}'")
            else:
                for mandatory_col in mandatory_col_found:
                    if dataset[mandatory_col].isnull().values.any():
                        logger.error(
                            f"mandatory column has empty values: '{mandatory_col}'"
                        )
        for optional_col_pattern in filter(
            lambda c: not EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS
        ):
            optional_col_found = list(
                common.grep_matching_columns(optional_col_pattern, found_columns)
            )

            if len(optional_col_found) == 0:
                logger.info(f"optional column not found: '{optional_col_pattern}'")

        for found_col in found_columns:
            expected = False
            for col_pattern in EXPECTED_COLUMNS:
                if re.match(col_pattern, found_col):
                    expected = True
                    break
            if not expected:
                logger.warning(f"unexpected column found: '{found_col}'")

        for col in found_columns:
            for col_pattern in EXPECTED_COLUMNS:
                if re.match(col_pattern, col):
                    if not COLUMN_TYPE_CHECK[EXPECTED_COLUMNS[col_pattern]["type"]](
                        dataset, col
                    ):
                        logger.error(
                            f"column '{col}' is expected to be of type "
                            f"'{TYPE_NAMES[EXPECTED_COLUMNS[col_pattern]['type']]}'"
                        )

        # Look for type specific mandatory columns
        for mandatory_cols_pattern in MANDATORY_TEST_TYPE_SPECIFIC[
            (test_type, fracture)
        ]:
            for mandatory_col_pattern in mandatory_cols_pattern:
                mandatory_col_found = list(
                    common.grep_matching_columns(mandatory_col_pattern, found_columns)
                )
                if len(mandatory_col_found) != 0:
                    break
            else:
                logger.error(
                    f"mandatory column missing for test type {test_type} "
                    f"{'with' if fracture else 'without'} fracture :"
                )
                logger.error(" or ".join(mandatory_cols_pattern))

        if dataset.isnull().all(axis=1).any():
            logger.error("found empty rows")


if __name__ == "__main__":
    for experiment_fp_folder in common.get_tst_folders_to_parse(
        description="Controls TST data quality"
    ):
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

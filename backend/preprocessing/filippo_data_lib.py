"""
Common tools for TST data preprocessing
"""

import copy
import glob
import json
import os
import re

import numpy as np
import pandas as pd
from datetime import datetime

EXPERIMENTS_FOLDER = os.path.abspath(f"{__file__}/../../../Data")
RAW_EXPERIMENT_FP_FOLDERS = sorted(
    filter(lambda fp: os.path.isdir(fp), glob.glob(f"{EXPERIMENTS_FOLDER}/raw/TST_*"))
)
XLS_TEMPLATE_URL = (
    "https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/main"
    "/Data/TST_Data_Template.xls"
)


class NotAnExperimentFolder(Exception):
    pass


class Logger:
    class Indent:
        def __init__(self):
            self.current_indent = 0

        def __enter__(self):
            self.current_indent += 1
            return self

        def __exit__(self, typ, value, traceback):
            self.current_indent -= 1

    def __init__(self, filename=None, write_to_stdout=True):
        self.filename = filename
        self.write_to_stdout = write_to_stdout
        self.messages = []
        self.warning_count = 0
        self.error_count = 0
        self.mute = False

    def __enter__(self):
        if self.filename is not None:
            self.f_handler = open(self.filename, "w")
        self.indent = Logger.Indent()
        return self

    def __exit__(self, typ, value, traceback):
        if self.filename is not None:
            self.f_handler.close()

    def _write(self, messages=[], end="\n"):
        message = end.join(messages)
        if self.filename is not None:
            self.f_handler.write(f"{message}{end}")
        if self.write_to_stdout and not self.mute:
            print(message, end=end)

    def reset_counts(self):
        self.warning_count = 0
        self.error_count = 0

    def write(self, message="", prefix="", end="\n"):
        indent = " " * self.indent.current_indent
        lines = [f"{indent}{prefix}{line}" for line in message.split("\n")]
        self._write(lines, end)
        self.messages.append(
            {
                "indent": self.indent.current_indent,
                "lines": message.split("\n"),
                "prefix": prefix,
                "end": end,
            }
        )

    def info(self, message="", end="\n"):
        self.write(message=message, prefix="(i): ", end=end)

    def warning(self, message="", end="\n"):
        self.warning_count += 1
        self.write(message=message, prefix="Warning: ", end=end)

    def error(self, message="", end="\n"):
        self.error_count += 1
        self.write(message=message, prefix="ERROR: ", end=end)


class FilippoExperiment:
    """
    Represent TST Experiment data
    contains:
    + exp_meta_meta : meta info about experiment++ metadata
    + experiment : metadata from XLS Experiment tab
    + tests : metadata from XLS Tests tab
    + measures : from ###.CSV files
    """

    def __init__(self, experiment_fp_folder, logger):
        self.experiment_fp_folder = experiment_fp_folder
        self._load_exp_meta_meta()
        self.logger = logger

        self._read_experiment()
        self._cleanup_experiment()
        self._validate_experiment()

        self._validate_files_naming()

        self._read_tests()
        self._cleanup_tests()
        self._validate_tests()

        self._read_measures()
        self._cleanup_measures()
        self._validate_measures()

    def save_preprocessed_data(self):
        """
        Save preprocessed experiment+tests+measures
        """
        os.makedirs(self.exp_meta_meta["preprocessed_folder"], exist_ok=True)
        self._save_preprocessed_experiment()
        self._save_preprocessed_tests()
        self._save_preprocessed_measures()

    def _load_exp_meta_meta(self):
        """
        save self.exp_meta_meta as following dict
        {
            "folder": folder basename
            "researcher_last_name": experiment's researcher_last_name
            "date": experiment's date
            "test_type": experiment's test_type
            "raw_xls_fp": full path to raw metadata xls(x)
            "preprocessed_folder": full path to experiment preprocessed folder
            "preprocessed_experiment_json_fp":
                full path to experiment preprocessed metadata json
            "preprocessed_tests_csv_fp": full path to test preprocessed metadata csv
            "measures": [
                {
                    "raw_fp": full path to raw measures csv file
                    "preprocessed_fp": full path to preprocessed measures csv file
                },
                ...
            ],
        }
        """

        self.exp_meta_meta = {}
        self.exp_meta_meta["folder"] = os.path.basename(self.experiment_fp_folder)
        self.exp_meta_meta["preprocessed_folder"] = os.path.abspath(
            f"{EXPERIMENTS_FOLDER}/preprocessed/{self.exp_meta_meta['folder']}"
        )
        m = re.match("TST_([^_]+)_([0-9-]+)_(FA|QS)", self.exp_meta_meta["folder"])
        if not m:
            raise NotAnExperimentFolder(self.experiment_fp_folder)
        (
            self.exp_meta_meta["researcher_last_name"],
            self.exp_meta_meta["date"],
            self.exp_meta_meta["test_type"],
        ) = m.groups()

        # search metadata XLS(X) file
        filename_without_ext = (
            f"TST_{self.exp_meta_meta['date']}_"
            f"{self.exp_meta_meta['test_type']}_metadata"
        )
        for extension in ("xls", "xlsx"):
            metadata_file = os.path.join(
                self.experiment_fp_folder,
                f"{filename_without_ext}.{extension}",
            )
            if os.path.exists(metadata_file):
                self.exp_meta_meta["raw_xls_fp"] = metadata_file
                self.exp_meta_meta["preprocessed_experiment_json_fp"] = os.path.join(
                    self.exp_meta_meta["preprocessed_folder"], "experiment.json"
                )
                self.exp_meta_meta["preprocessed_tests_csv_fp"] = os.path.join(
                    self.exp_meta_meta["preprocessed_folder"], "tests.csv"
                )
                break
        else:
            self.exp_meta_meta["raw_xls_fp"] = None
            self.exp_meta_meta["preprocessed_experiment_json_fp"] = None
            self.exp_meta_meta["preprocessed_tests_csv_fp"] = None

        # search CSV files
        self.exp_meta_meta["measures"] = [
            {
                "raw_fp": filename,
                "preprocessed_fp": os.path.abspath(
                    os.path.join(
                        self.exp_meta_meta["preprocessed_folder"],
                        "measure_"
                        + f'{int(re.search(r"([0-9]+).csv", filename).group(1)):03d}'
                        + ".csv",
                    )
                ),
            }
            for filename in sorted(
                filter(
                    lambda filename: re.search(
                        f"TST_{self.exp_meta_meta['date']}_"
                        f"{self.exp_meta_meta['test_type']}_([0-9]+).csv",
                        filename,
                    ),
                    glob.glob(
                        os.path.join(
                            self.experiment_fp_folder,
                            f"TST_{self.exp_meta_meta['date']}_"
                            f"{self.exp_meta_meta['test_type']}_*.csv",
                        )
                    ),
                )
            )
        ]

    def _read_experiment(self):
        """
        Read experiment metadata from XLS Experiment tab
        """

        def df_to_nested_dict(df: pd.DataFrame) -> dict:
            """
            Translate df to dict
            """

            def nest(d: dict) -> dict:
                """
                Nest keys as follow :
                >>> nest({('a', 'b', 'c'): 1, ('d', 'e', 'f'): 2, })
                [1] {'a': {'b': {'c': 1}}, 'd': {'e': {'f': 2}}}

                """
                result = {}
                for key, value in d.items():
                    target = result
                    for k in key[:-1]:  # traverse all keys but the last
                        target = target.setdefault(k, {})
                    target[key[-1]] = value
                return result

            d = df.to_dict(orient="index")
            return {k: nest(v) for k, v in d.items()}

        self.logger.info(
            f"Read {os.path.basename(self.exp_meta_meta['raw_xls_fp'])} Experiment tab"
        )
        with self.logger.indent:
            if self.exp_meta_meta["raw_xls_fp"] is None:
                self.logger.error("xls metadata file not found !")
                raise NotAnExperimentFolder()

            experiment_df = pd.read_excel(
                self.exp_meta_meta["raw_xls_fp"],
                sheet_name="Experiment",
                header=[0, 1],
            )
            self.experiment = df_to_nested_dict(experiment_df)[0]

    def _cleanup_experiment(self):
        """
        Cleanup experiment metadata
        + small typos fixed
        + only expected fields
        + only non-empty fields (null/NaN)
        + fields casted as bool / float (double) / int / str
        """

        def to_bool(value):
            """
            transform string value to boolean
            """
            if type(value) == bool:
                return value
            elif type(value) == str:
                if value.lower().startswith("y") or value == "1":
                    return True
                elif value.lower().startswith("n") or value == "0":
                    return False
                else:
                    raise ValueError
            else:
                return to_bool(str(value))

        self.logger.info("Cleanup experiment")
        with self.logger.indent:

            FilippoExperiment.__dict_cleanup(self.experiment)

            CASTING = {
                bool: to_bool,
                int: int,
                float: float,
                str: str,
            }

            fixed_experiment = {}

            for constraint in (
                # GENERAL SECTION
                {"path": "general>laboratory", "type": str},
                {"path": "general>researcher", "type": str},
                {"path": "general>date", "type": str},
                {"path": "general>experiment type", "type": str},
                {"path": "general>fatigue test type FA", "type": str},
                {"path": "general>FA add info", "type": str},
                {"path": "general>quasi-static test type QS", "type": str},
                {"path": "general>QS add info", "type": str},
                {"path": "general>other test type OT", "type": str},
                {"path": "general>OT add info", "type": str},
                {"path": "general>fracture mode", "type": str},
                {"path": "general>additional info", "type": str},                
                {"path": "general>control mode CM", "type": str},
                {"path": "general>CM add info", "type": str},
                {"path": "general>fatigue control mode FCM", "type": str},
                {"path": "general>FCM add info", "type": str},
                {"path": "general>measuring equipment", "type": str},
                {"path": "general>reliability level", "type": float},
                # PUBLICATION SECTION
                {"path": "publication>doi", "type": str},
                # MATERIAL TYPE SECTION
                {"path": "material type>material tested", "type": str},
                {"path": "material type>sample type", "type": str},
                {"path": "material type>fiber material", "type": str},
                {"path": "material type>fiber form", "type": str},
                {"path": "material type>area density", "type": str},
                {"path": "material type>resin", "type": str},
                {"path": "material type>hardener", "type": str},
                {"path": "material type>mixing ratio", "type": str},
                {"path": "material type>curing time", "type": float},
                {"path": "material type>curing temperature", "type": float},
                {"path": "material type>curing pressure", "type": float},
                {"path": "material type>postcuring time", "type": float},
                {"path": "material type>postcuring temperature", "type": float},
                {"path": "material type>postcuring pressure", "type": float},
                # LAMINATES AND ASSEMBLIES SECTION
                {"path": "laminates and assemblies>stacking sequence", "type": str},
                {"path": "laminates and assemblies>fiber volume ratio", "type": str},
            ):
                try:
                    try:
                        isnan = np.isnan(
                            FilippoExperiment.__get_val_at(self.experiment, constraint["path"])
                        )
                    except TypeError:
                        isnan = False
                    if not isnan:
                        # cast and save expected values
                        FilippoExperiment.__set_val_at(
                            fixed_experiment,
                            constraint["path"],
                            CASTING[constraint["type"]](
                                FilippoExperiment.__get_val_at(
                                    self.experiment, constraint["path"]
                                )
                            ),
                        )
                except KeyError:
                    pass
                except ValueError as e:
                    self.logger.error(f"exception: {e}")

            self.experiment = fixed_experiment

    def _validate_experiment(self):
        """
        Validate experiment constraints metadata
        """
        self.logger.info("Validate experiment")
        with self.logger.indent:
            # Block 1: Mandatory Fields Check
            # Description: Check that all mandatory fields are present in the experiment metadata.
            for col_constraint in (
                {
                    "path": "general>laboratory",
                    "mandatory": True,
                },
                {
                    "path": "general>researcher",
                    "mandatory": True,
                },
                {
                    "path": "general>date",
                    "mandatory": True,
                },
                {
                    "path": "general>experiment type",
                    "mandatory": True,
                },
                {
                    "path": "general>control mode",
                    "mandatory": True,
                },
                {
                    "path": "general>fracture mode",
                    "mandatory": (
                        FilippoExperiment.__get_val_at(self.experiment, "general>quasi-static test type", "") == "fracture"
                        or FilippoExperiment.__get_val_at(self.experiment, "general>fatigue test type", "") == "fracture"
                    ),
                    "mandatory_condition": "when quasi-static test type or fatigue test type is 'fracture'",
                },
                {
                    "path": "general>additional info",
                    "mandatory": False,
                },
                {
                    "path": "general>fatigue test type",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA",
                    "mandatory_condition": "when experiment type is 'FA'",
                },
                {
                    "path": "general>reliability level",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA",
                    "mandatory_condition": "when experiment type is 'FA'",
                },
                {
                    "path": "general>quasi-static test type",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "QS",
                    "mandatory_condition": "when experiment type is 'QS'",
                },
                {
                    "path": "general>other test type",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "TM",
                    "mandatory_condition": "when experiment type is 'OT'",
                },
                {
                    "path": "material type>material tested",
                    "mandatory": True,
                },
                {
                    "path": "material type>sample type",
                    "mandatory": True,
                },
            ):
                try:
                    FilippoExperiment.__get_val_at(self.experiment, col_constraint["path"])
                except KeyError:
                    if col_constraint["mandatory"]:
                        self.logger.error(
                            "missing mandatory column "
                            f"{col_constraint.get('mandatory_condition', '')}: "
                            f"{col_constraint['path']}"
                        )

            # Block 2: Date Format Validation
            # Description: Ensure that the 'general>date' field is in the correct YYYY-MM format.
            if not re.match(
                r"\d{4}-\d{2}",
                FilippoExperiment.__get_val_at(self.experiment, "general>date"),
            ):
                self.logger.error(
                    "Unrecognized Date : "
                    f"'{FilippoExperiment.__get_val_at(self.experiment, 'general>date')}'"
                )

            # Block 3: Enum Values Check
            # Description: Verify that fields have allowed values based on defined enumerations.
            for col_constraint in filter(
                lambda constr: constr["check_needed"],
                (
                    {
                        "check_needed": True,
                        "path": "general>experiment type",
                        "enum": (
                            "FA",
                            "QS",
                            "OT",
                        ),
                    },
                    {
                        "check_needed": (
                            FilippoExperiment.__get_val_at(self.experiment, "general>quasi-static test type", "") == "fracture"
                            or FilippoExperiment.__get_val_at(self.experiment, "general>fatigue test type", "") == "fracture"
                        ),
                        "path": "general>fracture mode",
                        "enum": (
                            "Mode I",
                            "Mode II",
                            "Mode III",
                            "Mixed-Mode",
                            "Other/NA",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            ""
                        ) == "FA",
                        "fields": [
                            {
                                "path": "general>fatigue control mode",
                                "enum": (
                                    "CA",
                                    "VA",
                                    "BL",
                                    "Combined",
                                    "Other/NA",
                                ),
                            },
                            {
                                "path": "general>fatigue test type",
                                "enum": (
                                    "Tensile",
                                    "Compressive",
                                    "Shear",
                                    "Bending",
                                    "Fracture",
                                    "Other/NA",
                                ),
                            },
                        ]
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "QS",
                        "path": "general>quasi-static test type",
                        "enum": (
                            "Tensile",
                            "Compressive",
                            "Shear",
                            "Bending",
                            "Fracture",
                            "Other/NA",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "TM",
                        "path": "general>temperature test type",
                        "enum": (
                            "DMA",
                            "DSC",
                            "Fire",
                            "Other/NA",
                        ),
                    },
                    {
                        "check_needed": True,
                        "path": "general>control mode",
                        "enum": (
                            "Load Controlled",
                            "Displacement Controlled",
                            "Other/NA",
                        ),
                    },
                ),
            ):
                try:
                    val = FilippoExperiment.__get_val_at(
                        self.experiment, col_constraint["path"]
                    )
                    if val not in col_constraint["enum"]:
                        self.logger.error(
                            "unrecognized value for column "
                            f"{col_constraint['path']} = '{val}'"
                        )
                except KeyError:
                    pass

            # Block 4: Additional Info Check for 'Other/NA' (Direct Checks)
            # Description: Directly check that if a field is 'Other/NA', its corresponding additional info field is provided.
            if self.experiment.get("general>fatigue test type") == "Other/NA":
                if not self.experiment.get("general>FA add info", "").strip():
                    self.logger.error(
                        "Il campo 'general>FA add info' è obbligatorio quando 'general>fatigue test type' è 'Other/NA'."
                    )

            if self.experiment.get("general>quasi-static test type") == "Other/NA":
                if not self.experiment.get("general>QS add info", "").strip():
                    self.logger.error(
                        "Il campo 'general>QS add info' è obbligatorio quando 'general>quasi-static test type' è 'Other/NA'."
                    )

            if self.experiment.get("general>control mode") == "Other/NA":
                if not self.experiment.get("general>CM add info", "").strip():
                    self.logger.error(
                        "Il campo 'general>CM add info' è obbligatorio quando 'general>control mode' è 'Other/NA'."
                    )

            if self.experiment.get("general>fatigue control mode") == "Other/NA":
                if not self.experiment.get("general>FCM add info", "").strip():
                    self.logger.error(
                        "Il campo 'general>FCM add info' è obbligatorio quando 'general>fatigue control mode' è 'Other/NA'."
                    )

            if self.experiment.get("general>temperature test type") == "Other/NA":
                if not self.experiment.get("general>OT add info", "").strip():
                    self.logger.error(
                        "Il campo 'general>OT add info' è obbligatorio quando 'general>temperature test type' è 'Other/NA'."
                    )

            # Block 5: Additional Info Check for 'Other/NA' (Structured Constraint)
            # Description: Using a structured constraints list to validate that the additional info field is provided if the main field is 'Other/NA'.
            other_fields_constraints = (
                {
                    "path": "general>fatigue test type",
                    "expected": "Other/NA",
                    "additional": "general>FA add info",
                    "type": str,
                },
                {
                    "path": "general>quasi-static test type",
                    "expected": "Other/NA",
                    "additional": "general>QS add info",
                    "type": str,
                },
                {
                    "path": "general>control mode",
                    "expected": "Other/NA",
                    "additional": "general>CM add info",
                    "type": str,
                },
                {
                    "path": "general>fatigue control mode",
                    "expected": "Other/NA",
                    "additional": "general>FCM add info",
                    "type": str,
                },
                {
                    "path": "general>temperature test type",
                    "expected": "Other/NA",
                    "additional": "general>OT add info",
                    "type": str,
                },
            )
            for constraint in other_fields_constraints:
                try:
                    main_val = FilippoExperiment.__get_val_at(self.experiment, constraint["path"])
                    if main_val == constraint["expected"]:
                        try:
                            additional_val = FilippoExperiment.__get_val_at(self.experiment, constraint["additional"])
                            if not additional_val or (isinstance(additional_val, str) and additional_val.strip() == ""):
                                self.logger.error(
                                    f"Il campo '{constraint['additional']}' è obbligatorio quando '{constraint['path']}' è 'Other/NA'."
                                )
                        except KeyError:
                            self.logger.error(
                                f"Il campo '{constraint['additional']}' è obbligatorio quando '{constraint['path']}' è 'Other/NA'."
                            )
                except KeyError:
                    # If the main field is not present, the error is already handled by other checks.
                    pass

            # Block 6: Type Check
            # Description: Confirm that each field's value is of the expected data type.
            for col_constraint in (
                {
                    "path": "general>laboratory",
                    "type": str,
                },
                {
                    "path": "general>researcher",
                    "type": str,
                },
                {
                    "path": "general>reliability level",
                    "type": float,
                },
                {
                    "path": "publication>doi",
                    "type": str,
                },
                {
                    "path": "material type>material tested",
                    "type": str,
                },
                {
                    "path": "material type>sample type",
                    "type": str,
                },
                {
                    "path": "material type>fiber material",
                    "type": str,
                },
                {
                    "path": "material type>fiber form",
                    "type": str,
                },
                {
                    "path": "material type>area density",
                    "type": str,
                },
                {
                    "path": "material type>resin",
                    "type": str,
                },
                {
                    "path": "material type>hardener",
                    "type": str,
                },
                {
                    "path": "material type>mixing ratio",
                    "type": str,
                },
                {
                    "path": "material type>curing time",
                    "type": float,
                },
                {
                    "path": "material type>curing temperature",
                    "type": float,
                },
                {
                    "path": "material type>curing pressure",
                    "type": float,
                },
                {
                    "path": "material type>postcuring time",
                    "type": float,
                },
                {
                    "path": "material type>postcuring temperature",
                    "type": float,
                },
                {
                    "path": "material type>postcuring pressure",
                    "type": float,
                },
                {
                    "path": "laminates and assemblies>stacking sequence",
                    "type": str,
                },
                {
                    "path": "laminates and assemblies>fiber volume ratio",
                    "type": float,
                },
            ):
                try:
                    val = FilippoExperiment.__get_val_at(self.experiment, col_constraint["path"])
                    if type(val) != col_constraint["type"]:
                        self.logger.error(
                            f"Wrong type for column {col_constraint['path']}: {val}"
                        )
                except KeyError:
                    pass

    def _save_preprocessed_experiment(self):
        """
        Save experiment metadata to JSON preprocessed file
        """
        self.logger.info("Save experiment preprocessed")
        with self.logger.indent:
            with open(self.exp_meta_meta["preprocessed_experiment_json_fp"], "w") as f:
                json.dump(self.experiment, f, indent=2)
            self.logger.info(
                "saved "
                + os.path.basename(
                    self.exp_meta_meta["preprocessed_experiment_json_fp"]
                )
            )

    def _validate_files_naming(self):
        """
        Validate that metadata from file/folder naming matches those found in the XLS
        """

        if self.exp_meta_meta["date"] != self.experiment["general"]["date"]:
            self.logger.error(
                "Date from experiment folder "
                f"({self.exp_meta_meta['date']}) "
                "doesn't match the one from the XLS file "
                f"({self.experiment['general']['date']})"
            )

        if (
            self.exp_meta_meta["test_type"]
            != self.experiment["general"]["experiment type"]
        ):
            self.logger.error(
                "Experiment type from experiment folder "
                f"({self.exp_meta_meta['test_type']}) "
                "doesn't match the one from the XLS file "
                f"({self.experiment['general']['experiment type']})"
            )

    def _read_tests(self):
        """
        Read tests metadata from XLS Tests tab
        """
        self.logger.info(
            f"Read {os.path.basename(self.exp_meta_meta['raw_xls_fp'])} Tests tab"
        )
        with self.logger.indent:
            self.tests = pd.read_excel(
                self.exp_meta_meta["raw_xls_fp"], sheet_name="Tests"
            )

    def _cleanup_tests(self):
        """
        Cleanup tests metadata
        + small typos fixed
        + only expected columns
        + fields casted as bool / float (double) / int / str
        """
        self.logger.info("Cleanup tests")
        with self.logger.indent:
            FilippoExperiment.__dict_cleanup(self.tests)
            EXPECTED_COLS = (
                "sequential number",
                "specimen name",
                "number of cycles",
                "controlled loading rate",
                "controlled displacement rate",
                "stress ratio",
                "maximum stress",
                "displacement ratio", 
                "maximum displacement",
                "frequency",
                "run out",
                "length",
                "width",
                "thickness",
                "temperature",
                "humidity",
                "initial crack length",
                "Subset size", 
                "Step size", 
                # r"x coordinate of measuring point \d",
                # r"y coordinate of measuring point \d",
            )

            # Strip spaces and \n on column names
            self.tests.columns = self.tests.columns.str.strip(" \n").str.lower()

            found_matching_columns = set()
            for pattern in EXPECTED_COLS:
                found_matching_columns.update(
                    list(
                        FilippoExperiment.__grep_matching_columns(pattern, self.tests.columns)
                    )
                )

            # drop unexpected columns
            unexpected_columns = list(set(self.tests.columns) - found_matching_columns)

            # Remove unexpected columns
            if len(unexpected_columns) != 0:
                for unexpected_column in unexpected_columns:
                    self.logger.warning(f"drop unexpected column {unexpected_column}")
                self.tests = self.tests.drop(columns=unexpected_columns)

    # def _validate_tests(self):
    #    """
    #    Validate tests metadata
    #    """
    #    self.logger.info("Validate tests")
    #    with self.logger.indent:
            # Check mandatory columns
            # for col_constraint in (
            #     {
            #         "paths": ("sequential number",),
            #         "mandatory": True,
            #     },
            #     {
            #         "paths": ("stress ratio",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA",
            #         "mandatory_condition": "when experiment type is 'FA'",
            #     },
            #     {
            #         "paths": ("maximum stress",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA",
            #         "mandatory_condition": "when experiment type is 'FA'",
            #     },
            #     {
            #         "paths": (
            #             "frequency",
            #             "displacement controlled loading rate",
            #             "load controlled loading rate",
            #         ),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA",
            #         "mandatory_condition": "when experiment type is 'FA'",
            #     },
            #     {
            #         "paths": ("run out",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA",
            #         "mandatory_condition": "when experiment type is 'FA'",
            #     },
            #     {
            #         "paths": ("length",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA"
            #         and not FilippoExperiment.__get_val_at(
            #             self.experiment, "general>fracture", False
            #         ),
            #         "mandatory_condition": (
            #             "when experiment type is 'FA' and no fracture"
            #         ),
            #     },
            #     {
            #         "paths": ("width",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA"
            #         and not FilippoExperiment.__get_val_at(
            #             self.experiment, "general>fracture", False
            #         ),
            #         "mandatory_condition": (
            #             "when experiment type is 'FA' and no fracture"
            #         ),
            #     },
            #     {
            #         "paths": ("thickness",),
            #         "mandatory": FilippoExperiment.__get_val_at(
            #             self.experiment, "general>experiment type", ""
            #         )
            #         == "FA"
            #         and not FilippoExperiment.__get_val_at(
            #             self.experiment, "general>fracture", False
            #         ),
            #         "mandatory_condition": (
            #             "when experiment type is 'FA' and no fracture"
            #         ),
            #     },
            # ):
            #     if col_constraint["mandatory"]:
            #         found = any(
            #             path in self.tests.columns for path in col_constraint["paths"]
            #         )
            #         if not found:
            #             self.logger.error(
            #                 "missing mandatory column "
            #                 f"{col_constraint.get('mandatory_condition', '')}: "
            #                 f"{col_constraint['paths']}"
            #             )

            # Type check
            # for col_constraint in (
            #     {
            #         "path_pattern": "Sequential number",
            #         "type": int,
            #     },
            #     {
            #         "path_pattern": "Specimen name TRIAL",
            #         "type": str,
            #     },
            #     {
            #         "path_pattern": "Stress Ratio",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Maximum Stress",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Frequency",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Run out",
            #         "type": bool,
            #     },
            #     {
            #         "path_pattern": "Displacement controlled loading rate",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Load controlled loading rate",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Length",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Width",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Thickness",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Temperature",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "Humidity",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": "initial crack length",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": r"x coordinate of measuring point \d",
            #         "type": float,
            #     },
            #     {
            #         "path_pattern": r"y coordinate of measuring point \d",
            #         "type": float,
            #     },
            # ):
            #     for path in list(
            #         FilippoExperiment.__grep_matching_columns(
            #             col_constraint["path_pattern"], self.tests.columns
            #         )
            #     ):
            #         try:
            #             val = FilippoExperiment.__get_val_at(self.experiment, path)
            #             if type(val) != col_constraint["type"]:
            #                 self.logger.error(
            #                     f"Wrong type for column {col_constraint['path']}: {val}"
            #                 )
            #         except KeyError:
            #             pass

    def _validate_tests(self):
        """
        Validate tests metadata.
        """
        self.logger.info("Validate tests")
        with self.logger.indent:
            # Preleviamo i valori condizionali dall'experiment
            test_type = FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "")
            control_mode = FilippoExperiment.__get_val_at(self.experiment, "general>control mode", "")
            fracture = FilippoExperiment.__get_val_at(self.experiment, "general>fracture", False)

            constraints = []
            
            # Vincoli sempre obbligatori
            constraints.append({
                "paths": ("sequential number",),
                "mandatory": True,
                "mandatory_condition": "always"
            })
            constraints.append({
                "paths": ("specimen name",),
                "mandatory": True,
                "mandatory_condition": "always"
            })
            constraints.append({
                "paths": ("width",),
                "mandatory": True,
                "mandatory_condition": "always"
            })
            constraints.append({
                "paths": ("thickness",),
                "mandatory": True,
                "mandatory_condition": "always"
            })
            
            # Se il test type è FA, allora "number of cycles" e "run out" sono obbligatorie
            if test_type == "FA":
                constraints.append({
                    "paths": ("number of cycles",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA"
                })
                constraints.append({
                    "paths": ("run out",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA"
                })
            
            # Se fracture è true in experiment, allora "initial crack length" è obbligatoria
            if fracture:
                constraints.append({
                    "paths": ("initial crack length",),
                    "mandatory": True,
                    "mandatory_condition": "when fracture is true"
                })
            
            # Vincoli per test type FA e Load Controlled
            if test_type == "FA" and control_mode == "Load Controlled":
                constraints.append({
                    "paths": ("stress ratio",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA and control mode is Load Controlled"
                })
                constraints.append({
                    "paths": ("maximum stress",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA and control mode is Load Controlled"
                })
                # Vincolo OR: almeno una tra "load controlled loading rate" e "frequency"
                constraints.append({
                    "paths": ("controlled loading rate", "frequency"),
                    "mandatory": True,
                    "constraint_type": "or",
                    "mandatory_condition": "when test type is FA and control mode is Load Controlled (one among load controlled loading rate and frequency)"
                })
            
            # Vincoli per test type FA e Displacement Controlled
            if test_type == "FA" and control_mode == "Displacement Controlled":
                constraints.append({
                    "paths": ("displacement ratio",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA and control mode is Displacement Controlled"
                })
                constraints.append({
                    "paths": ("maximum displacement",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is FA and control mode is Displacement Controlled"
                })
                # Vincolo OR: almeno una tra "controlled displacement rate" e "frequency"
                constraints.append({
                    "paths": ("controlled displacement rate", "frequency"),
                    "mandatory": True,
                    "constraint_type": "or",
                    "mandatory_condition": "when test type is FA and control mode is Displacement Controlled (one among controlled displacement rate and frequency)"
                })
            
            # Vincoli per test type QS e Load Controlled
            if test_type == "QS" and control_mode == "Load Controlled":
                constraints.append({
                    "paths": ("controlled loading rate",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is QS and control mode is Load Controlled"
                })
            
            # Vincoli per test type QS e Displacement Controlled
            if test_type == "QS" and control_mode == "Displacement Controlled":
                constraints.append({
                    "paths": ("controlled displacement rate",),
                    "mandatory": True,
                    "mandatory_condition": "when test type is QS and control mode is Displacement Controlled"
                })
            
            # Esecuzione dei controlli dei vincoli
            for cons in constraints:
                paths = cons["paths"]
                mandatory = cons["mandatory"]
                condition_text = cons.get("mandatory_condition", "")
                constraint_type = cons.get("constraint_type", "exact")  # default: exact
                if mandatory:
                    if constraint_type == "exact":
                        for col in paths:
                            if col not in self.tests.columns:
                                self.logger.error(f"Missing mandatory column {condition_text}: '{col}'")
                    elif constraint_type == "or":
                        if not any(col in self.tests.columns for col in paths):
                            self.logger.error(f"Missing mandatory column {condition_text}: one of {paths}")
            
            # --- Type check
            type_constraints = (
                {
                    "path_pattern": "Sequential number",
                    "type": int,
                },
                {
                    "path_pattern": "Specimen name",
                    "type": str,
                },
                {
                    "path_pattern": "number of cycles",
                    "type": int,
                },
                {
                    "path_pattern": "Stress Ratio",
                    "type": float,
                },
                {
                    "path_pattern": "Maximum Stress",
                    "type": float,
                },
                {
                    "path_pattern": "displacement Ratio",
                    "type": float,
                },
                {
                    "path_pattern": "Maximum displacement",
                    "type": float,
                },
                {
                    "path_pattern": "Frequency",
                    "type": float,
                },
                {
                    "path_pattern": "Run out",
                    "type": bool,
                },
                {
                    "path_pattern": "controlled displacement rate",
                    "type": float,
                },
                {
                    "path_pattern": "controlled loading rate",
                    "type": float,
                },
                {
                    "path_pattern": "Length",
                    "type": float,
                },
                {
                    "path_pattern": "Width",
                    "type": float,
                },
                {
                    "path_pattern": "Thickness",
                    "type": float,
                },
                {
                    "path_pattern": "Temperature",
                    "type": float,
                },
                {
                    "path_pattern": "Humidity",
                    "type": float,
                },
                {
                    "path_pattern": "initial crack length",
                    "type": float,
                },
                {
                    "path_pattern": "Subset size", 
                    "type": float,
                },
                {
                    "path_pattern": "Step size",
                    "type": float,
                },
                # {
                #    "path_pattern": r"x coordinate of measuring point \d",
                #    "type": float,
                # },
                # {
                #    "path_pattern": r"y coordinate of measuring point \d",
                #    "type": float,
                # },
            )
            
            for tc in type_constraints:
                for path in list(FilippoExperiment.__grep_matching_columns(tc["path_pattern"], self.tests.columns)):
                    try:
                        val = FilippoExperiment.__get_val_at(self.experiment, path)
                        if type(val) != tc["type"]:
                            self.logger.error(f"Wrong type for column '{tc['path_pattern']}': {val}")
                    except KeyError:
                        pass

    
    def _save_preprocessed_tests(self):
        """
        Save tests metadata to CSV preprocessed file
        """
        self.logger.info("Save tests preprocessed")
        with self.logger.indent:
            self.tests.to_csv(
                self.exp_meta_meta["preprocessed_tests_csv_fp"],
                index=False,
            )
            self.logger.info(
                "saved "
                + os.path.basename(self.exp_meta_meta["preprocessed_tests_csv_fp"])
            )

    def _read_measures(self):
        """
        Read measures from CSV files
        """
        self.measures_list = copy.deepcopy(self.exp_meta_meta["measures"])
        for measures in self.measures_list:
            self.logger.info(f"Read measures {os.path.basename(measures['raw_fp'])}")
            with self.logger.indent:
                measures["df"] = pd.read_csv(measures["raw_fp"], low_memory=False)

    def _cleanup_measures(self):
        """
        Cleanup measures
        + small typos fixed
        + only expected columns
        """
        for measures in self.measures_list:
            self.logger.info(f"Cleanup measures {os.path.basename(measures['raw_fp'])}")
            with self.logger.indent:

                # Strip spaces and \n on column names
                measures["df"].columns = measures["df"].columns.str.strip(" \n")
                EXPECTED_COLUMNS = (
                        "Test_Date",
                        "Load",
                        "u",
                        "v",
                        "e_xx",
                        "e_yy", 
                        "e_xy",
                        "Crack_length",
                        "Specimen_name",
                        "N_cycles",
                #     "Machine_Time",
                #     "Machine_N_cycles",
                #     "Machine_Displacement",
                #     "Machine_Load",
                #     r"MD_index--\d",
                #     r"MD_N_cycles--\d",
                #     r"MD_Displacement--\d",
                #     r"MD_Load--\d",
                #     r"u--\d",
                #     r"v--\d",
                #     r"exx--\d",
                #     r"eyy--\d",
                #     r"exy--\d",
                #     "Crack_length",
                #     "Crack_N_cycles",
                #     "Crack_Displacement",
                #     "Crack_Load",
                #     "Th_time",
                #     "Th_N_cycles",
                #     "Th_specimen_max",
                #     "Th_specimen_mean",
                #     "Th_chamber",
                #     "Th_uppergrips",
                #     "Th_lowergrips",
                #     r"T--\d",
                #     "Storage_modulus",
                #     "Tan_delta",
                #     "Specimen_name",
                )

                found_matching_columns = set()
                for pattern in EXPECTED_COLUMNS:
                    found_matching_columns.update(
                        list(
                            FilippoExperiment.__grep_matching_columns(
                                pattern, measures["df"].columns
                            )
                        )
                    )

                # drop unexpected columns
                unexpected_columns = list(
                    set(measures["df"].columns) - found_matching_columns
                )

                if len(unexpected_columns) != 0:
                    for unexpected_column in unexpected_columns:
                        self.logger.warning(
                            f"drop unexpected column {unexpected_column}"
                        )
                    measures["df"] = measures["df"].drop(columns=unexpected_columns)

    def _validate_measures(self):
        """
        Validate measures
        """
        for measures in self.measures_list:
            self.logger.info(
                f"Validate measures {os.path.basename(measures['raw_fp'])}"
            )
            with self.logger.indent:
                EXPECTED_COLUMNS = {
                    "Test_Date": {
                        "type": str,
                        "mandatory": False,
                    },
                    "Load": {
                        "type": float,
                        "mandatory": True,
                    },
                    "u": {
                        "type": float,
                        "mandatory": False,
                    },
                    "v": {
                        "type": float,
                        "mandatory": False,
                    },
                    "e_xx": {
                        "type": float,
                        "mandatory": False,
                    },
                    "e_yy": {
                        "type": float,
                        "mandatory": False,
                    },
                    "e_xy": {
                        "type": float,
                        "mandatory": False,
                    },
                    "Crack_length": {
                        "type": float,
                        "mandatory": False,
                    },
                    "Specimen_name": {
                        "type": str,
                        "mandatory": True,
                    },
                    "N_cycles": {
                        "type": int,
                        "mandatory": False,
                    },
                #     "Machine_Time": {
                #         "type": str,
                #         "mandatory": False,
                #     },
                #     "Machine_N_cycles": {
                #         "type": int,
                #         "mandatory": False,
                #     },
                #     "Machine_Displacement": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Machine_Load": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"MD_index--\d": {
                #         "type": int,
                #         "mandatory": False,
                #     },
                #     r"MD_time--\d": {
                #         "type": str,
                #         "mandatory": False,
                #     },
                #     r"MD_N_cycles--\d": {
                #         "type": int,
                #         "mandatory": False,
                #     },
                #     r"MD_Displacement--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"MD_Load--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"u--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"v--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"exx--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"eyy--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"exy--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Crack_length": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Crack_N_cycles": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Crack_Displacement": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Crack_Load": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Th_time": {
                #         "type": int,
                #         "mandatory": False,
                #     },
                #     "Th_N_cycles": {
                #         "type": int,
                #         "mandatory": False,
                #     },
                #     "Th_specimen_max": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Th_specimen_mean": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Th_chamber": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Th_uppergrips": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Th_lowergrips": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     r"T--\d": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Storage_modulus": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                #     "Tan_delta": {
                #         "type": float,
                #         "mandatory": False,
                #     },
                }

                RELAXED_MANDATORY_COLUMNS = {
                    "Specimen_name",
                    "Test_Date",
                }

                MANDATORY_TEST_TYPE_SPECIFIC = {
                    ("FA", False): [
                        [
                            "N_cycles",
                        ],
                        [
                            "u",
                            "exx",
                        ],
                    ],
                    ("FA", True): [
                        [
                            "N_cycles",
                        ],
                        [
                            "Crack_length",
                        ],
                    ],
                    ("QS", False): [
                        [
                            "u",
                            "exx",
                        ],
                    ],
                    ("QS", True): [
                        [
                            "Crack_length",
                        ],
                    ],
                    # ("TM", False): [
                    #    [
                    #        r"T--\d",
                    #    ],
                    #    [
                    #        "Storage_modulus",
                    #        "Tan_delta",
                    #        "Machine_Load",
                    #        r"MD_Load--\d",
                    #    ],
                    #],
                    # ("TM", True): [
                    #    [
                    #        r"T--\d",
                    #    ],
                    #    [
                    #        "Storage_modulus",
                    #        "Tan_delta",
                    #        "Machine_Load",
                    #        r"MD_Load--\d",
                    #    ],
                    #],
                }
                COLUMN_TYPE_CHECK = {
                    int: FilippoExperiment.__check_int_column,
                    float: FilippoExperiment.__check_float_column,
                    str: FilippoExperiment.__check_str_column,
                    datetime: FilippoExperiment.__check_datetime_column,  # 👈 nuovo
                }
                TYPE_NAMES = {
                    int: "integer",
                    float: "float",
                    str: "string",
                    datetime: "date (YYYY-MM-DD)",
                }

            # Check for mandatory columns
            for mandatory_col_pattern in filter(lambda c: EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS):
                mandatory_col_found = list(
                    FilippoExperiment.__grep_matching_columns(mandatory_col_pattern, measures["df"].columns)
                )
                if len(mandatory_col_found) == 0:
                    self.logger.error(f"mandatory column not found: '{mandatory_col_pattern}'")
                else:
                    for mandatory_col in mandatory_col_found:
                        if mandatory_col in RELAXED_MANDATORY_COLUMNS:
                            if measures["df"][mandatory_col].notnull().sum() == 0:
                                self.logger.error(f"mandatory column '{mandatory_col}' has no value at all")
                        else:
                            if measures["df"][mandatory_col].isnull().values.any():
                                self.logger.error(f"mandatory column has empty values: '{mandatory_col}'")
            
            # --- Nuovo blocco: Controllo lunghezza consistente per le colonne obbligatorie "strict"
            strict_mandatory_lengths = {}
            for col_pattern in filter(
                lambda c: EXPECTED_COLUMNS[c]["mandatory"] and c not in RELAXED_MANDATORY_COLUMNS,
                EXPECTED_COLUMNS
            ):
                found_cols = list(
                    FilippoExperiment.__grep_matching_columns(col_pattern, measures["df"].columns)
                )
                for col in found_cols:
                    strict_mandatory_lengths[col] = measures["df"][col].notnull().sum()
            
            # Info for all non-mandatory columns that are not present
            for optional_col_pattern in filter(
                lambda c: not EXPECTED_COLUMNS[c]["mandatory"],
                EXPECTED_COLUMNS,
            ):
                optional_col_found = list(
                    FilippoExperiment.__grep_matching_columns(
                        optional_col_pattern, measures["df"].columns
                    )
                )

                if len(optional_col_found) == 0:
                    self.logger.info(
                        f"optional column not found: '{optional_col_pattern}'"
                    )

            # Check column data type
            for col in measures["df"].columns:
                for col_pattern in EXPECTED_COLUMNS:
                    if re.match(col_pattern, col):
                        type = EXPECTED_COLUMNS[col_pattern]["type"]
                        if not COLUMN_TYPE_CHECK[type](measures["df"], col):
                            self.logger.error(
                                f"column '{col}' is expected to be of type "
                                f"'{TYPE_NAMES[type]}'"
                            )
            # --- Controllo specifico per il formato della data in Test_Date
            if "Test_Date" in measures["df"].columns:
                for date_val in measures["df"]["Test_Date"].dropna().unique():
                    if not re.match(r"^\d{4}-\d{2}", str(date_val)):
                        self.logger.error(f"Unrecognized Test_Date: '{date_val}'")

            # Look for type specific mandatory columns
            try:
                experiment_type = FilippoExperiment.__get_val_at(
                    self.experiment, "general>experiment type"
                )
                fracture = FilippoExperiment.__get_val_at(
                    self.experiment, "general>fracture"
                )
            except KeyError:
                self.logger.error(
                    "XLS file seems broken. Please double check it follows "
                    f"template provided here: {XLS_TEMPLATE_URL}"
                )
                return
            
            test_type_cols_found = set()

            # Per ciascun gruppo, basta che esista ALMENO una colonna
            # Se ne esistono più di una, verifichiamo che abbiano lunghezze coerenti fra loro
            groups = MANDATORY_TEST_TYPE_SPECIFIC.get((experiment_type, fracture), [])
            for group in groups:
                group_found = []
                for pattern in group:
                    group_found.extend(list(FilippoExperiment.__grep_matching_columns(pattern, measures["df"].columns)))
                if len(group_found) == 0:
                    # Nessuna colonna trovata in questo gruppo → errore
                    self.logger.error(
                        f"mandatory column missing for test type {experiment_type} "
                        f"{'with' if fracture else 'without'} fracture :"
                    )
                    self.logger.error(" or ".join(group))
                else:
                    # Se ci sono 2+ colonne presenti, controlla che abbiano la stessa lunghezza
                    if len(group_found) > 1:
                        sums = {col: measures["df"][col].notnull().sum() for col in group_found}
                        if len(set(sums.values())) > 1:
                            self.logger.error(
                                f"Inconsistent non-null counts among columns in group {group}: {sums}"
                            )
                    # A prescindere, aggiungiamo tutte quelle trovate
                    test_type_cols_found.update(group_found)

            # =====================
            # 6) Unione: strict mandatory + test_type_cols_found
            # =====================
            # Ora vogliamo che TUTTE le colonne “strict mandatory” e
            # quelle effettivamente trovate in MANDATORY_TEST_TYPE_SPECIFIC
            # abbiano la stessa lunghezza.
            for col in test_type_cols_found:
                if col not in strict_mandatory_lengths:
                    strict_mandatory_lengths[col] = measures["df"][col].notnull().sum()

            if len(strict_mandatory_lengths) > 1:
                unique_counts = set(strict_mandatory_lengths.values())
                if len(unique_counts) > 1:
                    self.logger.error(
                        f"Inconsistent non-null counts among mandatory columns: {strict_mandatory_lengths}"
                    )
            
            for mandatory_cols_pattern in MANDATORY_TEST_TYPE_SPECIFIC[
                (experiment_type, fracture)
            ]:
                for mandatory_col_pattern in mandatory_cols_pattern:
                    mandatory_col_found = list(
                        FilippoExperiment.__grep_matching_columns(
                            mandatory_col_pattern, measures["df"].columns
                        )
                    )
                    if len(mandatory_col_found) != 0:
                        break
                else:
                    self.logger.error(
                        f"mandatory column missing for test type {experiment_type} "
                        f"{'with' if fracture else 'without'} fracture :"
                    )
                    self.logger.error(" or ".join(mandatory_cols_pattern))

            # Check that there are no empty rows
            if measures["df"].isnull().all(axis=1).any():
                self.logger.error("found empty rows")

    def _save_preprocessed_measures(self):
        """
        Save measures to CSV preprocessed files
        """
        self.logger.info("Save measures")
        for measures in self.measures_list:
            with self.logger.indent:
                measures["df"].round(13).to_csv(
                    measures["preprocessed_fp"],
                    index=False,
                )
                self.logger.info(
                    f"saved to {os.path.basename(measures['preprocessed_fp'])}"
                )

    @classmethod
    def __grep_matching_columns(cls, pattern, columns):
        """
        return the list of all columns matching the pattern
        """
        return filter(lambda column: re.match(pattern, column), columns)

    @classmethod
    def __check_int_column(cls, df, column_name):
        """
        Check that all values of a specified column has int values
        """
        try:
            return (
                df[column_name].replace({np.NaN: 0})
                - df[column_name].replace({np.NaN: 0}).astype("int64")
                < 0.000001
            ).all()
        except ValueError:
            return False
        return True

    @classmethod
    def __check_float_column(cls, df, column_name):
        """
        Check that all values of a specified column has float values
        """
        try:
            df[column_name].astype("float64")
        except ValueError:
            return False
        return True

    @classmethod
    def __check_str_column(cls, df, column_name):
        """
        Check that all values of a specified column has str values
        """
        try:
            df[column_name].astype("str")
        except ValueError:
            return False
        return True
    
    @classmethod
    def __check_datetime_column(cls, df, column_name):
        try:
            pd.to_datetime(df[column_name], format="%Y-%m-%d", errors="raise")
            return True
        except Exception:
            return False

    @classmethod
    def __get_val_at(cls, dic, path, default=None):
        """
        return value from dic at multidepth
        path can be
        + a list/tuple of strings that are the successive keys to follow
        + a string that is splitted at ">" char to get a list of strings

        if no value, then return default if provided or raise KeyError
        """
        if type(path) == str:
            keys = path.split(">")
        else:
            keys = path
        try:
            obj = dic
            for key in keys:
                obj = obj[key]
            return obj
        except KeyError:
            if default is not None:
                return default
            else:
                raise

    @classmethod
    def __set_val_at(cls, dic, path, value):
        """
        set value in dic at multidepth
        `path` is as in Experiment.__get_val_at
        """
        if type(path) == str:
            keys = path.split(">")
        else:
            keys = path
        last_key = keys.pop()
        walk_in_dic = dic
        for k in keys:
            walk_in_dic.setdefault(k, {})
            walk_in_dic = walk_in_dic[k]
        walk_in_dic[last_key] = value

    @classmethod
    def __dict_cleanup(cls, dic):
        """
        Recursively clean dictionary
        works the same on Pandas DataFrame
        + keys :
            + strip white + \n
            + remove unexpected "Unnamed: *_level_*"
            + lowercase
        + values :
            + strip white + \n
        """
        for k, v in list(dic.items()):
            new_k = k.strip(" \n").lower()
            if new_k != k:
                dic[new_k] = dic.pop(k)
                k = new_k
            if re.match(r"Unnamed: \d+_level_\d+", k):
                del dic[k]
            elif type(v) == dict:
                FilippoExperiment.__dict_cleanup(v)
            elif type(v) == str:
                dic[k] = v.strip(" \n")

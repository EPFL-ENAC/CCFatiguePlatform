"""
Common functions for preprocessing
"""

import os
import re
import glob
import json
import copy
import argparse
import numpy as np
import pandas as pd

EXPERIMENTS_FOLDER = os.path.abspath(f"{__file__}/../../Data")
RAW_EXPERIMENT_FP_FOLDERS = sorted(
    filter(lambda fp: os.path.isdir(fp), glob.glob(f"{EXPERIMENTS_FOLDER}/raw/TST_*"))
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

    def __init__(self, filename=None):
        self.filename = filename
        self.warning_count = 0
        self.error_count = 0

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
            # self.f_handler.flush()
        print(message, end=end)

    def reset_counts(self):
        self.warning_count = 0
        self.error_count = 0

    def write(self, message="", prefix="", end="\n"):
        indent = " " * self.indent.current_indent
        lines = [f"{indent}{prefix}{line}" for line in message.split("\n")]
        self._write(lines, end)

    def info(self, message="", end="\n"):
        self.write(message=message, prefix="(i): ", end=end)

    def warning(self, message="", end="\n"):
        self.warning_count += 1
        self.write(message=message, prefix="Warning: ", end=end)

    def error(self, message="", end="\n"):
        self.error_count += 1
        self.write(message=message, prefix="ERROR: ", end=end)


class Experiment:
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
            f"{__file__}/../../Data/preprocessed/{self.exp_meta_meta['folder']}"
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
                        + re.search(r"([0-9]+).csv", filename).group(1)
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

        self.logger.info(f"Read {self.exp_meta_meta['raw_xls_fp']} Experiment tab")
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

        def dict_cleanup(dic):
            """
            Recursively clean dictionary
            + keys :
                + strip white + \n
            + values :
                + strip white + \n
                + remove unexpected "Unnamed: *_level_*"
            """
            for k, v in list(dic.items()):
                new_k = k.strip(" \n")
                if new_k != k:
                    dic[new_k] = dic.pop(k)
                    k = new_k
                if re.match(r"Unnamed: \d+_level_\d+", k):
                    del dic[k]
                elif type(v) == dict:
                    dict_cleanup(v)
                elif type(v) == str:
                    dic[k] = v.strip(" \n")

        self.logger.info("Cleanup experiment")
        with self.logger.indent:

            dict_cleanup(self.experiment)

            CASTING = {
                bool: to_bool,
                int: int,
                float: float,
                str: str,
            }

            fixed_experiment = {}

            for constraint in (
                {"path": "General>Laboratory", "type": str},
                {"path": "General>Researcher", "type": str},
                {"path": "General>Date", "type": str},
                {"path": "General>Experiment Type", "type": str},
                {"path": "General>Fracture", "type": bool},
                {"path": "General>Fracture Mode", "type": str},
                {"path": "General>Initial Crack length", "type": float},
                {"path": "General>Fatigue Test Type", "type": str},
                {"path": "General>Measuring Equipment", "type": str},
                {"path": "General>Reliability Level", "type": float},
                {"path": "General>Control mode", "type": str},
                {"path": "Publication>Title", "type": str},
                {"path": "Publication>Author", "type": str},
                {"path": "Publication>Year", "type": int},
                {"path": "Publication>DOI", "type": str},
                {"path": "Publication>Images Repository", "type": str},
                {"path": "Material Type>Fiber Material", "type": str},
                {"path": "Material Type>Fiber Geometry", "type": str},
                {"path": "Material Type>Area Density", "type": float},
                {"path": "Material Type>Resin", "type": str},
                {"path": "Material Type>Hardener", "type": str},
                {"path": "Material Type>Mixing ratio", "type": str},
                {"path": "Geometry>Length", "type": float},
                {"path": "Geometry>Width", "type": float},
                {"path": "Geometry>Thickness", "type": float},
                {"path": "Laminates and Assemblies>Curing Time", "type": float},
                {
                    "path": "Laminates and Assemblies>Curing Temperature",
                    "type": float,
                },
                {"path": "Laminates and Assemblies>Curing Pressure", "type": float},
                {"path": "Laminates and Assemblies>Fiber Content", "type": float},
                {"path": "Laminates and Assemblies>Stacking Sequence", "type": str},
                {"path": "Test condtions>Temperature", "type": float},
                {"path": "Test condtions>Humidity", "type": float},
                {"path": "DIC Analysis>Subset Size", "type": int},
                {"path": "DIC Analysis>Step Size", "type": int},
            ):
                try:
                    try:
                        isnan = np.isnan(
                            Experiment.__get_val_at(self.experiment, constraint["path"])
                        )
                    except TypeError:
                        isnan = False
                    if not isnan:
                        # cast and save expected values
                        Experiment.__set_val_at(
                            fixed_experiment,
                            constraint["path"],
                            CASTING[constraint["type"]](
                                Experiment.__get_val_at(
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
            # Check self.experiment
            # Mandatory fields check
            for col_constraint in (
                {
                    "path": "General>Laboratory",
                    "mandatory": True,
                },
                {
                    "path": "General>Researcher",
                    "mandatory": True,
                },
                {
                    "path": "General>Experiment Type",
                    "mandatory": True,
                },
                {
                    "path": "General>Fracture",
                    "mandatory": True,
                },
                {
                    "path": "General>Fracture Mode",
                    "mandatory": Experiment.__get_val_at(
                        self.experiment,
                        "General>Fracture",
                        False,
                    ),
                    "mandatory_condition": "when fracture",
                },
                {
                    "path": "General>Fatigue Test Type",
                    "mandatory": Experiment.__get_val_at(
                        self.experiment, "General>Experiment Type", ""
                    )
                    == "FA",
                    "mandatory_condition": "when Experiment Type is 'FA'",
                },
                {
                    "path": "General>Control mode",
                    "mandatory": True,
                },
                {
                    "path": "Geometry>Length",
                    "mandatory": True,
                },
                {
                    "path": "Geometry>Width",
                    "mandatory": True,
                },
                {
                    "path": "Geometry>Thickness",
                    "mandatory": True,
                },
            ):
                try:
                    Experiment.__get_val_at(self.experiment, col_constraint["path"])
                except KeyError:
                    if col_constraint["mandatory"]:
                        self.logger.error(
                            "missing mandatory column "
                            f"{col_constraint.get('mandatory_condition', '')}: "
                            f"{col_constraint['path']}"
                        )

            # Fields constraints
            # Date check : YYYY-MM
            if not re.match(
                r"\d{4}-\d{2}",
                Experiment.__get_val_at(self.experiment, "General>Date"),
            ):
                self.logger.error(
                    "Unrecognized Date : "
                    f"'{Experiment.__get_val_at(self.experiment, 'General>Date')}'"
                )

            # Enums check
            for col_constraint in filter(
                lambda constr: constr["check_needed"],
                (
                    {
                        "check_needed": True,
                        "path": "General>Experiment Type",
                        "enum": (
                            "FA",
                            "QS",
                        ),
                    },
                    {
                        "check_needed": Experiment.__get_val_at(
                            self.experiment, "General>Fracture", False
                        ),
                        "path": "General>Fracture Mode",
                        "enum": (
                            "Mode I",
                            "Mode II",
                            "Mode III",
                            "Combined",
                        ),
                    },
                    {
                        "check_needed": Experiment.__get_val_at(
                            self.experiment,
                            "General>Experiment Type",
                            "",
                        )
                        == "FA",
                        "path": "General>Fatigue Test Type",
                        "enum": (
                            "CA",
                            "VA",
                            "BL",
                            "Combined",
                        ),
                    },
                    {
                        "check_needed": True,
                        "path": "General>Control mode",
                        "enum": (
                            "Load Controlled",
                            "Displacement Controlled",
                        ),
                    },
                ),
            ):
                try:
                    val = Experiment.__get_val_at(
                        self.experiment, col_constraint["path"]
                    )
                    if val not in col_constraint["enum"]:
                        self.logger.error(
                            "unrecognized value for column "
                            f"{col_constraint['path']} = '{val}'"
                        )
                except KeyError:
                    pass

            # Type check
            for col_constraint in (
                {
                    "path": "General>Laboratory",
                    "type": str,
                },
                {
                    "path": "General>Researcher",
                    "type": str,
                },
                {
                    "path": "General>Fracture",
                    "type": bool,
                },
                {
                    "path": "General>Initial Crack length",
                    "type": float,
                },
                {
                    "path": "General>Measuring Equipment",
                    "type": str,
                },
                {
                    "path": "General>Reliability Level",
                    "type": float,
                },
                {
                    "path": "Publication>Title",
                    "type": str,
                },
                {
                    "path": "Publication>Author",
                    "type": str,
                },
                {
                    "path": "Publication>Year",
                    "type": int,
                },
                {
                    "path": "Publication>DOI",
                    "type": str,
                },
                {
                    "path": "Publication>Images Repository",
                    "type": str,
                },
                {
                    "path": "Material Type>Fiber Material",
                    "type": str,
                },
                {
                    "path": "Material Type>Fiber Geometry",
                    "type": str,
                },
                {
                    "path": "Material Type>Area Density",
                    "type": float,
                },
                {
                    "path": "Material Type>Resin",
                    "type": str,
                },
                {
                    "path": "Material Type>Hardener",
                    "type": str,
                },
                {
                    "path": "Material Type>Mixing ratio",
                    "type": str,
                },
                {
                    "path": "Geometry>Length",
                    "type": float,
                },
                {
                    "path": "Geometry>Width",
                    "type": float,
                },
                {
                    "path": "Geometry>Thickness",
                    "type": float,
                },
                {
                    "path": "Laminates and Assemblies>Curing Time",
                    "type": float,
                },
                {
                    "path": "Laminates and Assemblies>Curing Temperature",
                    "type": float,
                },
                {
                    "path": "Laminates and Assemblies>Curing Pressure",
                    "type": float,
                },
                {
                    "path": "Laminates and Assemblies>Fiber Content",
                    "type": float,
                },
                {
                    "path": "Laminates and Assemblies>Stacking Sequence",
                    "type": str,
                },
                {
                    "path": "Test condtions>Temperature",
                    "type": float,
                },
                {
                    "path": "Test condtions>Humidity",
                    "type": float,
                },
                {
                    "path": "DIC Analysis>Subset Size",
                    "type": int,
                },
                {
                    "path": "DIC Analysis>Step Size",
                    "type": int,
                },
            ):
                try:
                    val = Experiment.__get_val_at(
                        self.experiment, col_constraint["path"]
                    )
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
                f"saved {self.exp_meta_meta['preprocessed_experiment_json_fp']}"
            )

    def _read_tests(self):
        """
        Read tests metadata from XLS Tests tab
        """
        self.logger.info(f"Read {self.exp_meta_meta['raw_xls_fp']} Tests tab")
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
            EXPECTED_COLS = (
                "Specimen number",
                "Stress Ratio",
                "Maximum Stress",
                "Loading rate",
                "Run-out",
            )

            # Strip spaces and \n on column names
            self.tests.columns = self.tests.columns.str.strip(" \n")

            # Remove unexpected columns
            unexpected_columns = list(set(self.tests.columns) - set(EXPECTED_COLS))
            if len(unexpected_columns) != 0:
                for unexpected_column in unexpected_columns:
                    self.logger.warning(f"drop unexpected column {unexpected_column}")
                self.tests = self.tests.drop(columns=unexpected_columns)

    def _validate_tests(self):
        """
        Validate tests metadata
        """
        self.logger.info("Validate tests")
        with self.logger.indent:
            # Check mandatory columns
            MANDATORY_COLS = (
                "Specimen number",
                "Stress Ratio",
                "Maximum Stress",
                "Loading rate",
                "Run-out",
            )
            for mandatory_col in MANDATORY_COLS:
                if mandatory_col not in self.tests.columns:
                    self.logger.error(f"mandatory column '{mandatory_col}' not found")

    def _save_preprocessed_tests(self):
        """
        Save tests metadata to CSV preprocessed file
        """
        self.logger.info("Save tests preprocessed")
        with self.logger.indent:
            self.tests.to_csv(
                self.exp_meta_meta["preprocessed_tests_csv_fp"], index=False
            )
            self.logger.info(f"saved {self.exp_meta_meta['preprocessed_tests_csv_fp']}")

    def _read_measures(self):
        """
        Read measures from CSV files
        """
        self.measures_list = copy.deepcopy(self.exp_meta_meta["measures"])
        for measures in self.measures_list:
            self.logger.info(f"Read measures {measures['raw_fp']}")
            with self.logger.indent:
                measures["df"] = pd.read_csv(measures["raw_fp"], low_memory=False)

    def _cleanup_measures(self):
        """
        Cleanup measures
        + small typos fixed
        + only expected columns
        """
        for measures in self.measures_list:
            self.logger.info(f"Cleanup measures {measures['raw_fp']}")
            with self.logger.indent:

                # Strip spaces and \n on column names
                measures["df"].columns = measures["df"].columns.str.strip(" \n")
                EXPECTED_COLUMNS = (
                    "Machine_Time",
                    "Machine_N_cycles",
                    "Machine_Displacement",
                    "Machine_Load",
                    r"MD_index--\d",
                    r"MD_N_cycles--\d",
                    r"MD_Displacement--\d",
                    r"MD_Load--\d",
                    r"u--\d",
                    r"v--\d",
                    r"exx--\d",
                    r"eyy--\d",
                    r"exy--\d",
                    "Crack_length",
                    "Crack_N_cycles",
                    "Crack_Displacement",
                    "Crack_Load",
                    "Th_time",
                    "Th_N_cycles",
                    "Th_specimen_max",
                    "Th_specimen_mean",
                    "Th_chamber",
                    "Th_uppergrips",
                    "Th_lowergrips",
                    r"T--\d",
                    "Storage_modulus",
                    "Tan_delta",
                    "Specimen_name",
                )

                found_matching_columns = set()
                for pattern in EXPECTED_COLUMNS:
                    found_matching_columns.update(
                        list(
                            Experiment.__grep_matching_columns(
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
            self.logger.info(f"Validate measures {measures['raw_fp']}")
            with self.logger.indent:
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
                    ("TM", False): [
                        [
                            r"T--\d",
                        ],
                        [
                            "Storage_modulus",
                            "Tan_delta",
                            "Machine_Load",
                            r"MD_Load--\d",
                        ],
                    ],
                    ("TM", True): [
                        [
                            r"T--\d",
                        ],
                        [
                            "Storage_modulus",
                            "Tan_delta",
                            "Machine_Load",
                            r"MD_Load--\d",
                        ],
                    ],
                }
                COLUMN_TYPE_CHECK = {
                    int: Experiment.__check_int_column,
                    float: Experiment.__check_float_column,
                    str: Experiment.__check_str_column,
                }
                TYPE_NAMES = {
                    int: "integer",
                    float: "float",
                    str: "string",
                }

                # Check for mandatory columns
                for mandatory_col_pattern in filter(
                    lambda c: EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS
                ):
                    mandatory_col_found = list(
                        Experiment.__grep_matching_columns(
                            mandatory_col_pattern, measures["df"].columns
                        )
                    )
                    if len(mandatory_col_found) == 0:
                        self.logger.error(
                            f"mandatory column not found: '{mandatory_col_pattern}'"
                        )
                    else:
                        for mandatory_col in mandatory_col_found:
                            if measures["df"][mandatory_col].isnull().values.any():
                                self.logger.error(
                                    "mandatory column has empty values: "
                                    f"'{mandatory_col}'"
                                )

                # Info for all non-mandatory columns that are not present
                for optional_col_pattern in filter(
                    lambda c: not EXPECTED_COLUMNS[c]["mandatory"],
                    EXPECTED_COLUMNS,
                ):
                    optional_col_found = list(
                        Experiment.__grep_matching_columns(
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

                # Look for type specific mandatory columns
                experiment_type = Experiment.__get_val_at(
                    self.experiment, "General>Experiment Type"
                )
                fracture = Experiment.__get_val_at(self.experiment, "General>Fracture")
                for mandatory_cols_pattern in MANDATORY_TEST_TYPE_SPECIFIC[
                    (experiment_type, fracture)
                ]:
                    for mandatory_col_pattern in mandatory_cols_pattern:
                        mandatory_col_found = list(
                            Experiment.__grep_matching_columns(
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
                measures["df"].to_csv(measures["preprocessed_fp"], index=False)
                self.logger.info(f"saved to {measures['preprocessed_fp']}")

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


def get_tst_folders_to_parse(
    description,
    help=f"Folder containing the TST dataset. "
    "if none provided, then will parse all dataset "
    f"under {EXPERIMENTS_FOLDER}",
):
    """
    Read arguments. If tst_folders are given, then return it.
    Otherwise return default RAW_EXPERIMENT_FP_FOLDERS
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument(
        "tst_folders",
        nargs="*",
        help=help,
    )
    args = parser.parse_args()
    if args.tst_folders != []:
        return args.tst_folders
    else:
        return RAW_EXPERIMENT_FP_FOLDERS

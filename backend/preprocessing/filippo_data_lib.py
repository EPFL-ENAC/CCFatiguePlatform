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
                {"path": "general>qs experiment type", "type": str},
                {"path": "general>fa experiment type", "type": str},
                {"path": "general>ot experiment type", "type": str},
                {"path": "general>ot add info", "type": str},
                {"path": "general>fracture mode fm", "type": str},
                {"path": "general>fm add info", "type": str},                
                {"path": "general>control mode", "type": str},
                {"path": "general>fatigue control mode fcm", "type": str},
                {"path": "general>fcm add info", "type": str},
                {"path": "general>measuring equipment", "type": str},
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
                # FATIGUE SECTION
                {"path": "fatigue>r ratio", "type": float},
                {"path": "fatigue>frequency", "type": float},
                {"path": "fatigue>loading rate", "type": float},
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

            # Converte il dizionario flat in una struttura annidata
            def nest_dict(flat_dict):
                nested = {}
                for compound_key, value in flat_dict.items():
                    keys = compound_key.split(">")
                    d = nested
                    for key in keys[:-1]:
                        key = key.strip()  # Rimuove spazi indesiderati
                        if key not in d:
                            d[key] = {}
                        d = d[key]
                    d[keys[-1].strip()] = value
                return nested

            self.experiment = nest_dict(fixed_experiment)

    def _validate_experiment(self):
        """
        Validate experiment constraints metadata
        """
        self.logger.info("Validate experiment")
        with self.logger.indent:
            # Stampo l'intera struttura dell'esperimento
            import json
            self.logger.info("Experiment structure after cleanup:")
            self.logger.info(json.dumps(self.experiment, indent=2, ensure_ascii=False))

            # Normalizza il valore di experiment type per evitare problemi di spazi o case-sensitive
            experiment_type_raw = FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "")
            experiment_type = experiment_type_raw.strip().upper()
            self.logger.info(f"Normalized experiment type: '{experiment_type}' (raw: '{experiment_type_raw}')")

            # Se l'experiment type è FA, controlla i campi specifici
            if experiment_type == "FA":
                general_section = self.experiment.get("general", {})
                if "fa experiment type" not in general_section:
                    self.logger.error("Missing mandatory column 'general>fa experiment type' (experiment type is 'FA')")
                else:
                    self.logger.info(f"Found 'general>fa experiment type': {general_section['fa experiment type']}")

                if "fatigue control mode fcm" not in general_section:
                    self.logger.error("Missing mandatory column 'general>fatigue control mode fcm' (experiment type is 'FA')")
                else:
                    self.logger.info(f"Found 'general>fatigue control mode fcm': {general_section['fatigue control mode fcm']}")

                fatigue_section = self.experiment.get("fatigue", {})
                if "r ratio" not in fatigue_section:
                    self.logger.error("Missing mandatory column 'fatigue>r ratio' (experiment type is 'FA')")
                else:
                    self.logger.info(f"Found 'fatigue>r ratio': {fatigue_section['r ratio']}")
            else:
                self.logger.info("Experiment type is not 'FA'; specific FA checks skipped.")

            # Block 1: Mandatory Fields Check
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
                    "path": "general>fracture mode fm",
                    "mandatory": (
                        FilippoExperiment.__get_val_at(self.experiment, "general>qs experiment type", "") == "fracture"
                        or FilippoExperiment.__get_val_at(self.experiment, "general>fa experiment type", "") == "fracture"
                    ),
                    "mandatory_condition": "when quasi-static test type or fatigue test type is 'fracture'",
                },
                {
                    "path": "general>fa experiment type",
                    "mandatory": FilippoExperiment.__get_val_at(
                        self.experiment, "general>experiment type", ""
                    ) == "FA",
                    "mandatory_condition": "when experiment type is 'FA'",
                },
                {
                    "path": "general>fatigue control mode fcm",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA",
                    "mandatory_condition": "quando experiment type è 'FA'",
                },
                {
                    "path": "fatigue>r ratio",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA",
                    "mandatory_condition": "when experiment type is 'FA'",
                },
                {
                    "path": "fatigue>frequency",
                    "mandatory": (FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA") 
                                and (not self.experiment.get("fatigue", {}).get("loading rate")),
                    "mandatory_condition": "when experiment type is 'FA' and 'fatigue>loading rate' is not provided",
                },
                {
                    "path": "fatigue>loading rate",
                    "mandatory": (FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "FA") 
                                and (not self.experiment.get("fatigue", {}).get("frequency")),
                    "mandatory_condition": "when experiment type is 'FA' and 'fatigue>frequency' is not provided",
                },
                {
                    "path": "general>qs experiment type",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "QS",
                    "mandatory_condition": "when experiment type is 'QS'",
                },
                {
                    "path": "general>ot experiment type",
                    "mandatory": FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "") == "OT",
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
            if not re.match(
                r"\d{4}-\d{2}",
                FilippoExperiment.__get_val_at(self.experiment, "general>date"),
            ):
                self.logger.error(
                    "Unrecognized Date : "
                    f"'{FilippoExperiment.__get_val_at(self.experiment, 'general>date')}'"
                )

            # Block 3: Enum Values Check
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
                            FilippoExperiment.__get_val_at(self.experiment, "general>qs experiment type", "") == "fracture"
                            or FilippoExperiment.__get_val_at(self.experiment, "general>fa experiment type", "") == "fracture"
                        ),
                        "path": "general>fracture mode",
                        "enum": (
                            "Mode I",
                            "Mode II",
                            "Mode III",
                            "Mixed-Mode",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "FA",
                        "path": "general>fatigue control mode fcm",
                        "enum": (
                            "CA",
                            "VA",
                            "BL",
                            "Other/NA",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "FA",
                        "path": "general>fa experiment type",
                        "enum": (
                            "tensile",
                            "compressive",
                            "shear",
                            "bending",
                            "fracture",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "QS",
                        "path": "general>qs experiment type",
                        "enum": (
                            "tensile",
                            "compressive",
                            "shear",
                            "bending",
                            "fracture",
                        ),
                    },
                    {
                        "check_needed": FilippoExperiment.__get_val_at(
                            self.experiment,
                            "general>experiment type",
                            "",
                        )
                        == "OT",
                        "path": "general>ot experiment type",
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
                            "Strain controlled",
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

            # Block 4: Additional Info Check for 'Other/NA'
            # Check if, for OT and FCM, the 'add info' field is provided when necessary.
            if self.experiment.get("general", {}).get("ot experiment type") == "Other/NA":
                if not self.experiment.get("general", {}).get("ot add info", "").strip():
                    self.logger.error(
                        "The field 'general>ot add info' is mandatory when 'general>ot experiment type' is 'Other/NA'."
                    )

            if self.experiment.get("general", {}).get("fatigue control mode fcm") == "Other/NA":
                if not self.experiment.get("general", {}).get("fcm add info", "").strip():
                    self.logger.error(
                        "The field 'general>fcm add info' is mandatory when 'general>fatigue control mode fcm' is 'Other/NA'."
                    )

            # Block 6: Type Check
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
                {
                    "path": "fatigue>r ratio",
                    "type": float,
                },
                {
                    "path": "fatigue>frequency",
                    "type": float,
                },
                {
                    "path": "fatigue>loading rate",
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
            # Store the original tests DataFrame in tests_df
            self.tests_df = pd.read_excel(
                self.exp_meta_meta["raw_xls_fp"], sheet_name="Tests"
            )
            # Also keep self.tests for later processing (cleanup/validation)
            self.tests = self.tests_df.copy()

    def _cleanup_tests(self):
        """
        Cleanup tests metadata
        + small typos fixed
        + only expected fields
        + only non-empty fields (null/NaN)
        + fields casted as bool / float / int / str
        """
        self.logger.info("Cleanup tests")
        with self.logger.indent:

            # Logga struttura originale dei dati
            self.logger.info("Tests structure BEFORE cleanup:")
            try:
                import json
                if isinstance(self.tests, pd.DataFrame):
                    self.logger.info(json.dumps(self.tests.to_dict(orient="list"), indent=2, ensure_ascii=False))
                else:
                    self.logger.error("self.tests is not a DataFrame; cannot call to_dict.")
            except Exception as e:
                if isinstance(self.tests, dict):
                    self.logger.info(json.dumps(self.tests, indent=2, ensure_ascii=False))
                else:
                    self.logger.error("self.tests is not a dictionary; cannot process.")
                self.logger.error(f"Cannot print raw tests: {e}")

            def to_bool(value):
                if pd.isna(value):  # evita 'NaN' in input
                    raise ValueError("Cannot convert NaN to bool")
                if isinstance(value, bool):
                    return value
                if isinstance(value, str):
                    val = value.strip().lower()
                    if val.startswith("y") or val == "1":
                        return True
                    elif val.startswith("n") or val == "0":
                        return False
                raise ValueError(f"Cannot convert '{value}' to bool")

            CASTING = {
                bool: to_bool,
                int: int,
                float: float,
                str: str,
            }

            expected_constraints = (
                {"path": "sequential number", "type": int},
                {"path": "specimen name", "type": str},
                {"path": "number of cycles", "type": int},
                {"path": "maximum load", "type": float},
                {"path": "run out", "type": bool},
                {"path": "length", "type": float},
                {"path": "width", "type": float},
                {"path": "thickness", "type": float},
                {"path": "temperature", "type": float},
                {"path": "humidity", "type": float},
                {"path": "initial crack length", "type": float},
                {"path": "subset size", "type": float},
                {"path": "step size", "type": float},
            )

            fixed_tests = {}

            # Normalizza colonne
            if isinstance(self.tests, pd.DataFrame):
                self.tests.columns = self.tests.columns.str.strip().str.lower()
            else:
                self.logger.error("self.tests is not a DataFrame; cannot process columns.")

            for constraint in expected_constraints:
                col = constraint["path"]
                col_type = constraint["type"]
                try:
                    if col in self.tests:
                        series = self.tests[col]
                        # Filtra valori validi (non-NaN)
                        valid_values = [v for v in series if not pd.isna(v)]

                        if not valid_values:
                            continue  # niente da settare

                        casted_val = CASTING[col_type](valid_values[0])
                        FilippoExperiment.__set_val_at(fixed_tests, col, casted_val)

                except Exception as e:
                    self.logger.error(f"exception while processing column '{col}': {e}")

            # Converte in struttura annidata
            def nest_dict(flat_dict):
                nested = {}
                for compound_key, value in flat_dict.items():
                    keys = compound_key.split(">")
                    d = nested
                    for key in keys[:-1]:
                        key = key.strip()
                        if key not in d:
                            d[key] = {}
                        d = d[key]
                    d[keys[-1].strip()] = value
                return nested

            self.tests = nest_dict(fixed_tests)

            # Logga struttura finale dei test
            self.logger.info("Tests structure AFTER cleanup:")
            try:
                self.logger.info(json.dumps(self.tests, indent=2, ensure_ascii=False))
            except Exception as e:
                self.logger.error(f"Cannot print cleaned test structure: {e}")

    def _validate_tests(self):
        """
        Validate tests metadata.
        """
        self.logger.info("Validate tests")
        with self.logger.indent:
            import json
            self.logger.info("Tests structure after cleanup:")
            try:
                self.logger.info(json.dumps(self.tests, indent=2, ensure_ascii=False))
            except Exception as e:
                self.logger.error(f"Failed to print test structure: {e}")
            
            if hasattr(self, 'tests_df'):
                num_tests_lines = self.tests_df.shape[0]
                num_csv_files = len(self.exp_meta_meta["measures"])
                if num_tests_lines != num_csv_files:
                    self.logger.error(
                        f"Mismatch: Number of tests sheet lines ({num_tests_lines}) does not match number of CSV files ({num_csv_files})."
                    )
                else:
                    self.logger.info("The number of tests sheet lines matches the number of CSV files.")
            else:
                self.logger.error("tests_df is not available to perform the CSV count check.")

            # Recupera il tipo di test dall'esperimento
            test_type_raw = FilippoExperiment.__get_val_at(self.experiment, "general>experiment type", "")
            test_type = test_type_raw.strip().upper()
            self.logger.info(f"Normalized test type: '{test_type}' (raw: '{test_type_raw}')")

            fa_type = FilippoExperiment.__get_val_at(self.experiment, "general>fa experiment type", "").strip().lower()
            print(f"fa_type: {fa_type}")
            qs_type = FilippoExperiment.__get_val_at(self.experiment, "general>qs experiment type", "").strip().lower()
            is_fracture = fa_type == "fracture" or qs_type == "fracture"

        # Blocco 1: Verifica campi obbligatori
        for col_constraint in (
            {"path": "sequential number", "mandatory": True},
            {"path": "specimen name", "mandatory": True},
            {"path": "width", "mandatory": True},
            {"path": "thickness", "mandatory": True},
            {
                "path": "number of cycles",
                "mandatory": test_type == "FA",
                "mandatory_condition": "when test type is FA",
            },
            {
                "path": "run out",
                "mandatory": test_type == "FA",
                "mandatory_condition": "when test type is FA",
            },
            {
                "path": "maximum load",
                "mandatory": test_type == "FA",
                "mandatory_condition": "when test type is FA",
            },
            {
                "path": "initial crack length",
                "mandatory": is_fracture,
                "mandatory_condition": "when fa or qs type is fracture",
            },
        ):
            try:
                val = FilippoExperiment.__get_val_at(self.tests, col_constraint["path"])
                # Se è NaN o stringa vuota
                if val is None or (isinstance(val, float) and np.isnan(val)) or (isinstance(val, str) and val.strip() == ""):
                    if col_constraint["mandatory"]:
                        self.logger.error(
                            f"Missing mandatory column or value {col_constraint.get('mandatory_condition', '')}: '{col_constraint['path']}'"
                        )
            except KeyError:
                if col_constraint["mandatory"]:
                    self.logger.error(
                        f"Missing mandatory column {col_constraint.get('mandatory_condition', '')}: '{col_constraint['path']}'"
                    )

            # Blocco 2: Verifica tipi di dati
            for col_constraint in (
                {"path": "sequential number", "type": int},
                {"path": "specimen name", "type": str},
                {"path": "number of cycles", "type": int},
                {"path": "maximum load", "type": float},
                {"path": "run out", "type": bool},
                {"path": "length", "type": float},
                {"path": "width", "type": float},
                {"path": "thickness", "type": float},
                {"path": "temperature", "type": float},
                {"path": "humidity", "type": float},
                {"path": "initial crack length", "type": float},
                {"path": "subset size", "type": float},
                {"path": "step size", "type": float},
            ):
                try:
                    val = FilippoExperiment.__get_val_at(self.tests, col_constraint["path"])
                    expected_type = col_constraint["type"]

                    if not isinstance(val, expected_type):
                        self.logger.error(
                            f"Wrong type for column '{col_constraint['path']}': {val} (expected {expected_type.__name__})"
                        )
                except KeyError:
                    # già gestito sopra come campo mancante
                    pass


    def _save_preprocessed_tests(self):
        """
        Save tests metadata to CSV preprocessed file
        """
        self.logger.info("Save tests preprocessed")
        with self.logger.indent:
            if isinstance(self.tests, pd.DataFrame):
                self.tests.to_csv(
                    self.exp_meta_meta["preprocessed_tests_csv_fp"],
                    index=False,
                )
            else:
                self.logger.error("self.tests is not a DataFrame; cannot save to CSV.")
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
                        "exx",
                        "eyy", 
                        "exy",
                        "Crack_length",
                        "Specimen_name",
                        "N_cycles",
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
            self.logger.info(f"Validate measures {os.path.basename(measures['raw_fp'])}")
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
                }

                RELAXED_MANDATORY_COLUMNS = {"Specimen_name", "Test_Date"}

                # Recupera i valori di test type per FA e QS
                fa_experiment_type = FilippoExperiment.__get_val_at(
                    self.experiment, "general>fa experiment type", ""
                )
                qs_experiment_type = FilippoExperiment.__get_val_at(
                    self.experiment, "general>qs experiment type", ""
                )

                # Definisce le regole per FA in base alla selezione "fracture"
                if fa_experiment_type == "fracture":
                    mandatory_fa = [
                        ["N_cycles"],       # "N_cycles" deve essere presente
                        ["Crack_length"],   # e, in aggiunta, "Crack_length" diventa obbligatoria
                    ]
                else:
                    mandatory_fa = [
                        ["N_cycles"],
                    ]

                # Definisce le regole per QS in base alla selezione "fracture"
                if qs_experiment_type == "fracture":
                    mandatory_qs = [
                        ["Crack_length"],   # "Crack_length" obbligatoria
                        ["u", "exx"],       # e almeno una tra "u" o "exx"
                    ]
                else:
                    mandatory_qs = [
                        ["u", "exx"],
                    ]

                # Raggruppa le regole specifiche per test type
                MANDATORY_TEST_TYPE_SPECIFIC = {
                    "FA": mandatory_fa,
                    "QS": mandatory_qs,
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

            # --- Controllo presenza colonne mandatory da EXPECTED_COLUMNS
            for mandatory_col_pattern in filter(lambda c: EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS):
                mandatory_col_found = list(FilippoExperiment.__grep_matching_columns(mandatory_col_pattern, measures["df"].columns))
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

            # --- Controllo lunghezza coerente per le colonne obbligatorie "strict"
            strict_mandatory_lengths = {}
            for col_pattern in filter(
                lambda c: EXPECTED_COLUMNS[c]["mandatory"] and c not in RELAXED_MANDATORY_COLUMNS,
                EXPECTED_COLUMNS
            ):
                found_cols = list(FilippoExperiment.__grep_matching_columns(col_pattern, measures["df"].columns))
                for col in found_cols:
                    strict_mandatory_lengths[col] = measures["df"][col].notnull().sum()

            # --- Informazioni per le colonne opzionali non trovate
            for optional_col_pattern in filter(lambda c: not EXPECTED_COLUMNS[c]["mandatory"], EXPECTED_COLUMNS):
                optional_col_found = list(FilippoExperiment.__grep_matching_columns(optional_col_pattern, measures["df"].columns))
                if len(optional_col_found) == 0:
                    self.logger.info(f"optional column not found: '{optional_col_pattern}'")

            # --- Controllo tipo dati per ogni colonna
            for col in measures["df"].columns:
                for col_pattern in EXPECTED_COLUMNS:
                    if re.match(col_pattern, col):
                        expected_type = EXPECTED_COLUMNS[col_pattern]["type"]
                        if not COLUMN_TYPE_CHECK[expected_type](measures["df"], col):
                            self.logger.error(
                                f"column '{col}' is expected to be of type '{TYPE_NAMES[expected_type]}'"
                            )

            # --- Controllo formato della data in Test_Date
            if "Test_Date" in measures["df"].columns:
                for date_val in measures["df"]["Test_Date"].dropna().unique():
                    if not re.match(r"^\d{4}-\d{2}", str(date_val)):
                        self.logger.error(f"Unrecognized Test_Date: '{date_val}'")

            # --- Validazione specifica per test type (FA o QS) con logica "fracture"
            try:
                experiment_type = FilippoExperiment.__get_val_at(self.experiment, "general>experiment type")
            except KeyError:
                self.logger.error(
                    "XLS file seems broken. Please double check it follows "
                    f"template provided here: {XLS_TEMPLATE_URL}"
                )
                return

            # Determina se il test è in modalità fracture in base al campo specifico
            if experiment_type == "FA":
                is_fracture = (fa_experiment_type == "fracture")
                groups = MANDATORY_TEST_TYPE_SPECIFIC.get("FA", [])
            elif experiment_type == "QS":
                is_fracture = (qs_experiment_type == "fracture")
                groups = MANDATORY_TEST_TYPE_SPECIFIC.get("QS", [])
            else:
                groups = []
                is_fracture = False

            test_type_cols_found = set()
            for group in groups:
                group_found = []
                for pattern in group:
                    group_found.extend(
                        list(FilippoExperiment.__grep_matching_columns(pattern, measures["df"].columns))
                    )
                if len(group_found) == 0:
                    self.logger.error(
                        f"mandatory column missing for test type {experiment_type} " +
                        ("with fracture:" if is_fracture else "without fracture:")
                    )
                    self.logger.error(" or ".join(group))
                else:
                    if len(group_found) > 1:
                        sums = {col: measures["df"][col].notnull().sum() for col in group_found}
                        if len(set(sums.values())) > 1:
                            self.logger.error(
                                f"Inconsistent non-null counts among columns in group {group}: {sums}"
                            )
                    test_type_cols_found.update(group_found)

            # --- Unione: strict mandatory + colonne trovate per test type
            for col in test_type_cols_found:
                if col not in strict_mandatory_lengths:
                    strict_mandatory_lengths[col] = measures["df"][col].notnull().sum()

            if len(strict_mandatory_lengths) > 1:
                unique_counts = set(strict_mandatory_lengths.values())
                if len(unique_counts) > 1:
                    self.logger.error(
                        f"Inconsistent non-null counts among mandatory columns: {strict_mandatory_lengths}"
                    )

            # --- Controllo che non ci siano righe completamente vuote
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

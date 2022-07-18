"""
Common functions for preprocessing
"""

import os
import re
import glob
import argparse
import numpy as np


EXPERIMENTS_FOLDER = os.path.abspath(f"{__file__}/../../Data")
EXPERIMENT_FP_FOLDERS = sorted(
    filter(lambda fp: os.path.isdir(fp), glob.glob(f"{EXPERIMENTS_FOLDER}/TST_*"))
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

    def write(self, message="", prefix="", end="\n"):
        indent = " " * self.indent.current_indent
        lines = [f"{indent}{prefix}{line}" for line in message.split("\n")]
        self._write(lines, end)

    def info(self, message="", end="\n"):
        self.write(message=message, prefix="(i): ", end=end)

    def warning(self, message="", end="\n"):
        self.write(message=message, prefix="Warning: ", end=end)

    def error(self, message="", end="\n"):
        self.write(message=message, prefix="ERROR: ", end=end)


def get_tst_folders_to_parse(
    description,
    help=f"Folder containing the TST dataset. "
    "if none provided, then will parse all dataset "
    f"under {EXPERIMENTS_FOLDER}",
):
    """
    Read arguments. If tst_folders are given, then return it.
    Otherwise return default EXPERIMENT_FP_FOLDERS
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
        return EXPERIMENT_FP_FOLDERS


def check_int_column(df, column_name):
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


def check_float_column(df, column_name):
    """
    Check that all values of a specified column has float values
    """
    try:
        df[column_name].astype("float64")
    except ValueError:
        return False
    return True


def check_str_column(df, column_name):
    """
    Check that all values of a specified column has str values
    """
    try:
        df[column_name].astype("str")
    except ValueError:
        return False
    return True


def check_exist_in_dict(dic, keys):
    try:
        obj = dic
        for key in keys:
            obj = obj[key]
        return True
    except KeyError:
        return False


def check_str_in_dict(dic, keys):
    try:
        obj = dic
        for key in keys:
            obj = obj[key]
        return type(obj) == str
    except KeyError:
        return False


def check_bool_in_dict(dic, keys):
    try:
        obj = dic
        for key in keys:
            obj = obj[key]
        return type(obj) == bool
    except KeyError:
        return False


def get_experiment_meta(experiment_fp_folder):
    """
    return following dict
    {
        "folder": folder basename
        "researcher_last_name": experiment's researcher_last_name
        "date": experiment's date
        "test_type": experiment's test_type
        "metadata_xls_fp": full path to metadata xls(x)
        "csv_fps": full path list to csv files
        "metadata_json_fp": full path to experiment metadata json (produced from xls)
        "metadata_csv_fp": full path to test metadata csv (produced from xls)
    }
    """

    experiment = {}
    experiment["folder"] = os.path.basename(experiment_fp_folder)
    experiment["preprocessed_folder"] = os.path.join(
        experiment_fp_folder, "preprocessed"
    )
    m = re.match("TST_([^_]+)_([0-9-]+)_(FA|QS)", experiment["folder"])
    if not m:
        raise NotAnExperimentFolder(experiment_fp_folder)
    (
        experiment["researcher_last_name"],
        experiment["date"],
        experiment["test_type"],
    ) = m.groups()

    # search metadata XLS(X) file
    filename_without_ext = (
        f"TST_{experiment['date']}_{experiment['test_type']}_metadata"
    )
    for extension in ("xls", "xlsx"):
        metadata_file = os.path.join(
            experiment_fp_folder,
            f"{filename_without_ext}.{extension}",
        )
        if os.path.exists(metadata_file):
            experiment["metadata_xls_fp"] = metadata_file
            experiment["metadata_json_fp"] = os.path.join(
                experiment["preprocessed_folder"], f"{filename_without_ext}.json"
            )
            experiment["metadata_csv_fp"] = os.path.join(
                experiment["preprocessed_folder"], f"{filename_without_ext}.csv"
            )
            break
    else:
        experiment["metadata_xls_fp"] = None
        experiment["metadata_json_fp"] = None
        experiment["metadata_csv_fp"] = None

    # search CSV files
    experiment["csv_fps"] = sorted(
        filter(
            lambda filename: re.search(
                f"TST_{experiment['date']}_{experiment['test_type']}_([0-9]+).csv",
                filename,
            ),
            glob.glob(
                os.path.join(
                    experiment_fp_folder,
                    f"TST_{experiment['date']}_{experiment['test_type']}_*.csv",
                )
            ),
        )
    )

    return experiment

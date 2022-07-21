"""
+ read `xls(x)` metadata file
+ extract expected field
+ cast them
+ and save it to `json` and `csv` files
"""

import os
import re
import numpy as np
import pandas as pd
import simplejson as json
import common


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


def df_to_nested_dict(df: pd.DataFrame) -> dict:
    """
    Translate df to dict
    """
    d = df.to_dict(orient="index")
    return {k: nest(v) for k, v in d.items()}


def dict_typo_fix(dic):
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
            dict_typo_fix(v)
        elif type(v) == str:
            dic[k] = v.strip(" \n")


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


def get_experiment_meta_fixed(meta):
    """
    return approved dict with
        + only expected fields
        + only non-empty fields
        + fields casted as bool / float (double) / int / str
    """
    CASTING = {
        bool: to_bool,
        int: int,
        float: float,
        str: str,
    }

    fixed_meta = {}

    # Booleans
    for constraint in (
        {"path": "Experiment>General>Laboratory", "type": str},
        {"path": "Experiment>General>Researcher", "type": str},
        {"path": "Experiment>General>Date", "type": str},
        {"path": "Experiment>General>Experiment Type", "type": str},
        {"path": "Experiment>General>Fracture", "type": bool},
        {"path": "Experiment>General>Fracture Mode", "type": str},
        {"path": "Experiment>General>Initial Crack length", "type": float},
        {"path": "Experiment>General>Fatigue Test Type", "type": str},
        {"path": "Experiment>General>Measuring Equipment", "type": str},
        {"path": "Experiment>General>Reliability Level", "type": float},
        {"path": "Experiment>General>Control mode", "type": str},
        {"path": "Experiment>Publication>Title", "type": str},
        {"path": "Experiment>Publication>Author", "type": str},
        {"path": "Experiment>Publication>Year", "type": int},
        {"path": "Experiment>Publication>DOI", "type": str},
        {"path": "Experiment>Publication>Images Repository", "type": str},
        {"path": "Experiment>Material Type>Fiber Material", "type": str},
        {"path": "Experiment>Material Type>Fiber Geometry", "type": str},
        {"path": "Experiment>Material Type>Area Density", "type": float},
        {"path": "Experiment>Material Type>Resin", "type": str},
        {"path": "Experiment>Material Type>Hardener", "type": str},
        {"path": "Experiment>Material Type>Mixing ratio", "type": str},
        {"path": "Experiment>Geometry>Length", "type": float},
        {"path": "Experiment>Geometry>Width", "type": float},
        {"path": "Experiment>Geometry>Thickness", "type": float},
        {"path": "Experiment>Laminates and Assemblies>Curing Time", "type": float},
        {
            "path": "Experiment>Laminates and Assemblies>Curing Temperature",
            "type": float,
        },
        {"path": "Experiment>Laminates and Assemblies>Curing Pressure", "type": float},
        {"path": "Experiment>Laminates and Assemblies>Fiber Content", "type": float},
        {"path": "Experiment>Laminates and Assemblies>Stacking Sequence", "type": str},
        {"path": "Experiment>Test condtions>Temperature", "type": float},
        {"path": "Experiment>Test condtions>Humidity", "type": float},
        {"path": "Experiment>DIC Analysis>Subset Size", "type": int},
        {"path": "Experiment>DIC Analysis>Step Size", "type": int},
    ):
        try:
            try:
                isnan = np.isnan(common.get_val_at(meta, constraint["path"]))
            except TypeError:
                isnan = False
            if not isnan:
                # cast and save expected values
                common.set_val_at(
                    fixed_meta,
                    constraint["path"],
                    CASTING[constraint["type"]](
                        common.get_val_at(meta, constraint["path"])
                    ),
                )
        except KeyError:
            pass
        except ValueError as e:
            logger.error(f"exception: {e}")
    return fixed_meta


def get_tests_meta_fixed(tests_df):
    """
    + Fix typos in column names
    + Error msgs when incompatibility with Convention
    """
    MANDATORY_COLS = (
        "Specimen number",
        "Stress Ratio",
        "Maximum Stress",
        "Loading rate",
        "Run-out",
    )

    # Strip spaces and \n on column names
    tests_df.columns = tests_df.columns.str.strip(" \n")
    found_columns = tests_df.columns

    # Check mandatory columns
    for mandatory_col in MANDATORY_COLS:
        if mandatory_col not in found_columns:
            logger.error(f"mandatory column '{mandatory_col}' not found")

    # Remove unexpected columns
    unexpected_columns = list(set(found_columns) - set(MANDATORY_COLS))
    if len(unexpected_columns) != 0:
        logger.warning(f"drop unexpected columns {unexpected_columns}")
        tests_df = tests_df.drop(columns=unexpected_columns)

    return tests_df


if __name__ == "__main__":
    with common.Logger(None) as logger:
        for experiment_fp_folder in common.get_tst_folders_to_parse(
            description="Transform TST's CSV to JSON"
        ):
            logger.info(f"Processing {experiment_fp_folder} Experiment tab")
            with logger.indent:
                exp = common.get_experiment_meta(experiment_fp_folder)
                if exp["metadata_xls_fp"] is None:
                    logger.error("metadata file not found !")
                    continue

                # Experiment tab to JSON metadata
                # - - - - - - - - - - - - - - - -
                experiment_df = pd.read_excel(
                    exp["metadata_xls_fp"],
                    sheet_name="Experiment",
                    header=[0, 1],
                )
                experiment_dict = {}
                experiment_dict["Experiment"] = df_to_nested_dict(experiment_df)[0]

                # Cleanup
                dict_typo_fix(experiment_dict)
                experiment_dict = get_experiment_meta_fixed(experiment_dict)

                os.makedirs(exp["preprocessed_folder"], exist_ok=True)
                with open(exp["metadata_json_fp"], "w") as f:
                    json.dump(experiment_dict, f, ignore_nan=True, indent=2)
                logger.info(f"saved {exp['metadata_json_fp']}")

            logger.info(f"Processing {experiment_fp_folder} Tests tab")
            with logger.indent:
                # Tests tab to CSV metadata
                # - - - - - - - - - - - - -
                tests_df = pd.read_excel(exp["metadata_xls_fp"], sheet_name="Tests")

                # Cleanup
                tests_df = get_tests_meta_fixed(tests_df)

                tests_df.to_csv(exp["metadata_csv_fp"], index=False)
                logger.info(f"saved {exp['metadata_csv_fp']}")

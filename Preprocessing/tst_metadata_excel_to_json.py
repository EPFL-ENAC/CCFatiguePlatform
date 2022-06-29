"""
Writes TST metadata JSON and CSV files from TST metadata Excel file.
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
    if value.lower().startswith("y"):
        return True
    elif value.lower().startswith("n"):
        return False
    else:
        raise ValueError


def meta_dict_fix_types(meta):
    """
    Turn expected fields into bool / float (double) / int
    """
    # Booleans
    for (k1, k2, k3) in (("Experiment", "General", "Fracture"),):
        try:
            meta[k1][k2][k3] = to_bool(meta[k1][k2][k3])
        except KeyError:
            pass
        except ValueError as e:
            logger.error(f"exception: {e}")

    # Double
    for (k1, k2, k3) in (
        ("Experiment", "General", "Initial Crack length"),
        ("Experiment", "General", "Reliability Level"),
        ("Experiment", "Material Type", "Area Density"),
        ("Experiment", "Geometry", "Length"),
        ("Experiment", "Geometry", "Width"),
        ("Experiment", "Geometry", "Thickness"),
        ("Experiment", "Laminates and Assemblies", "Curing Time"),
        ("Experiment", "Laminates and Assemblies", "Curing Temperature"),
        ("Experiment", "Laminates and Assemblies", "Curing Pressure"),
        ("Experiment", "Laminates and Assemblies", "Fiber Content"),
        ("Experiment", "Test condtions", "Temperature"),
        ("Experiment", "Test condtions", "Humidity"),
    ):
        try:
            meta[k1][k2][k3] = float(meta[k1][k2][k3])
        except KeyError:
            pass
        except ValueError as e:
            logger.error(f"exception: {e}")

    # Int
    for (k1, k2, k3) in (
        ("Experiment", "DIC Analysis", "Subset Size"),
        ("Experiment", "DIC Analysis", "Step Size"),
    ):
        try:
            if not np.isnan(meta[k1][k2][k3]):
                meta[k1][k2][k3] = int(meta[k1][k2][k3])
        except KeyError:
            pass
        except ValueError as e:
            logger.error(f"exception: {e}")


if __name__ == "__main__":
    with common.Logger(None) as logger:
        for experiment_fp_folder in common.EXPERIMENT_FP_FOLDERS:
            logger.info(f"Processing {experiment_fp_folder}")
            with logger.indent:
                exp = common.get_experiment_meta(experiment_fp_folder)
                if exp["metadata_xls_fp"] is None:
                    logger.error("metadata file not found !")
                    continue

                # Experiment tab to JSON metadata
                # - - - - - - - - - - - - - - - -
                contents_df = pd.read_excel(
                    exp["metadata_xls_fp"],
                    sheet_name="Experiment",
                    header=[0, 1],
                )
                contents_dict = {}
                contents_dict["Experiment"] = df_to_nested_dict(contents_df)[0]

                # Cleanup
                dict_typo_fix(contents_dict)
                meta_dict_fix_types(contents_dict)

                os.makedirs(exp["preprocessed_folder"], exist_ok=True)
                with open(exp["metadata_json_fp"], "w") as f:
                    json.dump(contents_dict, f, ignore_nan=True, indent=4)
                logger.info(f"saved {exp['metadata_json_fp']}")

                # Tests tab to CSV metadata
                # - - - - - - - - - - - - -
                contents_df = pd.read_excel(exp["metadata_xls_fp"], sheet_name="Tests")
                contents_df.to_csv(exp["metadata_csv_fp"], index=False)
                logger.info(f"saved {exp['metadata_csv_fp']}")

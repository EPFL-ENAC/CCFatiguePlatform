"""
Writes TST metadata JSON and CSV files from TST metadata Excel file.
"""

import os
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
    """
    for k, v in dic.items():
        if type(v) == dict:
            dict_typo_fix(v)
        elif type(v) == str:
            dic[k] = v.strip()


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
                    exp["metadata_xls_fp"], sheet_name="Experiment", header=[0, 1]
                )
                contents_dict = {}
                contents_dict["Experiment"] = df_to_nested_dict(contents_df)[0]

                # Fix typos

                os.makedirs(exp["preprocessed_folder"], exist_ok=True)
                with open(exp["metadata_json_fp"], "w") as f:
                    json.dump(contents_dict, f, ignore_nan=True, indent=4)
                logger.info(f"saved {exp['metadata_json_fp']}")

                # Tests tab to CSV metadata
                # - - - - - - - - - - - - -
                contents_df = pd.read_excel(exp["metadata_xls_fp"], sheet_name="Tests")
                contents_df.to_csv(exp["metadata_csv_fp"], index=False)
                logger.info(f"saved {exp['metadata_csv_fp']}")

"""
Writes TST metadata JSON and CSV files from TST metadata Excel file.
"""

import pandas as pd
import os
import simplejson as json

DATA_DIR = os.path.abspath(f"{__file__}/../../Data")
FILENAMES = [
    "TST_FakeResearcher_2019-09_QS/TST_2019-09_QS_metadata",
    "TST_FakeResearcher_2020-06_FA/TST_2020-06_FA_metadata",
    "TST_FakeResearcher_2021-06_FA/TST_2021-06_FA_metadata",
    "TST_Khalooei_2021-10_FA/TST_2021-10_FA_metadata",
]


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


if __name__ == "__main__":
    for filename in FILENAMES:
        contents_filepath = os.path.join(DATA_DIR, filename + ".xls")
        output_json_filepath = os.path.join(DATA_DIR, filename + ".json")
        output_csv_filepath = os.path.join(DATA_DIR, filename + ".csv")

        # Experiment tab to JSON metadata
        # - - - - - - - - - - - - - - - -
        contents_df = pd.read_excel(
            contents_filepath, sheet_name="Experiment", header=[0, 1]
        )
        contents_dict = {}
        contents_dict["Experiment"] = df_to_nested_dict(contents_df)[0]
        with open(output_json_filepath, "w") as f:
            json.dump(contents_dict, f, ignore_nan=True, indent=4)

        # Tests tab to CSV metadata
        # - - - - - - - - - - - - -
        contents_df = pd.read_excel(contents_filepath, sheet_name="Tests")
        contents_df.to_csv(output_csv_filepath, index=False)

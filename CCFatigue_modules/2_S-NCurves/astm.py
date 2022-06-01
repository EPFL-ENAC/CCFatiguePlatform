#!/usr/bin/env python
"""
Helps reading ASTM csv data
"""

import os
import numpy as np
import pandas as pd


class Astm:
    """Astm helps reading ASTM table"""

    SRC_DIR = os.path.dirname(os.path.realpath(__file__))
    PARAM_DIR = os.path.join(SRC_DIR, "..", "ExternalDataSources")

    def __init__(self, confidence):

        self.PARAM_ASTM_RELIABILITY_LEVEL_FILENAME = f"astm{confidence}.csv"
        self.astm_rl_df = pd.read_csv(
            os.path.join(self.PARAM_DIR, self.PARAM_ASTM_RELIABILITY_LEVEL_FILENAME)
        )

    def get_astm_val(self, degrees_of_freedom_n1, degrees_of_freedom_n2):
        """TODO"""
        if degrees_of_freedom_n1 != np.inf:
            degrees_of_freedom_n1 = round(degrees_of_freedom_n1)

        return float(
            self.astm_rl_df.loc[self.astm_rl_df.n == degrees_of_freedom_n2][
                str(degrees_of_freedom_n1)
            ]
        )

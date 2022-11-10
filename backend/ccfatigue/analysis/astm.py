#!/usr/bin/env python
"""
Helps reading ASTM csv data
CSV data extracted from:
    https://www.astm.org/stp313-eb.html pp. 20-23
    DOI: 10.1520/STP313-EB

"""

import os

import numpy as np
import pandas as pd


class Astm:
    """Astm helps reading ASTM table"""

    SRC_DIR = os.path.dirname(os.path.realpath(__file__))
    PARAM_DIR = os.path.join(SRC_DIR, "external_data_sources")

    def __init__(self, confidence: int):
        """
        Constructor
        Parameters
        ----------
            confidence: int
                confidence={95|99}
        """

        self.PARAM_ASTM_RELIABILITY_LEVEL_FILENAME = f"astm{confidence}.csv"
        self.astm_rl_df = pd.read_csv(
            os.path.join(self.PARAM_DIR, self.PARAM_ASTM_RELIABILITY_LEVEL_FILENAME)
        )

    def get_astm_val(
        self, degrees_of_freedom_n1: float, degrees_of_freedom_n2: float
    ) -> float:
        """
        Return ASTM val corresponding to given degrees of freedom
        Parameters
        ----------
            degrees_of_freedom_n1: float
            degrees_of_freedom_n2: float
        Returns
        -------
            astm_val: float
        """
        if degrees_of_freedom_n1 != np.inf:
            degrees_of_freedom_n1 = round(degrees_of_freedom_n1)

        astm_val = self.astm_rl_df.loc[self.astm_rl_df.n == degrees_of_freedom_n2][
            str(degrees_of_freedom_n1)
        ].values[0]

        return astm_val or 0  # casting Optional[float] to float

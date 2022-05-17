"""
Implementation of the Piecewise Linear Methods as described in:
Anastasios P. Vassilopoulos, Behzad D. Manshadi, Thomas Keller,
Influence of the constant life diagram formulation on the fatigue life prediction of composite materials,
International Journal of Fatigue,
Volume 32, Issue 4, 2010, Pages 659-669, ISSN 0142-1123,
https://doi.org/10.1016/j.ijfatigue.2009.09.008
(Section 2.2)
"""

import os
import pandas as pd
import math
from itertools import chain


SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
## PARAM_DIR = os.path.join(SRC_DIR, "..", "ExternalDataSources")

INPUT_FILENAME = "SNC_input.csv"
INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
OUTPUT_CSV_FILENAME = "CLD_PiecewiseLinear.csv"
OUTPUT_CSV_FILE = os.path.join(DATA_DIR, OUTPUT_CSV_FILENAME)

## Variables def
##
## sigma prime a = ???


def calculate_sigma_prime_a(R, R_ratios):
    """
    Step 1 - Identifiy between whic R ratios R' is located.

    SNC_df = Dataframe with cols:
    - stress_ratio (R): is a vector of known R_ratios.
    - stress_amplitudes (sigma_a) :
    """
    R_1_TT = SNC_df.R_ratios[0]

    if R > 1:
        sigma_prime_a = CC_sector_equation()
    elif R < 0:
        sigma_prime_a = TT_sector_equation()
    elif R == 0 :
        ?? ## What happens when R == 0 or 1 ?
    elif R == 1 :
        ?? ## What happens when R == 0 or 1 ?
    else:
        ## Locate whic Rmin, Rmax known R_ratios R is located between:  TODO - TEST BEHAVIOR !
        for Rmin in R_ratios:
            if R > Rmin:
                pass
        for Rmax in R_ratios:
            if R < Rmax:
                continue

        sigma_prime_a = CT_sector_equation(Rmin, Rmax)



def TT_sector_equation(R, UTS, R1, sigmaA_R1):
    """
    Equation 3
    """
    sigma_prime_a = UTS / ( (UTS/sigmaA1) + R - R1)

    return sigma_prime_a



def TT_sector_equation(R, Rmin, Rmax, sigmaA_Rmin, sigmaA_Rmax):
    """
    Equation 4
    """
    sigma_prime_a = TODO

    return sigma_prime_a


def CC_sector_equation(R, UCS, R1, sigmaA1):
    """
    Equation 5
    """
    sigma_prime_a = TODO

    return sigma_prime_a


if __name__ == "__main__":

    SNC_df = pd.read_csv(INPUT_FILE)

    for N in .... :  TODO
        sigma_a calculate_sigma_prime_a(R, R_ratios)
        ## write results in CLD_df

    CLD_df = pd.to_csv(OUTPUT_FILE)
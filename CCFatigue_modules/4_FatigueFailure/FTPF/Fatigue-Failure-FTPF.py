#!/usr/bin/env python
""" CCFatigue - Module 4 - Fatigue-Failure-FTPF.py

FTPF is described in Tassos red book, in p. 163 - the original papers are 
https://www.sciencedirect.com/science/article/pii/S0142112398000735?via%3Dihub (https://doi.org/10.1016/S0142-1123(98)00073-5)
https://www.sciencedirect.com/science/article/pii/S014211230200004X?via%3Dihub (https://doi.org/10.1016/S0142-1123(02)00004-X)
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

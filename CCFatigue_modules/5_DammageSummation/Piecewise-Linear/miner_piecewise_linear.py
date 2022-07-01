#!/usr/bin/env python
""" CCFatigue - Module 5 - Miner-Piecewise-linear.py

There is no documentation specifically for this one. A description is given in the red book,
after p. 206 and in https://www.sciencedirect.com/science/article/pii/S1359835X04000466?via%3Dihub
(https://doi.org/10.1016/j.compositesa.2004.02.009)
I can explain how does it work… 
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

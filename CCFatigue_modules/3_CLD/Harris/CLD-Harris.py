#!/usr/bin/env python
""" CCFatigue - Module 3 - CLD-Harris.py

Harris diagram is described in Tassos red book p. 108 - the original papers are 
https://www.sciencedirect.com/science/article/pii/0142112394904782?via%3Dihub (https://doi.org/10.1016/0142-1123(94)90478-2)
https://www.sciencedirect.com/science/article/pii/S0266353897001218?via%3Dihub (https://doi.org/10.1016/S0266-3538(97)00121-8)
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

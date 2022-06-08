#!/usr/bin/env python
""" CCFatigue - Module 4 - Fatigue-Failure-Hashin-Rotem.py

H-R is described in the red book, in p. 159 - the original paper is
https://journals.sagepub.com/doi/10.1177/002199837300700404
"""

import os
import math
import numpy as np
import pandas as pd
from scipy import stats
from itertools import chain

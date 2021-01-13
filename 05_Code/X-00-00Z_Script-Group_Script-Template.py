#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : 

> Script Number(s)
  : 

> Purpose of Script
  : 

"""


# --------------------------------------------------
# Load libraries required
# --------------------------------------------------
# ------- Load libraries -------
# # 1. Basic libraries
import os
import sys
import importlib
import pathlib as pl
import pandas as pd
import dask.dataframe as dd
import multiprocessing

# # 2. Additional libraries used in this script
import re


# ------- Load user-written libraries -------
# # 1. Add a path to "sys.path", and change working directory if necessary
if os.getcwd() == '/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis':
    sys.path.append(os.path.abspath('../Energy-Demand-Analysis/05_Code'))
else:
    path_toAdd = pl.Path.cwd().parents[1]
    sys.path.append(str(path_toAdd))
    path_project = pl.Path.cwd().parents[2]
    os.chdir(path_project)

# # 2. Load libraries
HD = importlib.import_module('H-Energy-Demand-Analysis')
FNC = importlib.import_module('F-Energy-Demand-Analysis_Common-Functions')


# --------------------------------------------------
# Set path(s) and parameter(s)
# --------------------------------------------------
# ------- Set path(s) -------
# # 1. Path(s) from which data file(s) will be loaded
# (...)

# # 2. Path(s) at which data file(s) will be saved
# (...)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = ''
# # 0.2. Parameter for Dask DF
N_PARTITIONS = multiprocessing.cpu_count()


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# To ...
# --------------------------------------------------

# ------- To ... -------
# (...)

FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-02A

> Purpose of Script
  : To make a CSV file from TXT files containing metering data for Natural Gas.

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

# # 2. Additional libraries used in this script
import re
from tqdm import tqdm


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
# (NOT Applicable)

# # 2. Path(s) at which data file(s) will be saved
PATH_TOSAVE = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS, 'CER_Metering_Gas.CSV'
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-02A'


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Make a list of TXT files that will be loaded
# --------------------------------------------------
# ------- Make a list of TXT files -------
list_paths = []
for roots, dirs, files in os.walk(HD.PATH_DATA_RAW_USE_CER_GAS):
    for f in files:
        if f.endswith('.txt'):
            path_f = os.path.join(roots, f)
            if path_f not in list_paths:
                list_paths.append(path_f)


# --------------------------------------------------
# Create a CSV file from TXT files
# --------------------------------------------------
# ------- Create a CSV file -------
# # 1. Open a CSV file to write contents from TXT files
csv = open(PATH_TOSAVE, mode='a')

# # 2. Add contents in TXT files to the CSV file
for path in tqdm(list_paths):
    tmp_file = open(path)
    while True:
        line = tmp_file.readline()
        if not line: # ## This condition means facing the last line
            break
        else:
            result = re.search('[0-9]+?\,[0-9]+?\,.+\n', line)
            # ## In order to write meter read data only
            if result is not None: # ## Several lines contain no meter read data
                csv.write(result[0])
            else:
                pass
    tmp_file.close()

csv.close()



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

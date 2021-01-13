#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-02, NOAA

> Script Number(s)
  : B-02-01B

> Purpose of Script
  : Ingest NOAA LCD Dataset between 2003 and 2013

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
PATH_TO_LOAD = HD.PATH_DATA_RAW_USE_NOAA_LCD

# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE = 'NOAA_Local-Climatological-Data.parquet'
PATH_TO_SAVE = os.path.join(HD.PATH_DATA_INTERMEDIATE_NOAA_LCD, FILE_TO_SAVE)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-02-01B'
# # 0.2. Parameter for Dask DF
N_PARTITIONS = multiprocessing.cpu_count()

# # 1. Parameters for NOAA LCD Dataset
# # 1.1. For Time Range
YEAR_FROM = 2003
YEAR_TO = 2014


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Ingest NOAA LCD Dataset
# --------------------------------------------------
# ------- Ingest NOAA LCD Dataset -------
# # 1. Import CSV files
list_colsToUse = [
    'DATE', 'REPORT_TYPE', 'DailyCoolingDegreeDays', 'DailyHeatingDegreeDays'
]
dict_dtype = {col: 'object' for col in list_colsToUse}
dict_toRename = {
    'DATE': 'date',
    'REPORT_TYPE': 'report_type',
    'DailyCoolingDegreeDays': 'cdd_daily',
    'DailyHeatingDegreeDays': 'hdd_daily'
}
for year in range(YEAR_FROM, YEAR_TO + 1):
    path = os.path.join(PATH_TO_LOAD, '72483993225' + '_' + str(year) + '.csv')
    tmp_objName = 'raw_' + str(year)
    # ## Import a CSV file
    globals()[tmp_objName] = \
        pd.read_csv(path, usecols=list_colsToUse, dtype=dict_dtype)
    # ## Rename columns
    globals()[tmp_objName].rename(columns=dict_toRename, inplace=True)
    # ## Strip whitespaces
    globals()[tmp_objName]['report_type'] = \
        globals()[tmp_objName]['report_type'].apply(
            lambda x: re.sub(r'\s{1,}', '', x)
        )
    # ## Change values, and then convert data type
    globals()[tmp_objName]['cdd_daily'] = \
        globals()[tmp_objName]['cdd_daily'].apply(
            lambda x: str(x).replace('s', '')
        ).astype('float32')
    globals()[tmp_objName]['hdd_daily'] = \
        globals()[tmp_objName]['hdd_daily'].apply(
            lambda x: str(x).replace('s', '')
        ).astype('float32')
    # ## Convert data type
    globals()[tmp_objName] = \
        globals()[tmp_objName].astype({'date': 'datetime64[ns]'})
    # ## Drop unnecessary rows
    globals()[tmp_objName] = \
        globals()[tmp_objName][globals()[tmp_objName]['report_type'] == 'SOD']


# # 2. Combine DFs imported
list_df_toJoin = [obj for obj in dir() if re.match('raw_', obj)]
dfs = [globals()[obj] for obj in list_df_toJoin]
raw = pd.concat(dfs, axis='index', ignore_index=True)


# ------- Add observations that are missed in LCD -------
# ## Note:
# # 1. Make a DF that is used to add observations
dict_toAdd = {
    'date': [
        pd.Timestamp('20031231 23:59:00'), pd.Timestamp('20051231 23:59:00'),
        pd.Timestamp('20111231 23:59:00'), pd.Timestamp('20131231 23:59:00'),
        pd.Timestamp('20141231 23:59:00')
    ],
    'report_type': ['Manual'] * 5,
    'cdd_daily': [0, 0, 0, 0, 0],
    'hdd_daily': [65 - 47.1, 65 - 54.8, 65 - 49, 65 - 46.9, 65 - 41.8]
}
df_toAdd = pd.DataFrame(dict_toAdd)

# # 2. Add observations
raw_combined = pd.concat([raw, df_toAdd], axis='index', ignore_index=True)

# # 3. Sort the DF combined
noaa_lcd = \
    raw_combined.sort_values(by=['date'], axis='index', ignore_index=True)


# ------- Save the DF ingested in Parquet format -------
FNC.save_toPq(noaa_lcd, PATH_TO_SAVE)


FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

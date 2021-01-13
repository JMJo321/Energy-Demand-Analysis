#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-02, NOAA

> Script Number(s)
  : B-02-01A

> Purpose of Script
  : Download NOAA Local Climatological Data (LCD) and Global Summary of the
    Day Datasets

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
import requests


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
url_base_lcd = \
    'https://www.ncei.noaa.gov/data/local-climatological-data/access/'
url_base_gsod = \
    'https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/'

# # 2. Path(s) at which data file(s) will be saved
# # 2.1. For NOAA LCD
PATH_TO_SAVE_ORIGINAL_LCD = HD.PATH_DATA_RAW_ORIGINAL_NOAA_LCD
PATH_TO_SAVE_USE_LCD = HD.PATH_DATA_RAW_USE_NOAA_LCD
# # 2.2. For NOAA GSOD
PATH_TO_SAVE_ORIGINAL_GSOD = HD.PATH_DATA_RAW_ORIGINAL_NOAA_GSOD
PATH_TO_SAVE_USE_GSOD = HD.PATH_DATA_RAW_USE_NOAA_GSOD


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-02-01A'
# # 0.2. Parameter for Dask DF
N_PARTITIONS = multiprocessing.cpu_count()

# # 1. Parameters for NOAA LCD Dataset
# # 1.1. For Time Range
YEAR_FROM = 2003
YEAR_TO = 2014
# # 1.2. Station IDs for Sacramento Metropolitan Airport
ID_STATION_MASTER = '724839'    # USAF Master Station Catalog Identifier
ID_STATION_WBAN = '93225'   # NCEI WBAN Identifier
ID_STATION = ID_STATION_MASTER + ID_STATION_WBAN


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Download NOAA LCD Dataset
# --------------------------------------------------
# ------- Download NOAA LCD Dataset. -------
for year in range(YEAR_FROM, YEAR_TO + 1):
    tmp_url = url_base_lcd + '/' + str(year) + '/' + ID_STATION + '.csv'
    r = requests.get(tmp_url)
    tmp_fileName = ID_STATION + '_' + str(year) + '.csv'

    tmp_path = os.path.join(PATH_TO_SAVE_ORIGINAL_LCD, tmp_fileName)
    with open(tmp_path, 'wb') as file:
        file.write(r.content)

    tmp_path = os.path.join(PATH_TO_SAVE_USE_LCD, tmp_fileName)
    with open(tmp_path, 'wb') as file:
        file.write(r.content)


# --------------------------------------------------
# Download NOAA GSOD Dataset
# --------------------------------------------------
# ------- Download NOAA GSOD Dataset -------
# ## Note: 12-31 Observations for 2003, 2005, 2011, 2013, and 2014 are
# ## missed from LCD.
for year in [2003, 2005, 2011, 2013, 2014]:
    if year == 2003:
        tmp_url = url_base_gsod + '/' + str(year) + '/' + ID_STATION_MASTER + \
              '99999' + '.csv'
    else:
        tmp_url = url_base_gsod + '/' + str(year) + '/' + ID_STATION + '.csv'

    r = requests.get(tmp_url)
    tmp_fileName = ID_STATION + '_' + str(year) + '.csv'

    tmp_path = os.path.join(PATH_TO_SAVE_ORIGINAL_GSOD, tmp_fileName)
    with open(tmp_path, 'wb') as file:
        file.write(r.content)

    tmp_path = os.path.join(PATH_TO_SAVE_USE_GSOD, tmp_fileName)
    with open(tmp_path, 'wb') as file:
        file.write(r.content)


FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

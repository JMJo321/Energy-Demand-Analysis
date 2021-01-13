#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : (NA)

> Script Number(s)
  : (NA)

> Purpose of Script
: To define parameters and functions that are commonly used in the Project.
"""


# --------------------------------------------------
# To import module(s) and package(s) required
# --------------------------------------------------
import os
import sys
import pandas as pd
import importlib


# --------------------------------------------------
# To define commonly used function(s)
# --------------------------------------------------
# ------- To load functions -------
fnc = importlib.import_module('F-Energy-Demand-Analysis_Common-Functions')


# --------------------------------------------------
# To define parameter(s), path(s), etc.
# --------------------------------------------------
# ------- To define parameters -------
# # 1. Parameters used to download raw data files
# # 1.1. Sacramento Municipal Utility District (SMUD)
# # 1.1.1. Residential Rate Schedules
# (NOT Applicable)


# ------- To set path(s) -------
# # 1. Project path
PATH_PROJECT = os.path.abspath('../Energy-Demand-Analysis')


# # 2. Base path(s) for data files
# # 2.1. Raw Data
PATH_DATA_RAW_ORIGINAL = os.path.join(
    PATH_PROJECT,
    '04_Data/01_Raw-Data/01_Original'
)
PATH_DATA_RAW_USE = os.path.join(PATH_PROJECT, '04_Data/01_Raw-Data/02_Use')

# # 2.2. Intermediate Data
PATH_DATA_INTERMEDIATE = os.path.join(
    PATH_PROJECT,
    '04_Data/02_Intermediate-Data'
)

# # 2.3. Data for Analysis
PATH_DATA_ANALYSIS = os.path.join(
    PATH_PROJECT,
    '04_Data/03_Data-for-Analysis'
)

# ## To generate folders defined
fnc.makeFolder(
    [PATH_DATA_RAW_ORIGINAL, PATH_DATA_RAW_USE, PATH_DATA_INTERMEDIATE]
)


# # 3. Specific path(s) for data files
# # 3.1. Sacramento Municipal Utility District (SMUD)
DIR_DATA_SMUD = 'SMUD'
PATH_DATA_RAW_ORIGINAL_SMUD \
    = os.path.join(PATH_DATA_RAW_ORIGINAL, DIR_DATA_SMUD)
PATH_DATA_RAW_USE_SMUD = os.path.join(PATH_DATA_RAW_USE, DIR_DATA_SMUD)
PATH_DATA_INTERMEDIATE_SMUD \
    = os.path.join(PATH_DATA_INTERMEDIATE, DIR_DATA_SMUD)

# # 3.1.1. Data Files obtained from Kevin
DIR_DATA_SMUD_KEVIN = 'From-Kevin'
# # 3.1.1.1. For Raw Data Folders
PATH_DATA_RAW_ORIGINAL_SMUD_KEVIN \
    = os.path.join(PATH_DATA_RAW_ORIGINAL_SMUD, DIR_DATA_SMUD_KEVIN)
PATH_DATA_RAW_USE_SMUD_KEVIN \
    = os.path.join(PATH_DATA_RAW_USE_SMUD, DIR_DATA_SMUD_KEVIN)
# # 3.1.1.2. For Intermediate Data Folders
PATH_DATA_INTERMEDIATE_SMUD_BILLING \
    = os.path.join(PATH_DATA_INTERMEDIATE_SMUD, 'Billing-Data')

# # 3.1.2. Data Files obtained from SMUD directly
# # 3.1.2.1. Residential Rate Schedules
DIR_DATA_SMUD_RRS = 'Residential-Rate-Schedules'
PATH_DATA_RAW_ORIGINAL_SMUD_RRS \
    = os.path.join(PATH_DATA_RAW_ORIGINAL_SMUD, DIR_DATA_SMUD_RRS)
PATH_DATA_RAW_USE_SMUD_RRS \
    = os.path.join(PATH_DATA_RAW_USE_SMUD, DIR_DATA_SMUD_RRS)
PATH_DATA_INTERMEDIATE_SMUD_RRS \
    = os.path.join(PATH_DATA_INTERMEDIATE_SMUD, DIR_DATA_SMUD_RRS)


# # 3.2. National Oceanic and Atmospheric Administration (NOAA)
DIR_DATA_NOAA = 'NOAA'
PATH_DATA_RAW_ORIGINAL_NOAA \
    = os.path.join(PATH_DATA_RAW_ORIGINAL, DIR_DATA_NOAA)
PATH_DATA_RAW_USE_NOAA = os.path.join(PATH_DATA_RAW_USE, DIR_DATA_NOAA)
PATH_DATA_INTERMEDIATE_NOAA \
    = os.path.join(PATH_DATA_INTERMEDIATE, DIR_DATA_NOAA)
# # 3.2.1. Local Climatological Data (LCD) Dataset
DIR_DATA_NOAA_LCD = 'LCD'
PATH_DATA_RAW_ORIGINAL_NOAA_LCD \
    = os.path.join(PATH_DATA_RAW_ORIGINAL_NOAA, DIR_DATA_NOAA_LCD)
PATH_DATA_RAW_USE_NOAA_LCD \
    = os.path.join(PATH_DATA_RAW_USE_NOAA, DIR_DATA_NOAA_LCD)
PATH_DATA_INTERMEDIATE_NOAA_LCD \
    = os.path.join(PATH_DATA_INTERMEDIATE_NOAA, DIR_DATA_NOAA_LCD)
# # 3.2.2. Global Summary of the Day (GSOD) Dataset
DIR_DATA_NOAA_GSOD = 'GSOD'
PATH_DATA_RAW_ORIGINAL_NOAA_GSOD \
    = os.path.join(PATH_DATA_RAW_ORIGINAL_NOAA, DIR_DATA_NOAA_GSOD)
PATH_DATA_RAW_USE_NOAA_GSOD \
    = os.path.join(PATH_DATA_RAW_USE_NOAA, DIR_DATA_NOAA_GSOD)
PATH_DATA_INTERMEDIATE_NOAA_GSOD \
    = os.path.join(PATH_DATA_INTERMEDIATE_NOAA, DIR_DATA_NOAA_GSOD)


# ------- To set categories and orderedness for categorical variables -------
# # Note
# # : Dataset-specific categorical variables are defined in B-00-00A scripts,
# #   if necessary.

# # 1. For categorical data for 'TRUE' or 'FALSE'
CATEGORY_FOR_TF = pd.CategoricalDtype(
    categories = ['FALSE', 'TRUE'],
    ordered    = True
)


#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-01, SMUD

> Script Number(s)
  : B-01-01A

> Purpose of Script
  : Ingest Residential Rate Schedules from an Excel file.

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
FILE_TO_LOAD = 'SMUD_Residential-Rate-Schedules.xlsx'
PATH_TO_LOAD = os.path.join(HD.PATH_DATA_RAW_USE_SMUD_RRS, FILE_TO_LOAD)

# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE = 'SMUD_Residential-Rate-Schedules.parquet'
PATH_TO_SAVE = os.path.join(HD.PATH_DATA_INTERMEDIATE_SMUD_RRS, FILE_TO_SAVE)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-01-01A'

# # 1. Parameters used when making an empty panel DF
PANEL_DATE_FROM = '2004-01-01'
PANEL_DATE_TO = '2013-12-31'
PANEL_FREQUENCY = 'D'


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Ingest an Excel file to make Pandas DFs
# --------------------------------------------------
# ------- Import an Excel file -------
raw_rates = pd.read_excel(PATH_TO_LOAD, sheet_name= 'UNIT_RATES')
raw_qtys = pd.read_excel(PATH_TO_LOAD, sheet_name= 'QUANTITIES')


# ------- Generate a Pandas DF from "raw_rates" -------
print('With respect to \"raw_rates\":')
# # 1. Reshape the imported DF into a long-form Pandas DF
# # 1.1. Add a column that will be used later
raw_rates['tmp_col'] = \
    raw_rates['RATE_CODES'].apply(lambda row: len(re.split(', ', row)))

# # 1.2. Make an empty Pandas DF to which data will be appended
data_rates = pd.DataFrame()

# # 1.3. Append data to the empty DF
for row in range(0, len(raw_rates)):
    tmp_codes = raw_rates.loc[row, 'RATE_CODES']
    tmp_list_codes = re.split(', ', tmp_codes)
    tmp_list_cols_ignore = ['RATE_CODES', 'tmp_col']
    tmp_list_cols = \
        [col for col in raw_rates.columns if col not in tmp_list_cols_ignore]

    for code in tmp_list_codes:
        data_rates = data_rates.append(raw_rates.loc[row, tmp_list_cols])

    data_rates.loc[row, 'RATE_CODE'] = tmp_list_codes

# # 1.4. Reset index
data_rates.reset_index(drop= True, inplace= True)


# # 2. Make the Pandas DF clean
# # 2.1. Rename columns
list_cols_old = data_rates.columns.to_list()
list_cols_new = [col.lower() for col in list_cols_old]
data_rates.rename(
    columns= dict(zip(list_cols_old, list_cols_new)), inplace= True
)

# # 2.2. Reorder columns
list_cols_toReorder = [
    'revision_year', 'season', 'season_from', 'season_to', 'rate_category',
    'rate_subcategory', 'is_closed','rate_code', 'charge_item', 'charge_type',
    'charge_in_usd'
]
data_rates = data_rates.reindex(columns= list_cols_toReorder)

# # 2.3. Convert data types of several columns
dict_astype = {'is_closed': 'bool'}
data_rates = data_rates.astype(dict_astype)


# # 3. Find primary key(s) of the Pandas DF, and sort it based on the key(s)
# # 3.1. Check whether there are duplicated observations
if data_rates.duplicated().any():
    print('There are duplicated observations. Those will be dropped.')
    data_rates.drop_duplicates(inplace= True)
else:
    print('There are NO duplicated observations.')

# # 3.2. Find primary key(s) of the Pandas DF
list_cols = ['revision_year', 'season_from', 'rate_code', 'charge_item']
if data_rates[list_cols].duplicated().any():
    sys.exit('The column(s) is(are) NOT primary key(s) of the DF.\n')
else:
    print('The column(s) consist(s) of primary key(s) of the DF.\n')

# # 3.3. Sort the Pandas DF based on the primary key(s)
list_cols_sortby = list_cols
data_rates.sort_values(by= list_cols_sortby, inplace= True, ignore_index= True)


# ------- Generate a Pandas DF from "raw_qtys" -------
print('With respect to \"raw_qtys\":')
# # 1. Reshape the imported DF into a long-form Pandas DF
# # 1.1. Add a column that will be used later
raw_qtys['tmp_col'] = \
    raw_qtys['RATE_CODES'].apply(lambda row: len(re.split(', ', row)))

# # 1.2. Make an empty Pandas DF to which data will be appended
data_qtys = pd.DataFrame()

# # 1.3. Append data to the empty DF
for row in range(0, len(raw_qtys)):
    tmp_codes = raw_qtys.loc[row, 'RATE_CODES']
    tmp_list_codes = re.split(', ', tmp_codes)
    tmp_list_cols_ignore = ['RATE_CODES', 'tmp_col']
    tmp_list_cols = \
        [col for col in raw_qtys.columns if col not in tmp_list_cols_ignore]

    for code in tmp_list_codes:
        data_qtys = data_qtys.append(raw_qtys.loc[row, tmp_list_cols])

    data_qtys.loc[row, 'RATE_CODE'] = tmp_list_codes

# # 1.4. Reset index
data_qtys.reset_index(drop= True, inplace= True)


# # 2. Make the Pandas DF clean
# # 2.1. Rename columns
list_cols_old = data_qtys.columns.to_list()
list_cols_new = [col.lower() for col in list_cols_old]
data_qtys.rename(
    columns= dict(zip(list_cols_old, list_cols_new)), inplace= True
)

# # 2.2. Reorder columns
list_cols_toReorder = [
    'revision_year', 'season', 'season_from', 'season_to', 'rate_category',
    'rate_subcategory', 'rate_code', 'tier_1_name', 'tier_1_qty_from',
    'tier_1_qty_to', 'tier_2_name', 'tier_2_qty_from', 'tier_2_qty_to',
    'tier_3_name', 'tier_3_qty_from', 'tier_3_qty_to'
]
data_qtys = data_qtys.reindex(columns= list_cols_toReorder)


# # 3. Find primary key(s) of the Pandas DF, and sort it based on the key(s)
# # 3.1. Check whether there are duplicated observations
if data_qtys.duplicated().any():
    print('There are duplicated observations. Those will be dropped.')
    data_qtys.drop_duplicates(inplace= True)
else:
    print('There are NO duplicated observations.')

# # 3.2. Find primary key(s) of the Pandas DF
list_cols = ['revision_year', 'season', 'season_from', 'rate_code']
if data_qtys[list_cols].duplicated().any():
    sys.exit('The column(s) is(are) NOT primary key(s) of the DF.\n')
else:
    print('The column(s) consist(s) of primary key(s) of the DF.\n')

# # 3.3. Sort the Pandas DF based on the primary key(s)
list_cols_sortby = list_cols
data_qtys.sort_values(by= list_cols_sortby, inplace= True, ignore_index= True)

# --------------------------------------------------
# Make a Panel Dataset from the two Pandas DF
# --------------------------------------------------
# ------- Make a Panel Dataset -------
# # 1. Make an empty panel data template
smud_rrs = pd.DataFrame()


# # 2. Copy data to the empty panel data template
# # 2.1. From "data_rates"
list_idxs = data_rates.index
list_cols = data_rates.columns.to_list()
list_dtypes = [data_rates[col].dtype for col in list_cols]
for idx in list_idxs:
    tmp_dateFrom = data_rates.loc[idx, 'season_from']
    tmp_dateTo = data_rates.loc[idx, 'season_to']
    tmp_emptyPanel = \
        FNC.makeEmptyPanel(
            date_from=tmp_dateFrom, date_to=tmp_dateTo, frequency='D'
        )

    for col in list_cols:
        tmp_value_forCopy = data_rates.loc[idx, col]
        tmp_emptyPanel.loc[tmp_dateFrom:tmp_dateTo, col] = tmp_value_forCopy

    tmp_emptyPanel.reset_index(drop= False, inplace= True)
    smud_rrs = pd.concat([smud_rrs, tmp_emptyPanel], ignore_index= True)

# # 2.2. From "data_qtys"
list_cols_forConditions = \
    [
        'season_from', 'season_to', 'rate_category', 'rate_subcategory',
        'rate_code'
    ]
list_cols_toCopy = [col for col in data_qtys.columns if re.match('tier', col)]
list_cols_toCopy_dtypes = [data_qtys[col].dtype for col in list_cols_toCopy]
for col in list_cols_toCopy:
    # ## To add a column with no values
    smud_rrs[col] = None
    tmp_dtype = list_cols_toCopy_dtypes[list_cols_toCopy.index(col)]
    smud_rrs = smud_rrs.astype({col: tmp_dtype})

    for row in range(0, data_qtys.shape[0]):
        # ## To make an object having the value that will be copied
        tmp_varName_value = 'tmp_' + col
        globals()[tmp_varName_value] = data_qtys.loc[row, col]

        for key in list_cols_forConditions:
            # ## To make objects that will be used to make a condition for
            # ## selecting rows
            tmp_varName = 'tmp_' + key
            globals()[tmp_varName] = data_qtys.loc[row, key]

        # ## To copy data
        tmp_conditions = \
            '@tmp_season_from <= date & date <= @tmp_season_to' + ' & ' + \
                'rate_category == @tmp_rate_category' + ' & ' + \
                'rate_subcategory == @tmp_rate_subcategory' + ' & ' + \
                'rate_code == @tmp_rate_code'
        tmp_select = smud_rrs.query(tmp_conditions).index
        smud_rrs.loc[tmp_select, col] = eval(tmp_varName_value)


# # 3. To convert data type of a column
dict_astype = {'revision_year': 'int'}
smud_rrs = smud_rrs.astype(dict_astype)
# ## Note: Converting data type (from 'object' to 'int') before copying data
# ## makes an error.


# --------------------------------------------------
# Save the Panel Dataset
# --------------------------------------------------
# ------- Save the Panel Dataset -------
FNC.save_toPq(smud_rrs, PATH_TO_SAVE)


FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

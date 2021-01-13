#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-01, SMUD

> Script Number(s)
  : B-01-02A

> Purpose of Script
  : To ingest Stata files - SMUD billing data

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
import numpy as np


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
DD = importlib.import_module('D-Energy-Demand-Analysis_Data-Dictionary')


# --------------------------------------------------
# Set path(s) and parameter(s)
# --------------------------------------------------
# ------- Set path(s) -------
# # 1. Path(s) from which data file(s) will be loaded
# ## Note
# ## : Paths of .dta files are generated when they are imported.

# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE = 'SMUD_Billing-Data.parquet'
PATH_TO_SAVE = \
  os.path.join(HD.PATH_DATA_INTERMEDIATE_SMUD_BILLING, FILE_TO_SAVE)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-01-02A'
# # 0.2. Parameter for Dask DF
N_PARTITIONS = multiprocessing.cpu_count()


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Generate a Pandas DF from Stata files
# --------------------------------------------------
# ------- Import Stata files, and make a Pandas DF -------
# # 1. Import Stata files
for year in range(2005, 2013 + 1):
  tmp_file_toLoad = "ucd_billing_" + str(year) + '.dta'
  tmp_path_toLoad = \
    os.path.join(HD.PATH_DATA_RAW_USE_SMUD_KEVIN, tmp_file_toLoad)
  tmp_objName = "data_" + str(year)
  globals()[tmp_objName] = pd.read_stata(tmp_path_toLoad)

# # 2. Concatenate the DFs
list_df_toJoin = [var for var in dir() if re.match('data_', var)]
dfs = [globals()[var] for var in list_df_toJoin]
data_concat = pd.concat(dfs, axis= 'index', ignore_index= True)

# # 3. Rename columns
data_concat.rename(columns= DD.dict_toRename_billing, inplace= True)


# --------------------------------------------------
# Convert DF's class, and do additional work
# --------------------------------------------------
# ------- Convert the Pandas DF to Dask DF, and do additional work -------
# # 1. Convert DF's class
smud_billing = \
  dd.from_pandas(data_concat, npartitions= N_PARTITIONS)
FNC.ddf_info(smud_billing)

# # 2. Conduct additional work
# # 2.1. Add a column indicating the length of billing period
list_cols_select = ['period_to', 'period_from']
smud_billing['period_len'] = \
  smud_billing['period_to'] - smud_billing['period_from']
smud_billing['period_len'] = smud_billing['period_len'].dt.days
# ## Note
# ## : Data type "timedelta" is NOT supported in Apache Arrow.

# # 2.2. Convert data type of "is_pv"
dict_astype = {'is_pv': 'bool'}
smud_billing = smud_billing.astype(dict_astype)
FNC.ddf_info(smud_billing)

# # 2.3. Prevent columns whose data types are "object" include numeric data, and
# #      then check whether the columns include empty string values that should
# #      be replaced with NaN
list_cols_object = ['rate_code']
list_toNan = []
for col in list_cols_object:
    smud_billing[col] = smud_billing[col].apply(lambda x: str(x), meta= (col))

    tmp_test = smud_billing[col].apply(lambda x: x == "", meta= (col)).any()
    if tmp_test.compute():
        list_toNan = list_toNan + [col]
        print(
          'There is(are) empty string value(s) with respect to "{0}".\n'.format(col)
        )
    else:
        print(
          'There is NOT empty string value with respect to "{0}".\n'.format(col)
        )

# ## Replace empty string values with NaN
if not len(list_toNan) == 0:
    for col in list_toNan:
        smud_billing = smud_billing.replace(to_replace={col: {'': np.nan}})


# ------- Find primary key(s) of the Dask DF -------
# # 1. Check whether there are duplicated observations
test = smud_billing.shape[0] == smud_billing.drop_duplicates().shape[0]
if test.compute():
    print('There are NO duplicated observation.\n')
else:
    print('There are duplicated observation. Those will be dropped.\n')
    smud_billing = smud_billing.drop_duplicates()

# # 2. Find the Dask DF's primary keys
list_cols_by = \
  ['id_account', 'id_premise', 'period_from', 'period_to', 'rate_code']
test = \
  smud_billing.shape[0] == smud_billing[list_cols_by].drop_duplicates().shape[0]
if not test.compute():
  sys.exit('The column(s) given do NOT consist of primary key(s).\n')
else:
    print('Those columns consist of primary key(s) of the DF.\n')


# --------------------------------------------------
# Save the Dask DF generated above
# --------------------------------------------------
# ------- Save the Dask DF -------
# # 1. Convert the Dask DF to Pandas DF
smud_billing_pd = smud_billing.compute()

# # 2. Reorder columns of the Pandas DF
list_cols_reorder = [
  'id_account', 'id_premise', 'is_pv', 'period_from', 'period_to',
  'period_len', 'rate_code', 'charge_fixed', 'charge_variable',
  'charge_total', 'kwh_total', 'kwh_t1', 'charge_variable_t1', 'kwh_t2',
  'charge_variable_t2', 'kwh_t3', 'charge_variable_t3', 'id_bu_part'
]
smud_billing_pd = smud_billing_pd.reindex(columns= list_cols_reorder)

# # 3. Sort the Pandas DF
list_sortby = list_cols_by
smud_billing_pd.sort_values(by= list_sortby, inplace= True, ignore_index= True)

# # 4. Save the Pandas DF in parquet format
FNC.save_toPq(smud_billing_pd, PATH_TO_SAVE)


FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

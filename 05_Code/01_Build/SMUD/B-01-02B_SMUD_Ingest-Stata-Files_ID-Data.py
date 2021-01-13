#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-01, SMUD

> Script Number(s)
  : B-01-02B

> Purpose of Script
  : To ingest Stata files - SMUD ID data

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
FILE_TO_LOAD = 'ucd_id_numbers_data.dta'
PATH_TO_LOAD = os.path.join(HD.PATH_DATA_RAW_USE_SMUD_KEVIN, FILE_TO_LOAD)

# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE = 'SMUD_ID-Data.parquet'
PATH_TO_SAVE = \
    os.path.join(HD.PATH_DATA_INTERMEDIATE_SMUD_BILLING, FILE_TO_SAVE)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-01-02B'
# # 0.2. Parameter for Dask DF
N_PARTITIONS = multiprocessing.cpu_count()


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Generate a Dask DF from a Stata file
# --------------------------------------------------
# ------- Import a Stata file as Pandas DF -------
data = pd.read_stata(PATH_TO_LOAD)

# ------- Convert the Pandas DF to Dask DF -------
smud_id = dd.from_pandas(data, npartitions= N_PARTITIONS)
FNC.ddf_info(smud_id)


# --------------------------------------------------
# Make the DF clean, and then find primary key(s)
# --------------------------------------------------
# ------- Make the DF clean. -------
# # 1. Rename data fields
smud_id = smud_id.rename(columns= DD.dict_toRename_id)

# # 2. Prevent columns whose data types are "object" include numeric data, and
# #    then check whether the columns include empty string values that should
# #    be replaced with NaN
list_cols_object = smud_id.select_dtypes(include= 'object').columns
list_toNan = []
for col in list_cols_object:
    smud_id[col] = smud_id[col].apply(lambda x: str(x), meta= (col))

    tmp_test = smud_id[col].apply(lambda x: x == "", meta= (col)).any()
    if tmp_test.compute():
        list_toNan = list_toNan + [col]
        print(
          'There is(are) empty string value(s) with respect to' + \
            ' "{0}".\n'.format(col)
        )
    else:
        print(
          'There is NOT empty string value with respect to "{0}".\n'.format(col)
        )

# ## Replace empty string values with NaN
if not len(list_toNan) == 0:
    for col in list_toNan:
        smud_id = smud_id.replace(to_replace= {col: {'': np.nan}})

# # 3. Convert data type(s)
# # 3.1. Prepare for conversion
smud_id = smud_id.replace(to_replace= {'is_in_isa': {'Y': True, 'N': False}})
smud_id['date_move_out'] = \
    smud_id['date_move_out'].apply(
        lambda row: re.sub('^9{4}', '2222', row), meta= ('date_move_out')
    )
# ## Note:
# ## 1) Although "dtypes" denotes the type of `date_move_out` is "object",
# ##    applying "re.sub" to the column makes an error.
# ## 2) Year "9999" is forcibly converted to "2222".

# # 3.2. Convert columns' data types
dict_astype = {'is_in_isa': 'bool', 'date_move_out': 'datetime64[ns]'}
smud_id = smud_id.astype(dict_astype)


# ------- Find Primary Key(s) of the Dask DF. -------
# # 1. Check whether there are duplicated observations
test = smud_id.shape[0] == smud_id.drop_duplicates().shape[0]
if test.compute():
    print('There are NO duplicated observation.\n')
else:
    print('There are duplicated observation. Those will be dropped.\n')
    smud_id = smud_id.drop_duplicates()

# # 2. Find the Dask DF's primary keys
list_cols_by = \
    [
        'id_account', 'id_premise', 'rate_code_from_id', 'date_move_in',
        'date_move_out'
    ]
grouped = smud_id.groupby(list_cols_by).count()
if not grouped.where(grouped > 1).isnull().values.all().compute():
    sys.exit('The column(s) given do NOT consist of primary key(s).\n')
else:
    print('The column(s) consist(s) of primary key(s) of the DF.\n')


# --------------------------------------------------
# Save the Dask DF generated above
# --------------------------------------------------
# ------- Save the Dask DF -------
# # 1. Convert the Dask DF to a Pandas DF
smud_id_pd = smud_id.compute()
smud_id_pd.info()

# # 2. Sort the Pandas DF
smud_id_pd.sort_values(by= list_cols_by, inplace= True, ignore_index= True)

# # 3. Save the Pandas DF in parquet format
FNC.save_toPq(smud_id_pd, PATH_TO_SAVE)


FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

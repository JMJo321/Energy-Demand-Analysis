#!/Users/jmjo/.pyenv/versions/energy-demand/bin/python
#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : (NA)

> Script Number(s)
  : (NA)

> Purpose of Script
: To define functions that are commonly used in the Project.
"""


# --------------------------------------------------
# To import module(s) and package(s) required
# --------------------------------------------------
import os
import re
import time
import pandas as pd
import dask.dataframe as dd
import pyarrow as pa
import pyarrow.parquet as papq


# --------------------------------------------------
# To define commonly used function(s)
# --------------------------------------------------
# ------- To print the information about Dask DF -------
def ddf_info(dask_dataframe):
    print(dask_dataframe.info())
    print('\n')
    print(dask_dataframe.dtypes)
    print('\n')


# ------- To find Primary Key(s) of a DF -------
def findKeys(dataframe, list_cols):
    '''
    To Find Primary Keys of a DF.
    Since Modin does not fully support "groupby" API, "query" API is used.
    '''

    grouped_sum \
        = dataframe.assign(N_obs = 1)[list_cols + ['N_obs']].groupby(list_cols).sum()
    df_query = grouped_sum.query('N_obs > 1')

    if df_query.shape[0] == 0:
        print('No Duplicated Observations given the column(s).')
    else:
        return df_query
    
    # groupby = dataframe.groupby(list_cols).count()

    # if groupby.where(groupby > 1).isnull().values.all():
    #     print('No Duplicated Observations given the column(s).')
    # else:
    #     return groupby.where(groupby > 1)


# ------- To generate an empty panel DF -------
def makeEmptyPanel(date_from: str, date_to: str, frequency: str):
    idx = pd.date_range(start= date_from, end= date_to, freq= frequency)
    emptyPanel = pd.DataFrame({'date': idx})
    emptyPanel.set_index('date', inplace= True)

    return emptyPanel


# ------- To generate folders that are defined but not exsting -------
def makeFolder(list_paths = []):
    '''
    To make defined folder if not exist
    '''
    for path in list_paths:
        if not os.path.exists(path):
            os.makedirs(path)


# ------- To load a DF from a Parquet file -------
def load_fromPq(path):
    # # To load a Arrow Table
    pa_table = papq.read_table(
        path,
        read_dictionary= pd.read_parquet(path).columns
    )
    dataframe = pa.Table.to_pandas(pa_table)

    return dataframe


# ------- To print Datetime -------
def printDt(str_in: str = None):
    '''
    To Print Datatime, with String typed.
    '''
    dt_inStr = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime())

    if isinstance(str_in, str):
        print(dt_inStr + ' - ' + str_in)
    else:
        print(dt_inStr)


# ------- To save a DF as a Parquet file -------
def save_toPq(dataframe, path, preserve_idx= False):
    # # To convert DF's class type from "modin.pandas.dataframe" to
    # # "pandas.core.dataframe"
    # ## Note (on 2020-08-19)
    # ## : "modin.pandas.dataframe" cannot be directly converted to Parquet
    # ##   table.

    # df_class = str(type(dataframe))
    # if re.search('modin', df_class):
    #     dataframe = dataframe._to_pandas()

    # # To convert DF to Arrow Table
    data_inArrowTable = \
        pa.Table.from_pandas(dataframe, preserve_index= preserve_idx)

    # # To write the Arrow Table to Parquet file
    papq.write_table(
        data_inArrowTable,
        where= path,
        version= '2.0',
        compression= 'GZIP',
        flavor= 'spark'
    )


# ------- To ... -------
# (...)

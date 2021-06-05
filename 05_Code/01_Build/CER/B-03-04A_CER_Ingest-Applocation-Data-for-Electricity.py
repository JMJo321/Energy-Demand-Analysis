#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-04A

> Purpose of Script
  : Ingest Allocation Data for Electricity

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

import numpy as np
import pandas as pd

# # 2. PySpark libraries
import pyspark
from pyspark.sql import Column
from pyspark.sql import DataFrame
from pyspark.sql import SQLContext
from pyspark.sql.types import *
from pyspark.sql.functions import *

# # 3. Additional libraries used in this script
import re
from itertools import chain


# ------- Load user-written libraries -------
# # 1. Add a path to "sys.path", and change working directory if necessary
if os.getcwd() == '/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis':
    sys.path.append(os.path.abspath('../Energy-Demand-Analysis/05_Code'))
else:
    path_toAdd = pl.Path.cwd().parents[1] # TODO Must add an appropriate number
    sys.path.append(str(path_toAdd))
    path_project = pl.Path.cwd().parents[2] # TODO Must add an appropriate number
    os.chdir(path_project)

# # 2. Load libraries
HD = importlib.import_module('H-Energy-Demand-Analysis')
FNC = importlib.import_module('F-Energy-Demand-Analysis_Common-Functions')
SPARK = importlib.import_module('S-Energy-Demand-Analysis_PySpark')


# --------------------------------------------------
# Set path(s) and parameter(s)
# --------------------------------------------------
# ------- Set path(s) -------
# # 1. Path(s) from which data file(s) will be loaded
DIR_TO_LOAD_CER_ALLOCATION_ELECTRICITY = 'CER_Electricity_Documentation'
FILE_TO_LOAD_CER_ALLOCATION_ELECTRICITY = 'SME and Residential allocations.xlsx'
PATH_TO_LOAD_CER_ALLOCATION_ELECTRICITY = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_ELECTRICITY,
    DIR_TO_LOAD_CER_ALLOCATION_ELECTRICITY,
    FILE_TO_LOAD_CER_ALLOCATION_ELECTRICITY
)

# # 2. Path(s) at which data file(s) will be saved
DIR_TO_SAVE_CER_ALLOCATION_ELECTRICITY = 'Allocation'
FILE_TO_SAVE_CER_ALLOCATION_ELECTRICITY = 'CER_Allocation_Electricity.parquet'
PATH_TO_SAVE_CER_ALLOCATION_ELECTRICITY = os.path.join(
    HD.PATH_DATA_INTERMEDIATE_CER,
    DIR_TO_SAVE_CER_ALLOCATION_ELECTRICITY,
    FILE_TO_SAVE_CER_ALLOCATION_ELECTRICITY
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-04A'

# # 1. A str for dealing with empty cells
ARBITRARY_INT_IN_STR = '-999'

# # 2. Dictionaries that include codes with
# #    (`simple description`, `detailed description`)
alloc_group = {
    '1': ('Residential', 'Residential',),
    '2': ('SME', 'SME',),
    '3': ('Other', 'Other',)
}
alloc_r_stimulus = {
    'E': ('Control', 'Control',),
    '1': ('Bi-Monthly Bill', 'Bi-Monthly Detailed Bill',),
    '2': ('Monthly Bill', 'Monthly Detailed Bill',),
    '3': (
        'Bi-Monthly Bill + IHD',
        'Bi-Monthly Detailed Bill and In-Home Display (IHD)',
    ),
    '4': (
        'Bi-Monthly Bill + OLR',
        'Bi-Monthly Detailed Bill and Overall Load Reduction (OLR) Incentive',
    )
}
alloc_r_tariff = {
    'E': ('Control', 'Control',),
    'A': ('Tariff A', 'Tariff A',),
    'B': ('Tariff B', 'Tariff B',),
    'C': ('Tariff C', 'Tariff C',),
    'D': ('Tariff D', 'Tariff D',),
    'W': ('Weekend Tariff', 'Weekend Tariff',)
}
alloc_sme = {
    '1': ('Monthly Bill', 'Monthly Detailed Bill',),
    '2': (
        'Bi-Monthly Bill + IOD',
        'Bi-Monthly Detailed Bill and In-Office Display (IOD)',
    ),
    '3': (
        'Bi-Monthly Bill + WA',
        'Bi-Monthly Detailed Bill and Web Access to Energy Usage Info.',
    ),
    '4': ('Bi-Monthly Bill', 'Bi-Monthly Detailed Bill',),
    'C': ('Control', 'Control',)
}


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Deploy PySpark
# --------------------------------------------------
# ------- Deploy PySpark -------
# # 1. Spark Context, SQL Context, and Session
sc = pyspark.SparkContext(conf=SPARK.pyspark_conf)
sqlc = pyspark.SQLContext(sc)
spark = sqlc.sparkSession


# --------------------------------------------------
# Ingest Allocation Data for Electricity
# --------------------------------------------------
# ------- Create a Spark DF by importing an Excel file -------
# # 1. Import an Excel file
sheet_toRead = 'Sheet1'
col_names = [
    'id', 'alloc_group', 'alloc_r_tariff', 'alloc_r_stimulus', 'alloc_sme'
]
pdf_alloc_electricity = pd.read_excel(
    PATH_TO_LOAD_CER_ALLOCATION_ELECTRICITY,
    sheet_name=sheet_toRead,
    usecols='A:E',
    names=col_names,
    dtype='object',
    keep_default_na=False
)

# # 2. Create a Spark DF from the Pandas DF above
# # 2.1. Process data before creating a Spark DF
for c in pdf_alloc_electricity.columns:
    pdf_alloc_electricity[c] = \
        pdf_alloc_electricity[c].apply(lambda row: str(row).upper().strip())
    pdf_alloc_electricity[c] = \
        pdf_alloc_electricity[c].replace('', ARBITRARY_INT_IN_STR)
# # 2.2. Create a Spark DF
df_alloc_electricity = spark.createDataFrame(pdf_alloc_electricity)


# ------- Modify the Spark DF created above -------
# # 1. Change values
# ## Note:
# ## Previously, empty cells were filled with an arbitrary integer.
df_alloc_electricity = (
    df_alloc_electricity.select(
        [
            (
                when(col(c) == ARBITRARY_INT_IN_STR, None)
                    .otherwise(col(c))
                    .alias(c)
            )
            for c in df_alloc_electricity.columns
        ]
    )
)

# # 2. Add columns showing descriptions for codes
for c in df_alloc_electricity.columns[1:]:
    col_name = c + '_desc'
    mapping_codes = \
        create_map([lit(x[0]) for x in chain(*globals()[c].items())])
    df_alloc_electricity = \
        df_alloc_electricity.withColumn(col_name, mapping_codes[col(c)])

# # 3. Change data type of a column from string to integer
df_alloc_electricity = \
    df_alloc_electricity.withColumn('id', col('id').cast(IntegerType()))


# # 4. Re-order columns and rows
# # 4.1. Re-order columns
tmp_colNames = []
for c in col_names[1:]:
    tmp_colNames.append(c)
    tmp_colNames.append(c + '_desc')
cols_reorder = ['id'] + tmp_colNames
df_alloc_electricity = df_alloc_electricity.select(cols_reorder)

# # 4.2. Re-order rows
df_alloc_electricity = df_alloc_electricity.orderBy('id', ascending=True)


# --------------------------------------------------
# Save the Spark DF
# --------------------------------------------------
# ------- Save the Spark DF in Parquet format -------
pdf_alloc_electricity = df_alloc_electricity.toPandas()
FNC.save_toPq(
    pdf_alloc_electricity,
    path=PATH_TO_SAVE_CER_ALLOCATION_ELECTRICITY,
    preserve_idx=False
)



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

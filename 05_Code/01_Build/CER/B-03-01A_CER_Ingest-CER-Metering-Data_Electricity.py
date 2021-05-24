#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-01A

> Purpose of Script
  : To ingest CER Metering Data for Electricity

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
import shutil
import zipfile
from functools import reduce
from tqdm import tqdm

import pyspark
from pyspark.sql import Column
from pyspark.sql import DataFrame
from pyspark.sql import SQLContext
from pyspark.sql.types import *
from pyspark.sql.functions import *


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
SPARK = importlib.import_module('S-Energy-Demand-Analysis_PySpark')


# --------------------------------------------------
# Set path(s) and parameter(s)
# --------------------------------------------------
# ------- Set path(s) -------
# # 1. Path(s) from which data file(s) will be loaded
# (NOT Applicable)

# # 2. Path(s) at which data file(s) will be saved
PATH_TOSAVE = os.path.join(
    HD.PATH_DATA_INTERMEDIATE_CER, 'CER_Metering_Electricity.parquet'
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-01A'


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

# --------------------------------------------------
# Make a list of TXT files that will be ingested
# --------------------------------------------------
# ------- Make a list of TXT files -------
# # 1. Create a list of ZIP files
paths_zipfile = []
for roots, dirs, files in os.walk(HD.PATH_DATA_RAW_USE_CER_ELECTRICITY):
    for f in files:
        if f.endswith('.zip'):
            path_f = os.path.join(roots, f)
            if path_f not in paths_zipfile:
                paths_zipfile.append(path_f)

# # 2. Extract ZIP files
for path in paths_zipfile:
    zipfile.ZipFile(path).extractall(path= HD.PATH_TMP)

# # 3. Create a list of TXT files
paths_txt = []
for roots, dirs, files in os.walk(HD.PATH_TMP):
    for f in files:
        if f.endswith('.txt'):
            path_f = os.path.join(roots, f)
            if path_f not in paths_txt:
                paths_txt.append(path_f)


# --------------------------------------------------
# Ingest TXT files
# --------------------------------------------------
# ------- Create Spark Contexts -------
sc = pyspark.SparkContext(conf=SPARK.pyspark_conf)
sqlc = SQLContext(sc)


# ------- Ingest TXT files -------
# # 1. Define a schema
customSchema = StructType([
    StructField("id", IntegerType(), nullable=True),
    StructField("daytime", StringType(), nullable=True),
    StructField("kwh", FloatType(), nullable=True)
])


# # 2. Ingest TXT files
for txt in tqdm(paths_txt):
    # ## Make temporary objects
    tmp_filename = re.search('File[0-9].txt', txt)[0]
    tmp_number = re.search('[0-9]', tmp_filename)[0]
    tmp_objName = 'tmp_df_' + str(tmp_number)

    # ## Import a TXT file
    globals()[tmp_objName] = (
        sqlc.read
            .format('csv')
            .option('sep', ' ')
            .option('enforceSchema', 'true')
            .schema(customSchema)
            .load(path= txt)
    )

    # ## Modify the DF
    globals()[tmp_objName] = (
        globals()[tmp_objName]
            .withColumn('day', substring('daytime', 1, 3)
            .cast(IntegerType()))
    )
    globals()[tmp_objName] = (
        globals()[tmp_objName]
            .withColumn('interval_30min', substring('daytime', 4, 2)
            .cast(IntegerType()))
    )
    globals()[tmp_objName] = (
        globals()[tmp_objName]
            .withColumn(
                'tmp_date_reference',
                to_date(lit('2009-01-01'),
                format= 'yyyy-MM-dd')
            )
    )
    globals()[tmp_objName] = (
        globals()[tmp_objName]
            .withColumn(
                'date',
                expr('date_add(tmp_date_reference, day)')
            )
    )
    globals()[tmp_objName] = (
        globals()[tmp_objName].drop('daytime', 'tmp_date_reference')
    )


# ------- Concatenate DFs ingested, and then modify the DF -------
# # 1. Concatenate DFs ingested
# # 1.1. Make a list of DFs ingested
list_df_toJoin = [var for var in dir() if re.match('tmp_df_', var)]
dfs = [globals()[var] for var in list_df_toJoin]

# # 1.2. Concatenate DFs ingested
def unionAll(*dfs):
    return reduce(DataFrame.unionAll, dfs)

df = unionAll(*dfs)


# # 2. Modify the DF
# # 2.1. Sort the DF
cols_toOrder = ['id', 'day', 'interval_30min']
df = df.orderBy(cols_toOrder, ascending=True)

# # 2.2. Re-order columns
df = df.select('id', 'day', 'date', 'interval_30min', 'kwh')


# --------------------------------------------------
# Save the DF in Parquet format
# --------------------------------------------------
# ------- Save the DF in Parquet format -------
pdf = df.toPandas()
FNC.save_toPq(pdf, path=PATH_TOSAVE, preserve_idx=False)



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

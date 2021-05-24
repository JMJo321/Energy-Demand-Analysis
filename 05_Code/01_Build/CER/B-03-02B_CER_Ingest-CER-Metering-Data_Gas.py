#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-02B

> Purpose of Script
  : To ingest CER Metering Data for Natural Gas

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
PATH_TOLOAD = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS, 'CER_Metering_Gas.csv'
)

# # 2. Path(s) at which data file(s) will be saved
PATH_TOSAVE = os.path.join(
    HD.PATH_DATA_INTERMEDIATE_CER, 'CER_Metering_Gas.parquet'
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-02B'


FNC.printDt('Work begins: ' + SCRIPT_NO + '\n')

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
# ## Import a CSV file
df = (
    sqlc.read
        .format('csv')
        .option('sep', ',')
        .option('enforceSchema', 'true')
        .schema(customSchema)
        .load(path= PATH_TOLOAD)
)

# ## Modify the DF
df = (
    df.withColumn('day', substring('daytime', 1, 3).cast(IntegerType()))
)
df = (
    df.withColumn('interval_30min', substring('daytime', 4, 2)
        .cast(IntegerType()))
)
df = (
    df.withColumn(
        'tmp_date_reference',
        to_date(lit('2009-01-01'),
        format= 'yyyy-MM-dd')
    )
)
df = (
    df.withColumn(
        'date',
        expr('date_add(tmp_date_reference, day)')
    )
)
df = df.drop('daytime', 'tmp_date_reference')


# ------- Modify the DF ingested -------
# # 1. Modify the DF
# # 1.1. Sort the DF
cols_toOrder = ['id', 'day', 'interval_30min']
df = df.orderBy(cols_toOrder, ascending=True)

# # 1.2. Re-order columns
df = df.select('id', 'day', 'date', 'interval_30min', 'kwh')


# --------------------------------------------------
# Save the DF in Parquet format
# --------------------------------------------------
# ------- Save the DF in Parquet format -------
pdf = df.toPandas()
FNC.save_toPq(pdf, path=PATH_TOSAVE, preserve_idx=False)



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

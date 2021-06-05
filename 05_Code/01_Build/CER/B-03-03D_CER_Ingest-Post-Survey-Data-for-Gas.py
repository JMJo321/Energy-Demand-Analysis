#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-03D

> Purpose of Script
  : Ingest Residential Post Survey Dataset for Natural Gas

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
import functools
from itertools import chain
from tqdm import tqdm


# ------- Load user-written libraries -------
# # 1. Add a path to "sys.path", and change working directory if necessary
if os.getcwd() == '/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis':
    sys.path.append(os.path.abspath('../Energy-Demand-Analysis/05_Code'))
    sys.path.append(
        os.path.abspath('../Energy-Demand-Analysis/05_Code/01_Build/CER')
    )
    # ## Note:
    # ## To import a dictionary that maps from old column names to new column
    # ## names.
else:
    path_toAdd = pl.Path.cwd().parents[1]
    sys.path.append(str(path_toAdd))
    path_project = pl.Path.cwd().parents[2]
    os.chdir(path_project)

# # 2. Load libraries
HD = importlib.import_module('H-Energy-Demand-Analysis')
FNC = importlib.import_module('F-Energy-Demand-Analysis_Common-Functions')
SPARK = importlib.import_module('S-Energy-Demand-Analysis_PySpark')
DICTIONARY = importlib.import_module(
    'B-03-03D_CER_Dictionary-for-Column-Name-Conversion'
)


# --------------------------------------------------
# Set path(s) and parameter(s)
# --------------------------------------------------
# ------- Set path(s) -------
# # 1. Path(s) from which data file(s) will be loaded
# # 1.1. For survey data
DIR_TO_LOAD_CER_SURVEY_GAS = \
    'CER_Gas_Data'
FILE_TO_LOAD_CER_SURVEY_GAS_POST = \
    'Smart meters Residential post-trial survey data - Gas.xls'
PATH_TO_LOAD_CER_SURVEY_GAS_POST = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS,
    DIR_TO_LOAD_CER_SURVEY_GAS,
    FILE_TO_LOAD_CER_SURVEY_GAS_POST
)

# # 1.2. For survey document
DIR_TO_LOAD_CER_SURVEY_GAS_DOC = 'CER_Gas_Documentation'
FILE_TO_LOAD_CER_SURVEY_GAS_DOC_POST = \
    'RESIDENTIAL-POST-TRIAL-SURVEY_GAS_Modified.xlsx'
PATH_TO_LOAD_CER_SURVEY_GAS_DOC_POST = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS,
    DIR_TO_LOAD_CER_SURVEY_GAS_DOC,
    FILE_TO_LOAD_CER_SURVEY_GAS_DOC_POST
)


# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE_CER_SURVEY_GAS_POST = \
    'CER_Survey_Gas_Post.parquet'
PATH_TO_SAVE_CER_SURVEY_GAS_POST = os.path.join(
    HD.PATH_DATA_INTERMEDIATE_CER,
    'Survey',
    FILE_TO_SAVE_CER_SURVEY_GAS_POST
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-03D'


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
# Create a DF by importing Survey Data
# --------------------------------------------------
# ------- Create a DF by importing Survey Data -------
# # 1. Import raw data file
sheet_read_data = 'Sheet1'
pdf_survey_gas_post_data = pd.read_excel(
    PATH_TO_LOAD_CER_SURVEY_GAS_POST,
    sheet_name=sheet_read_data,
    dtype='object',
    keep_default_na=False
)


# # 2. Modify the Pandas DF imported
# # 2.1. Rename columns
names_questions_old = pdf_survey_gas_post_data.columns
names_questions_new = \
    [DICTIONARY.names_toConvert[name] for name in names_questions_old]
for i in range(0, len(names_questions_old)):
    pdf_survey_gas_post_data.rename(
        columns={names_questions_old[i]: names_questions_new[i]},
        inplace=True
    )

# # 2.2. Drop unnecessary columns
# ## Note:
# ## Keep columns that exist in other data file only.
cols_drop = \
    [col for col in names_questions_new if not re.search('(id)|(q_id)', col)]
pdf_survey_gas_post_data.drop(columns=cols_drop, inplace=True)

# # 2.3. Remove whitespaces
# ## Note:
# ## This task is necessary to avoid any error in the middle of joining work.
for c in pdf_survey_gas_post_data.columns:
    pdf_survey_gas_post_data[c] = \
        pdf_survey_gas_post_data[c].apply(lambda row: str(row).strip())

# # 2.4. Fill empty cells with an arbitrary integer
# ## Note:
# ## This task is necessary to convert data type from string to integer after
# ## this Pandas DF is converted to Spark DF.
for c in pdf_survey_gas_post_data.columns:
    pdf_survey_gas_post_data[c] = \
        pdf_survey_gas_post_data[c].replace('', '-999')


# # 3. Melt the Pandas DF
id_vars = ['id']
value_vars = \
    [col for col in pdf_survey_gas_post_data.columns if col not in id_vars]
pdf_survey_gas_post_data_melted = \
    pdf_survey_gas_post_data.melt(
        id_vars=id_vars, value_vars=value_vars,
        var_name='question_id', value_name='answer_code'
    )


# # 4. Create a Spark DF from the Pandas DF above
df_survey_gas_post_data = \
    spark.createDataFrame(pdf_survey_gas_post_data_melted)


# ------- Modify the DF -------
# # 1. Add column(s)
# # 1.1. Add a column showing the data imported is for natural gas
df_survey_gas_post_data = (
    df_survey_gas_post_data
        .withColumn('utility', lit('gas'))
)

# # 1.2. Add a column showing the data imported is for post survey
df_survey_gas_post_data = \
    df_survey_gas_post_data.withColumn('timing', lit('post'))


# # 2. Change columns' values
# # 2.1. Remove non-numeric part from values
df_survey_gas_post_data = (
    df_survey_gas_post_data
        .withColumn(
            'question_id', regexp_replace(col('question_id'), 'q_id_', '')
    )
)

# # 2.2. Fill empty cells (filled with -999 currently) with None values
df_survey_gas_post_data = (
    df_survey_gas_post_data
        .withColumn(
            'answer_code',
            when(col('answer_code') == '-999', None)
                .otherwise(col('answer_code'))
        )
)

# # 2.3. Convert columns' types
cols_convert = ['id', 'answer_code']
# ## Note
# ## : `question_id` cannot be converted to IntegerType because of question
# ## IDs that have the form of "[0-9].+?_[0-9]".
df_survey_gas_post_data = (
    functools.reduce(
        lambda df_survey_gas_post_data, idx:
        df_survey_gas_post_data.withColumn(
            cols_convert[idx], col(cols_convert[idx]).cast(IntegerType())
        ),
        range(len(cols_convert)),
        df_survey_gas_post_data
    )
)


# --------------------------------------------------
# Create a DF by importing Survey Document
# --------------------------------------------------
# ------- Create a DF by importing survey document -------
# # 1. Import an Excel file
sheet_read = 'RESIDENTIAL POST TRIAL SURVEY'
pdf_survey_gas_post_doc = pd.read_excel(
    PATH_TO_LOAD_CER_SURVEY_GAS_DOC_POST,
    sheet_name=sheet_read,
    names=['q_id', 'answer_code', 'answer_desc', 'question_desc'],
    usecols='A:D',
    dtype='object',
    keep_default_na=False
)


# # 2. Modify the Pandas DF imported
# # 2.1. Remove whitespaces
for c in pdf_survey_gas_post_doc.columns:
    pdf_survey_gas_post_doc[c] = \
        pdf_survey_gas_post_doc[c].apply(lambda row: str(row).strip())

# # 2.2. Fill empty cells with an arbitrary integer
for c in pdf_survey_gas_post_doc.columns:
    pdf_survey_gas_post_doc[c] = pdf_survey_gas_post_doc[c].replace('', '-999')


# # 3. Create a Spark DF from the Pandas DF above
df_survey_gas_post_doc = spark.createDataFrame(pdf_survey_gas_post_doc)


# ------- Modify the DF -------
# # 1. Add a column from an existing column
df_survey_gas_post_doc = (
    df_survey_gas_post_doc
        .withColumn(
            'question_id', lower(regexp_replace(col('q_id'), 'QUESTION ', ''))
        )
        .drop('q_id')
)

# # 2. Fill empty cells (filled with -999 currently) with None values
# ## Note:
# ## Previously, empty cells were filled out with -999.
cols_refill = ['question_desc', 'answer_code', 'answer_desc']
for c in cols_refill:
    df_survey_gas_post_doc = (
        df_survey_gas_post_doc
            .withColumn(
                c, when(trim(col(c)) == '-999', None).otherwise(col(c))
            )
)

# # 3. Convert data type(s) from string to integer
cols_convert = ['answer_code']
df_survey_gas_post_doc = (
    functools.reduce(
        lambda df_survey_gas_post_doc, idx:
            df_survey_gas_post_doc.withColumn(
                cols_convert[idx], col(cols_convert[idx]).cast(IntegerType())
            ),
            range(len(cols_convert)),
            df_survey_gas_post_doc
    )
)


# --------------------------------------------------
# Join Survey Data and Document DFs
# --------------------------------------------------
# ------- Join DFs -------
# # 1. Join DFs
df_survey_gas_post = (
    df_survey_gas_post_data
        .join(
            df_survey_gas_post_doc.select(
                'question_id', 'question_desc', 'answer_code', 'answer_desc'
            ),
            on=['question_id', 'answer_code'],
            how='leftouter'
        )
)

# # 2. Re-order columns and rows
# # 2.1. Re-order columns
cols_toOrder = [
    'id', 'utility', 'timing', 'question_id', 'question_desc', 'answer_code',
    'answer_desc'
]
df_survey_gas_post = df_survey_gas_post.select(cols_toOrder)

# # 2.2. Re-order rows
cols_toOrder = ['id', 'question_id', 'answer_code']
df_survey_gas_post = \
    df_survey_gas_post.orderBy(cols_toOrder, ascending=True)


# --------------------------------------------------
# Save the DF
# --------------------------------------------------
# ------- Save the DF in Parquet Format -------
pdf_survey_gas_post = df_survey_gas_post.toPandas()
FNC.save_toPq(
    pdf_survey_gas_post,
    path=PATH_TO_SAVE_CER_SURVEY_GAS_POST,
    preserve_idx=False
)



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

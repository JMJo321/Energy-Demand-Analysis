#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : B-03, CER

> Script Number(s)
  : B-03-03C

> Purpose of Script
  : Ingest Residential Pre Survey Dataset for Natural Gas

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
# # 1.1. For survey data
DIR_TO_LOAD_CER_SURVEY_GAS = \
    'CER_Gas_Data'
FILE_TO_LOAD_CER_SURVEY_GAS_PRE = \
    'Smart meters Residential pre-trial survey data - Gas.csv'
PATH_TO_LOAD_CER_SURVEY_GAS_PRE = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS,
    DIR_TO_LOAD_CER_SURVEY_GAS,
    FILE_TO_LOAD_CER_SURVEY_GAS_PRE
)

# # 1.2. For survey document
DIR_TO_LOAD_CER_SURVEY_GAS_DOC = 'CER_Gas_Documentation'
FILE_TO_LOAD_CER_SURVEY_GAS_DOC_PRE = \
    'RESIDENTIAL-PRE-TRIAL-SURVEY_GAS_Modified.xlsx'
PATH_TO_LOAD_CER_SURVEY_GAS_DOC_PRE = os.path.join(
    HD.PATH_DATA_RAW_USE_CER_GAS,
    DIR_TO_LOAD_CER_SURVEY_GAS_DOC,
    FILE_TO_LOAD_CER_SURVEY_GAS_DOC_PRE
)


# # 2. Path(s) at which data file(s) will be saved
FILE_TO_SAVE_CER_SURVEY_GAS_PRE = \
    'CER_Survey_Gas_Pre.parquet'
PATH_TO_SAVE_CER_SURVEY_GAS_PRE = os.path.join(
    HD.PATH_DATA_INTERMEDIATE_CER,
    'Survey',
    FILE_TO_SAVE_CER_SURVEY_GAS_PRE
)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = 'B-03-03C'


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
df_survey_gas_pre_data = (
    sqlc.read
        .format('csv')
        .option('header', True)
        .load(PATH_TO_LOAD_CER_SURVEY_GAS_PRE)
)


# # 2. Rename columns
# # 2.1. Make a list of new names
names_questions_old = df_survey_gas_pre_data.columns[2:]
# ## Note:
# ## To exclude the name of the first two columns, i.e., `ID` and
# ## `Stimulus`
names_questions_id = []
tmp_names_questions_desc = []
for name in names_questions_old:
    str_q_id = re.search('^Q[a-z]+\s[0-9]+', name)[0]
    q_id = re.sub('uestion ', '_id_', str_q_id.lower())
    q_desc = re.sub('(^Q[a-z]+\s[0-9]+:\s)|(^Q[a-z]+\s[0-9]+)', '', name)
    names_questions_id.append(q_id)
    tmp_names_questions_desc.append(q_desc)

# # 2.2. Deal with questions that have the same question number with different
# #      descriptions
q_id_count = {id: names_questions_id.count(id) for id in names_questions_id}
q_id_rename = \
    {id: [q_id_count[id]] * 2 for id in q_id_count if q_id_count[id] > 1}
# ## Note: [Total number of columns, Number of remaining columns]
for i in range(0, len(names_questions_id)):
    tmp_id = names_questions_id[i]
    if tmp_id in q_id_rename:
        tmp_id_rename = (
            tmp_id + '_' +
                str(q_id_rename[tmp_id][0] - q_id_rename[tmp_id][1] + 1)
        )
        names_questions_id[i] = tmp_id_rename
        q_id_rename[tmp_id][1] -= 1

# # 2.3. Rename columns
# # 2.3.1. For `ID` and `Stimulus` columns
df_survey_gas_pre_data = (
    df_survey_gas_pre_data
        .withColumnRenamed('ID', 'id')
        .withColumnRenamed('Stimulus', 'stimulus')
)

# # 2.3.2. For remaining columns
df_survey_gas_pre_data = functools.reduce(
    lambda df_survey_gas_pre_data, idx:
    df_survey_gas_pre_data.withColumnRenamed(
        names_questions_old[idx], names_questions_id[idx]
    ),
    range(len(names_questions_old)),
    df_survey_gas_pre_data
)


# # 3. Reshape the DF from wide form to long one
# # 3.1. Create a Pandas DF from the Spark DF
pdf_survey_gas_pre_data = df_survey_gas_pre_data.toPandas()

# # 3.2. Melt the Pandas DF
id_vars = df_survey_gas_pre_data.columns[0:2]
value_vars = [
    col for col in df_survey_gas_pre_data.columns if col not in id_vars
]
pdf_survey_gas_pre_data_melted = \
    pdf_survey_gas_pre_data.melt(
        id_vars=id_vars, value_vars=value_vars,
        var_name='question_id', value_name='answer_code'
    )

# # 3.3. Transform the melted Pandas DF to a Spark DF
df_survey_gas_pre_data_melted = \
    spark.createDataFrame(pdf_survey_gas_pre_data_melted)


# ------- Modify the DF -------
# # 1. Add column(s)
# # 1.1. Add a column showing the data imported is for electricity
df_survey_gas_pre_data_melted = (
    df_survey_gas_pre_data_melted
        .withColumn('utility', lit('gas'))
)

# # 1.2. Add a column showing the data imported is for post survey
df_survey_gas_pre_data_melted = \
    df_survey_gas_pre_data_melted.withColumn('timing', lit('pre'))

# # 1.3. Add a column including survey question descriptions
# # 1.3.1. Make a list containing survey question descriptions
names_questions_desc = {}
for i in range(0, len(names_questions_id)):
    id = names_questions_id[i]
    names_questions_desc[id] = tmp_names_questions_desc[i]
# # 1.3.2. Add a column by using MapType
mapping_questionDesc = \
    create_map([lit(x) for x in chain(*names_questions_desc.items())])
df_survey_gas_pre_data_melted = (
    df_survey_gas_pre_data_melted
        .withColumn('question_desc', mapping_questionDesc[col('question_id')])
)
# # 1.3.3. Remove non-numeric part from values
df_survey_gas_pre_data_melted = (
    df_survey_gas_pre_data_melted
        .withColumn(
            'question_id', regexp_replace(col('question_id'), 'q_id_', '')
    )
)


# # 2. Fill empty cells with None values
df_survey_gas_pre_data_melted = (
    df_survey_gas_pre_data_melted
        .select(
            [
                when(col(c) == '', None)
                    .otherwise(col(c))
                    .alias(c) for c in df_survey_gas_pre_data_melted.columns
            ]
        )
        .select(
            [
                when(col(c) == ' ', None)
                    .otherwise(col(c))
                    .alias(c) for c in df_survey_gas_pre_data_melted.columns
            ]
        )
)


# # 3. Convert columns' types
cols_convert = ['id', 'answer_code']
# ## Note
# ## : `question_id` cannot be converted to IntegerType because of question
# ## IDs that have the form of "[0-9].+?_[0-9]".
df_survey_gas_pre_data_melted = (
    functools.reduce(
        lambda df_survey_gas_pre_data_melted, idx:
        df_survey_gas_pre_data_melted.withColumn(
            cols_convert[idx], col(cols_convert[idx]).cast(IntegerType())
        ),
        range(len(cols_convert)),
        df_survey_gas_pre_data_melted
    )
)


# --------------------------------------------------
# Create a DF by importing Survey Document
# --------------------------------------------------
# ------- Create a DF by importing survey document -------
# # 1. Import an Excel file
sheet_read = 'RESIDENTIAL PRE TRIAL SURVEY'
pdf_survey_gas_pre_doc = pd.read_excel(
    PATH_TO_LOAD_CER_SURVEY_GAS_DOC_PRE,
    sheet_name=sheet_read,
    names=['q_no', 'answer_code', 'answer_desc'],
    dtype={'q_no': str, 'answer_code': str, 'answer_desc': str}
)

# # 2. Change values of columns
# # 2.1. Change NaN to an arbitrary integer
# ## Note:
# ## This task is necessary to convert the type of `answer_code` from string
# ## to integer after this Pandas DF is converted to Spark DF.
cols_convert = ['answer_code', 'answer_desc']
for c in cols_convert:
    pdf_survey_gas_pre_doc[c] = \
        pdf_survey_gas_pre_doc[c].replace(' ', '-999')
    pdf_survey_gas_pre_doc[c] = \
        pdf_survey_gas_pre_doc[c].replace(np.nan, '-999')
# pdf_survey_gas_pre_doc['answer_code'] = \
#     pdf_survey_gas_pre_doc['answer_code'].replace(np.nan, '-999')

# # 2.2. Remove whitespaces
# ## Note:
# ## This task is necessary to avoid any error in the middle of joining work.
pdf_survey_gas_pre_doc['q_no'] = \
    pdf_survey_gas_pre_doc['q_no'].apply(lambda row: row.strip())
pdf_survey_gas_pre_doc['answer_code'] = (
    pdf_survey_gas_pre_doc['answer_code']
        .apply(lambda row: row.strip())
)

# # 2.3. Add whitespaces
# ## Note:
# ## To keep the data type (i.e., string) of `answer_desc`
pdf_survey_gas_pre_doc['answer_desc'] = (
    pdf_survey_gas_pre_doc['answer_desc']
        .apply(lambda row: ' ' + str(row))
)

# # 3. Create a Spark DF from the Pandas DF above
df_survey_gas_pre_doc = \
    spark.createDataFrame(pdf_survey_gas_pre_doc)


# ------- Modify the DF -------
# # 1. Add a column from an existing column
df_survey_gas_pre_doc = (
    df_survey_gas_pre_doc
        .withColumn(
        'question_id', regexp_replace(col('q_no'), 'QUESTION ', '')
    ).drop('q_no')
)

# # 2. Fill empty cells with None values
# ## Note:
# ## Previously, empty cells were filled out with -999.
cols_refill = ['answer_code', 'answer_desc']
for c in cols_refill:
    df_survey_gas_pre_doc = (
        df_survey_gas_pre_doc
            .withColumn(
                c, when(trim(col(c)) == '-999', None).otherwise(col(c))
                # ## Note: Remember that ' ' was added to keep type as string.
            )
)

# # 3. Convert data type(s)
cols_convert = ['answer_code']
df_survey_gas_pre_doc = (
    functools.reduce(
        lambda df_survey_gas_pre_doc, idx:
            df_survey_gas_pre_doc.withColumn(
                cols_convert[idx], col(cols_convert[idx]).cast(IntegerType())
            ),
            range(len(cols_convert)),
            df_survey_gas_pre_doc
    )
)


# --------------------------------------------------
# Join Survey Data and Document DFs
# --------------------------------------------------
# ------- Join DFs -------
# # 1. Make a temporary column in each DF
df_survey_gas_pre_data_melted = (
    df_survey_gas_pre_data_melted
        .withColumn(
            'tmp_question_id',
            regexp_replace(col('question_id'), '(?>_.+$)', '')
        )
)
# ## Note:
# ## Questions that have the same ID but have different description have the
# ## same answer code and description.
df_survey_gas_pre_doc = (
    df_survey_gas_pre_doc
        .withColumn('tmp_question_id', col('question_id'))
)
# ## Note:
# ## `tmp_question_id` and `question_id` are identical. The purpose of
# ## `tmp_question_id` is only for joining DFs.

# # 2. Join DFs
df_survey_gas_pre = (
    df_survey_gas_pre_data_melted
        .join(
            df_survey_gas_pre_doc.select(
                'tmp_question_id', 'answer_code', 'answer_desc'
            ),
            on=['tmp_question_id', 'answer_code'],
            how='leftouter'
        )
        .drop('tmp_question_id')
)

# # 3. Re-order columns and rows
# # 3.1. Re-order columns
cols_toOrder = [
    'id', 'utility', 'timing', 'question_id', 'question_desc',
    'answer_code', 'answer_desc'
]
# ## Note:
# ## Drop `stimulus`.
df_survey_gas_pre = df_survey_gas_pre.select(cols_toOrder)

# # 3.2. Re-order rows
cols_toOrder = ['id', 'question_id', 'answer_code']
df_survey_gas_pre = \
    df_survey_gas_pre.orderBy(cols_toOrder, ascending=True)


# --------------------------------------------------
# Save the DF
# --------------------------------------------------
# ------- Save the DF in Parquet Format -------
pdf_survey_gas_pre = df_survey_gas_pre.toPandas()
FNC.save_toPq(
    pdf_survey_gas_pre,
    path=PATH_TO_SAVE_CER_SURVEY_GAS_PRE,
    preserve_idx=False
)



FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

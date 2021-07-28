#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : 

> Script Number(s)
  : 

> Purpose of Script
  : 

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
from tqdm import tqdm


# ------- Load user-written libraries -------
# # 1. Add a path to "sys.path", and change working directory if necessary
if os.getcwd() == '/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis':
    sys.path.append(os.path.abspath('../Energy-Demand-Analysis/05_Code'))
else:
    path_toAdd = pl.Path.cwd().parents[] # TODO Must add an appropriate number
    sys.path.append(str(path_toAdd))
    path_project = pl.Path.cwd().parents[] # TODO Must add an appropriate number
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
# (...)

# # 2. Path(s) at which data file(s) will be saved
# (...)


# ------- Set parameter(s) -------
# # 0. Basic Parameters
# # 0.1. Script Number
SCRIPT_NO = ''


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
# ...
# --------------------------------------------------
# ------- ... -------
# (...)

FNC.printDt('Work ends: ' + SCRIPT_NO + '\n')

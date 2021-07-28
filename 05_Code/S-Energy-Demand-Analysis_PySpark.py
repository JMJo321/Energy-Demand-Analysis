#-*- coding: utf-8 -*-

"""
* Description *
> Script Group Indicator Number and Name
  : (NA)

> Script Number(s)
  : (NA)

> Purpose of Script
: To set configurations of Spark Context.
"""


# --------------------------------------------------
# To import module(s) and package(s) required
# --------------------------------------------------
import pyspark


# --------------------------------------------------
# To set configurations of Spark Context
# --------------------------------------------------
# ------- To set Spark Configuration -------
# # 1. To set parameters
N_CORES = 6
DRIVER_MEMORY_GB = 20
DRIVER_RESULTS_GB = 17
EXECUTOR_MEMORY_GB = 10

# # 2. To set Spark configuration
pyspark_conf = (
    pyspark.SparkConf()
        .setMaster('local[{0}]'.format(N_CORES))
        .set('spark.driver.memory', '{0}g'.format(DRIVER_MEMORY_GB))
        .set('spark.driver.maxResultSize', '{0}g'.format(DRIVER_RESULTS_GB))
        .set('spark.executor.memory', '{0}g'.format(EXECUTOR_MEMORY_GB))
        .set('spark.sql.execution.arrow.pyspark.enabled', 'true')
)

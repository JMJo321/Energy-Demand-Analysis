# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-2
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Table(s) that illustrate the average treatment
# # effect estimated with hourly data

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(stargazer)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Dataset(s)/Script(s) is(are) loaded
# # 1.1. For Regression Results
DIR_TO.LOAD_CER <- "04_CER"
DIR_TO.LOAD_CER_RESULTS <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Make Table(s) from Regression Results
# ------------------------------------------------------------------------------
# ------- Make Table(s) from Regression Results -------
# # 1. Load Regression Results
load(
  paste(
    DIR_TO.LOAD_CER_RESULTS,
    "CER_Regression-Results_Average-Treatment-Effect_With-Clustered-SEs.RData",
    sep = "/"
  )
)

results_with.clustered.ses <- results_avg.effect_30min_with.clustered.ses[3:5]
rm(results_avg.effect_30min_with.clustered.ses)
gc(reset = TRUE, full = TRUE)

load(
  paste(
    DIR_TO.LOAD_CER_RESULTS,
    "CER_Regression-Results_Average-Treatment-Effect_Without-Clustered-SEs.RData",
    sep = "/"
  )
)
results_without.clustered.ses <-
  results_avg.effect_30min_without.clustered.ses[3:5]
rm(results_avg.effect_30min_without.clustered.ses)
gc(reset = TRUE, full = TRUE)

# # 2. Make objects that will be used to make a table
covariate.labels_whole <- paste0(
  "1[Treatment and Post] x 1[30-Minute Interval = ", seq(1, 48, by = 1),"]"
)
add.lines_whole <- list(
  c("FEs: Day of Week", rep(c("Yes", "No", "No"), times = 2)),
  c("FEs: ID-by-30-Minute-Interval", rep(c("Yes", "Yes", "Yes"), times = 2)),
  c(
    "FEs: Day-of-Week-by-30-Minute-Interval",
    rep(c("No", "Yes", "Yes"), times = 2)
  ),
  c("FEs: Month of Year", rep(c("No", "No", "Yes"), times = 2)),
  c(
    "Tow-way Clustered SEs: At ID and Day:",
    c(rep("No", times = 3), rep("Yes", times = 3))
  )
)
column.labels <- c("Without Clustered SEs", "With Clustered SEs")
column.seperate <- c(3, 3)

# # 3. Print Table(s)
stargazer(
  results_without.clustered.ses, results_with.clustered.ses,
  type = "text",
  column.labels = colnames,
  column.separate = column.seperate,
  covariate.labels = covariate.labels_whole[1:12],
  dep.var.labels = "kWh per 30-Minute Interval",
  add.lines = add.lines_whole,
  keep = 1:12
)
stargazer(
  results_without.clustered.ses, results_with.clustered.ses,
  type = "text",
  column.labels = colnames,
  column.separate = column.seperate,
  covariate.labels = covariate.labels_whole[13:24],
  dep.var.labels = "kWh per 30-Minute Interval",
  add.lines = add.lines_whole,
  keep = 13:24
)
stargazer(
  results_without.clustered.ses, results_with.clustered.ses,
  type = "text",
  column.labels = colnames,
  column.separate = column.seperate,
  covariate.labels = covariate.labels_whole[25:36],
  dep.var.labels = "kWh per 30-Minute Interval",
  add.lines = add.lines_whole,
  keep = 25:36
)
stargazer(
  results_without.clustered.ses, results_with.clustered.ses,
  type = "text",
  column.labels = colnames,
  column.separate = column.seperate,
  covariate.labels = covariate.labels_whole[37:48],
  dep.var.labels = "kWh per 30-Minute Interval",
  add.lines = add.lines_whole,
  keep = 37:48
)

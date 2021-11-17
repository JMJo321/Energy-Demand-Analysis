# < Description > *
# > Date created
# # : 2021-11-16
# #
# > Purpose of the script(s)
# # : To make a table that shows the household daily average consumption by
# #   tariff and period. 

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(huxtable)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"
PROJ.NAME_DETAIL <- "CER-Trial"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, "_", PROJ.NAME_DETAIL, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# ABC
# ------------------------------------------------------------------------------
# ------- XYZ -------
# TODO: 1. Load a DT
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY,
  sep = "/"
)
dt_for.reg <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# TODO: 2. Compute Electricity Consumption by Tariff and Period
dt_consumption <-
  dt_for.reg[  # To compute day-level consumption
    is_in.sample_incl.control == TRUE,
    lapply(.SD, sum, rm.na = TRUE), .SDcols = "kwh",
    by = .(id, alloc_r_tariff_desc, group, period, day)
  ] %>%
    .[  # To compute the mean consumption of households by tariff and period
      ,
      lapply(.SD, mean, rm.na = TRUE), .SDcols = "kwh",
      by = .(alloc_r_tariff_desc, period)
    ] %>%
    dcast(., alloc_r_tariff_desc ~ period, value.var = "kwh")

dt_consumption[, diff := Treatment - Baseline]

# TODO: 3. Make a Pretty Table
ht <- hux(dt_consumption)
bold(ht)[1,] <- TRUE
top_border(ht)[1,] <- brdr(thickness = 1)
bottom_border(ht)[c(1, 6),] <- brdr(thickness = 1)
right_border(ht)[, 1] <- brdr(thickness = 1)
align(ht)[, 1:4] <- "center"
contents(ht)[1, 1] <- "Tariff Group"
contents(ht)[1, 4] <- "Change"
ht

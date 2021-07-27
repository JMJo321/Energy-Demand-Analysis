# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Estimate the Treatment Impact on
# # Household Response to Temperature at hourly consumption

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
library(lfe)
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
# # 1.1. For Metering Data
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.RData"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY,
  sep = "/"
)

# # 1.2. For R Script including Regression Models
FILE_TO.LOAD_CER_MODELS <- "M-Energy-Demand-Analysis_Regression-Models_CER.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_CODE,
  FILE_TO.LOAD_CER_MODELS,
  sep = "/"
)


# # 2. Path(s) to which Regression Results will be stored
DIR_TO.SAVE_CER <- paste(PATH_DATA_ANALYSIS, "04_CER", sep = "/")
FILE_TO.SAVE_CER_RESULTS <- "CER_Regression-Results_Rate-Period.RData"
PATH_TO.SAVE_CER_RESULTS <- paste(
  DIR_TO.SAVE_CER, FILE_TO.SAVE_CER_RESULTS, sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
load(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Load Script(s) -------
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Create DT(s) for Regressions
# ------------------------------------------------------------------------------
# ------- Create DT(s) for Regressions -------
# # 1. Create a DT that includes Household-level Hourly Average Consumption
dt_for.reg_hourly <- dt_for.reg[
  is_in.sample_incl.control == TRUE,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(
    date, id_in.factor, season, rate.period, length_rate.period,
    is_treated_r, is_treatment.period, treatment.and.post,
    hd_by.season.and.rate.period,
    day.of.week_in.factor, id.and.day.of.week_in.factor, month_in.factor
  )
]
dt_for.reg_hourly[, kwh := kwh / length_rate.period]


# ------------------------------------------------------------------------------
# Run Regressions
# ------------------------------------------------------------------------------
# ## Note:
# ## It takes about 2 minutes to get a regression result.

# ------- Run Regressions with OLS Models -------
# # 1. Run Regressions with Hourly Consumption Data: By using kWh
result_ols_hourly_real.kwh_cold <- felm(
  data = dt_for.reg[is_in.sample_incl.control == TRUE & season == "Cold"],
  formula = model_ols_hourly
)
result_ols_hourly_real.kwh_warm <- felm(
  data = dt_for.reg[is_in.sample_incl.control == TRUE & season == "Warm"],
  formula = model_ols_hourly
)


# # 2. Run Regressions with Hourly Consumption Data: By using Average kWh
result_ols_hourly_avg.kwh_cold <- felm(
  data = dt_for.reg_hourly[season == "Cold"],
  formula = model_ols_hourly
)
result_ols_hourly_avg.kwh_warm <- felm(
  data = dt_for.reg_hourly[season == "Warm"],
  formula = model_ols_hourly
)


# ------- Run Regressions with FEs Models -------
# # 1. Run Regressions with Hourly Consumption Data: By using kWh
result_fes_hourly_real.kwh_cold <- felm(
  data = dt_for.reg[is_in.sample_incl.control == TRUE & season == "Cold"],
  formula = model_fes_hourly_var1
)
result_fes_hourly_real.kwh_warm <- felm(
  data = dt_for.reg[is_in.sample_incl.control == TRUE & season == "Warm"],
  formula = model_fes_hourly_var1
)


# # 2. Run Regressions with Hourly Consumption Data: By using Average kWh
result_fes_hourly_avg.kwh_cold <- felm(
  data = dt_for.reg_hourly[season == "Cold"],
  formula = model_fes_hourly_var1
)
result_fes_hourly_avg.kwh_warm <- felm(
  data = dt_for.reg_hourly[season == "Warm"],
  formula = model_fes_hourly_var1
)


# ------------------------------------------------------------------------------
# Save Regression Results in .RData Format
# ------------------------------------------------------------------------------
# ------- Save Regression Results -------
# # 1. Make an Object for later use
rate.periods <- dt_for.reg[, .N, by = .(rate.period)]$rate.period

# # 2. Save Regression Results
results <- ls()[str_detect(ls(), "^result_")]
save(list = c(results, "rate.periods"), file = PATH_TO.SAVE_CER_RESULTS)

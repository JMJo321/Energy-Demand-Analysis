# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Estimate the Average Treatment Effect with
# # Household 30-Minute Electricity Consumption

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
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
  "CER_DT-for-Regressions_Electricity.parquet"
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


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To get a subsetting condition
get_condition.in.str <- function (season_rate.period) {
  condition_in.str <- paste(
    "is_in.sample_incl.control == TRUE",
    paste0(
      "season == '", season_rate.period[1] %>% str_to_title(.), "'"
    ),
    paste0(
      "rate.period == '", season_rate.period[2] %>% str_to_title(.), "'"
    ),
    sep = " & "
  )
  return (condition_in.str)
}

# # 2. To run regressions based on pairs of seasons and rate periods
get_reg.result <- function(season_rate.period, formula) {
  reg.result <- felm(
    data = dt_for.reg[
      eval(parse(text = get_condition.in.str(season_rate.period)))
    ],
    formula = formula
  )
  return (reg.result)
}


# ------------------------------------------------------------------------------
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
dt_for.reg <-
  arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY) %>%
    setDT(.)


# ------- Load Script(s) -------
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Run Regressions and Save Results in .RData Format
# ------------------------------------------------------------------------------
# ------- Create Object(s) that will be used later -------
# # 1. Create list(s) that include models for estimating the average treatment
# #    effect
# # 1.1. For models with clustered SEs
list_avg.effect_30min_with.clustered.ses <- list(
  model_avg.effect_30min_i_clustered.ses =
    model_avg.effect_30min_i_clustered.ses,
  model_avg.effect_30min_i.d_clustered.ses =
    model_avg.effect_30min_i.d_clustered.ses,
  model_avg.effect_30min_iw.d_clustered.ses =
    model_avg.effect_30min_iw.d_clustered.ses,
  model_avg.effect_30min_iw.dw_clustered.ses =
    model_avg.effect_30min_iw.dw_clustered.ses,
  model_avg.effect_30min_iw.dw.m_clustered.ses =
    model_avg.effect_30min_iw.dw.m_clustered.ses
)

# # 1.2. For models without clustered SEs
list_avg.effect_30min_without.clustered.ses <- list(
  model_avg.effect_30min_i = model_avg.effect_30min_i,
  model_avg.effect_30min_i.d = model_avg.effect_30min_i.d,
  model_avg.effect_30min_iw.d = model_avg.effect_30min_iw.d,
  model_avg.effect_30min_iw.dw = model_avg.effect_30min_iw.dw,
  model_avg.effect_30min_iw.dw.m = model_avg.effect_30min_iw.dw.m
)


# # 2. Create list(s) that include pairs of seasons and rate periods
seasons <- c("Warm","Cold")
rate.periods <- c("Night","Day", "Peak")
dt_cases <- data.table(
  season = rep(seasons, each = 3),
  rate.period = rep(rate.periods, times = 2)
)
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  tmp_season <- dt_cases[row]$season %>% tolower(.)
  tmp_rate.period <- dt_cases[row]$rate.period %>% tolower(.)
  tmp_name <- paste(tmp_season, tmp_rate.period, sep = "_")
  tmp_list <- list(c(tmp_season, tmp_rate.period))
  names(tmp_list) <- tmp_name
  list_cases <- c(list_cases, tmp_list)
}


# ------- Run Regressions: With Full Sample -------
# # 1. For Models with Clustered SEs
results_avg.effect_30min_with.clustered.ses <- lapply(
  list_avg.effect_30min_with.clustered.ses,
  felm,
  data = dt_for.reg[is_in.sample_incl.control == TRUE]
)
save(
  results_avg.effect_30min_with.clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_With-Clustered-SEs.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 90 (= 50 + 40) minutes. 

# # 2. For Models without Clustered SEs
results_avg.effect_30min_without.clustered.ses <- lapply(
  list_avg.effect_30min_without.clustered.ses,
  felm,
  data = dt_for.reg[is_in.sample_incl.control == TRUE]
)
save(
  results_avg.effect_30min_without.clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_Without-Clustered-SEs.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 90 (= 50 + 40) minutes. 


# ------- Run Regressions: With Sample constructed based on Rate Period -------
# # 1. For Night
results_avg.effect_30min_night <- lapply(
  list_avg.effect_30min_with.clustered.ses,
  felm,
  data = dt_for.reg[is_in.sample_incl.control == TRUE & rate.period == "Night"]
)
save(
  results_avg.effect_30min_night,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_By-Rate-Period_Night.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 40 (= 20 + 20) minutes. 

# # 2. For Day
results_avg.effect_30min_day <- lapply(
  list_avg.effect_30min_with.clustered.ses,
  felm,
  data = dt_for.reg[is_in.sample_incl.control == TRUE & rate.period == "Day"]
)
save(
  results_avg.effect_30min_day,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_By-Rate-Period_Day.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 50 (= 25 + 25) minutes. 

# # 3. For Peak
results_avg.effect_30min_peak <- lapply(
  list_avg.effect_30min_with.clustered.ses,
  felm,
  data = dt_for.reg[is_in.sample_incl.control == TRUE & rate.period == "Peak"]
)
save(
  results_avg.effect_30min_peak,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_By-Rate-Period_Peak.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 8 (= 4 + 4) minutes. 


# ------- Run Regressions: With Sample constructed based on Season and Rate Period -------
results_avg.effect_30min_by.season.and.rate.period <- lapply(
  list_cases,
  get_reg.result, formula = model_avg.effect_30min_iw.d_clustered.ses
)
save(
  results_avg.effect_30min_by.season.and.rate.period,
  file = paste(
    DIR_TO.SAVE_CER,
    "CER_Regression-Results_Average-Treatment-Effect_By-Season-and-Rate-Period.RData",
    sep = "/"
  )
)
# ## Note:
# ## This work takes about 20 (= 10 + 10) minutes. 
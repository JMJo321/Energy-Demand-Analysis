# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04H-1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Run regressions, by using sub-samples constructed
# # based on season, which include rate-period-level consumption, to estimate
# # the treatment effect on Rate-Period-Level household response to temperature

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
get_condition.in.str <- function (season) {
  if (season %in% c("warm", "cold")) {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0(
        "season == '", (season %>% str_to_title(.)), "'"
      ),
      sep = " & "
    )
  } else {
    condition_in.str <- "is_in.sample_incl.control == TRUE"
  }
  return (condition_in.str)
}

# # 2. To run regressions by using `lapply` function
get_reg.result <- function(season.and.rate.period, formula) {
  reg.result <- felm(
    data = dt_for.reg_rate.period[
      eval(parse(text = get_condition.in.str(season.and.rate.period)))
    ],
    formula = formula
  )
  return (reg.result)
}

# # 3. To change terms in a formula
# ## Note:
# ## Formulas are defined by using `hdd`. But exact variable names are
# ## `hdd_all`, `hdd_extremes`, and `hdd_soil`.
change_terms.in.formula <-
  function (
    formula, is_terms.in.dep.var = FALSE, term_old_in.str, term_new_in.str
  ) {
    formula_in.str <- as.character(formula)
    if (is_terms.in.dep.var == TRUE) {
      formula_in.str[2] <-
        str_replace(formula_in.str[2], term_old_in.str, term_new_in.str)
    } else {
      formula_in.str[3] <-
        str_replace_all(formula_in.str[3], term_old_in.str, term_new_in.str)
    }
    formula_modified <-
      paste(formula_in.str[2], formula_in.str[3], sep = " ~ ") %>%
        as.formula(.)
    return (formula_modified)
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
# Run regressions and save the results in .RData format
# ------------------------------------------------------------------------------
# -------  -------
rate.periods_detail1_modified <-
  c("night", "day_pre.peak", "peak", "day_post.peak")
cols_by <- c(
  "id", "day", "date", "rate.period_detail_level1",
  "length_rate.period_detail_level1", "season", "is_in.sample_incl.control",
  "hdd_all",
  paste0("is_treatment_rate.period_", rate.periods_detail1_modified),
  paste0("is_post_rate.period_", rate.periods_detail1_modified),
  paste0("is_treatment.and.post_rate.period_", rate.periods_detail1_modified),
  "id_in.factor", "id.and.day.of.week_in.factor",
  "id.and.rate.period.level1_in.factor",
  "id.and.day.of.week.and.rate.period.level1_in.factor",
  "day_in.factor", "day.of.week_in.factor",
  "day.of.week.and.rate.period.level1_in.factor",
  "month_in.factor", "month.and.rate.period.level1_in.factor"
)
dt_for.reg_rate.period <- dt_for.reg[
  ,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by
]
dt_for.reg_rate.period[, kwh_per.hour := kwh / length_rate.period_detail_level1]
rm(dt_for.reg)
gc(reset = TRUE, full = TRUE)


# ------- Create Object(s) that will be used later -------
# # 1. Create a list of cases
seasons <- dt_for.reg_rate.period[!is.na(season), .N, by = .(season)]$season
dt_cases <- data.table(season = c("Both", seasons))
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  tmp_season <- dt_cases[row]$season %>% tolower(.)
  tmp_name <- tmp_season
  tmp_list <- list(tmp_season)
  names(tmp_list) <- tmp_name
  list_cases <- c(list_cases, tmp_list)
}


# # 2. Create a list that includes econometric models
# # 2.1. Create a list
models_temp.response.by.period_rate.period_with.clustered.ses <- list(
  model_temp.response.by.period_rate.period_i_clustered.ses =
    model_temp.response.by.period_rate.period_i_clustered.ses,
  model_temp.response.by.period_rate.period_i.d_clustered.ses =
    model_temp.response.by.period_rate.period_i.d_clustered.ses,
  model_temp.response.by.period_rate.period_id.d_clustered.ses =
    model_temp.response.by.period_rate.period_id.d_clustered.ses,
  model_temp.response.by.period_rate.period_ip.d_clustered.ses =
    model_temp.response.by.period_rate.period_ip.d_clustered.ses,
  model_temp.response.by.period_rate.period_idp.d_clustered.ses =
    model_temp.response.by.period_rate.period_idp.d_clustered.ses,
  model_temp.response.by.period_rate.period_idp.dp_clustered.ses =
    model_temp.response.by.period_rate.period_idp.dp_clustered.ses,
  model_temp.response.by.period_rate.period_idp.dp.m_clustered.ses =
    model_temp.response.by.period_rate.period_idp.dp.m_clustered.ses,
  model_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses =
    model_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses
)

# # 2.2. Change terms in formulas
modified.models_temp.response.by.period_rate.period_with.clustered.ses <-
  lapply(
    models_temp.response.by.period_rate.period_with.clustered.ses,
    change_terms.in.formula,
    is_terms.in.dep.var = FALSE,
    term_old_in.str = "hdd", term_new_in.str = "hdd_all"
  )


# ------- Run regressions -------
# ## Note:
# ## Run regression, save as .RData format, remove regression results, and
# ## make memory clean. Each one takes about 2 minutes
# ## (1 minutes + 1 minutes).

# # 1. For the first model
results_temp.response.by.period_rate.period_i_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[1]]
  )
save(
  results_temp.response.by.period_rate.period_i_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_i.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_i_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 2. For the second model
results_temp.response.by.period_rate.period_i.d_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[2]]
)
save(
  results_temp.response.by.period_rate.period_i.d_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_i-d.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_i.d_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 3. For the third model
results_temp.response.by.period_rate.period_id.d_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[3]]
)
save(
  results_temp.response.by.period_rate.period_id.d_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_id-d.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_id.d_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 4. For the fourth model
results_temp.response.by.period_rate.period_ip.d_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[4]]
)
save(
  results_temp.response.by.period_rate.period_ip.d_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_ip-d.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_ip.d_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 5. For the fifth model
results_temp.response.by.period_rate.period_idp.d_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[5]]
)
save(
  results_temp.response.by.period_rate.period_idp.d_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_idp-d.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_idp.d_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 6. For the sixth model
results_temp.response.by.period_rate.period_idp.dp_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[6]]
)
save(
  results_temp.response.by.period_rate.period_idp.dp_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_idp-dp.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_idp.dp_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 7. For the seventh model
results_temp.response.by.period_rate.period_idp.dp.m_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[7]]
)
save(
  results_temp.response.by.period_rate.period_idp.dp.m_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_idp-dp-m.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_idp.dp.m_clustered.ses)
gc(reset = TRUE, full = TRUE)


# # 8. For the eighth model
results_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses <- lapply(
  list_cases,
  get_reg.result,
  formula =
    modified.models_temp.response.by.period_rate.period_with.clustered.ses[[8]]
)
save(
  results_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses,
  file = paste(
    DIR_TO.SAVE_CER,
    paste0(
      "CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      "Rate-Period-Consumption_",
      "By-Season_idp-dp-mp.RData"
    ),
    sep = "/"
  )
)
rm(results_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses)
gc(reset = TRUE, full = TRUE)

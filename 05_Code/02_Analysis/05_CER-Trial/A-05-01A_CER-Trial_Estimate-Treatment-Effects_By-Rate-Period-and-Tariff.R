# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-01A
# #
# > Purpose of the script(s):
# # Run regressions to estimate the treatment effect by rate period and tariff,
# # Use HDDs that are computed based on 60 and 65 degrees Fahrenheit,
# # respectively.

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
# # 1.1. For the DT for regression analysis
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
get_condition.in.str <- function (case) {
  if (case[1] %in% c("warm", "cold")) {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0("season == '", (case[1] %>% str_to_title(.)), "'"),
      paste0(
        "as.character(rate.period_detail_level1) == '",
        list_rate.periods[[case[2]]],
        "'"
      ),
      paste0("alloc_r_tariff %in% c('", list_tariffs[[case[3]]], "', 'E')"),
      sep = " & "
    )
  } else {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0(
        "as.character(rate.period_detail_level1) == '",
        list_rate.periods[[case[2]]],
        "'"
      ),
      paste0("alloc_r_tariff %in% c('", list_tariffs[[case[3]]], "', 'E')"),
      sep = " & "
    )
  }
  return (condition_in.str)
}

# # 2. To run regressions by using `lapply` function
get_reg.result <- function(season.and.rate.period, formula) {
  reg.result <- felm(
    data = dt_for.reg[
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
dt_for.reg <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Load Script(s) -------
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Run regressions
# ------------------------------------------------------------------------------
# ------- Modify the DT that will be used to run regressions -------
# # 1. Add columns
cols_indicator <-
  c("is_treated_r", "is_treatment.period", "is_treatment.and.post")
cols_hdd <- c("hdd_all", "hdd_all_based.on.60f")
for (col in cols_indicator) {
  for (col_ in cols_hdd) {
    tmp_col.name <- paste(col_, col, sep = "_by_")
    dt_for.reg[, (tmp_col.name) := as.numeric(get(col)) * get(col_)]
  }
}


# ------- Create objects that will be used when running regressions -------
# ## Note:
# ## Vectors `rate.periods_detail1_modified`, `tariffs`, and `tariffs_modified`
# ## are created in the script "M-Energy-Demand-Analysis_Regression-Models_CER".

# # 1. Create a list that includes the subsetting conditions for each
# #    regression
# # 1.1. Create objects that will be used later
# # 1.1.1. Object(s) regarding rate periods
rate.periods_detail1 <-
  dt_for.reg[
    , .N, by = .(rate.period_detail_level1)
  ]$rate.period_detail_level1 %>% as.character(.)
list_rate.periods <- as.list(rate.periods_detail1)
names(list_rate.periods) <- rate.periods_detail1_modified
# # 1.1.2. Object(s) regarding tariffs
list_tariffs <- as.list(tariffs)
names(list_tariffs) <- tariffs_modified
# # 1.1.3. Object(s) regarding seasons
seasons <- "Both"
# ## Note:
# ## Do not separately estimate the treatment effect for "Warm" and "Cold"
# ## seasons.

# # 1.2. Create a list that includes conditions for subsetting the sample
# # 1.2.1. Create a DT that will be used to create a list
dt_cases <-
  expand.grid(seasons, rate.periods_detail1_modified, tariffs_modified) %>%
    setDT(.)
names(dt_cases) <- c("season", "rate.period", "tariff")
# # 1.2.2. Create a list by using the DT created above
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  tmp_season <- dt_cases[row]$season %>% tolower(.)
  tmp_rate.period <- dt_cases[row]$rate.period %>% as.character(.)
  tmp_tariff <- dt_cases[row]$tariff %>% as.character(.)

  tmp_name <- paste(tmp_season, tmp_rate.period, tmp_tariff, sep = "_")
  tmp_list <- list(c(tmp_season, tmp_rate.period, tmp_tariff))
  names(tmp_list) <- tmp_name
  list_cases <- c(list_cases, tmp_list)
}


# # 2. Create a list that includes regression models
# # 2.1. Create a list that includes econometric models defined in the script
# #      "M-Energy-Demand-Analysis_Regression-Models_CER"
models_treatment.effect_by.period.and.tariff_30min <- list(
  model_treatment.effect_by.period.and.tariff_30min_iw =
    model_treatment.effect_by.period.and.tariff_30min_iw_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.dp =
    model_treatment.effect_by.period.and.tariff_30min_iw.dp_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.dw =
    model_treatment.effect_by.period.and.tariff_30min_iw.dw_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.mp =
    model_treatment.effect_by.period.and.tariff_30min_iw.mp_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.mw =
    model_treatment.effect_by.period.and.tariff_30min_iw.mw_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.dp.mp =
    model_treatment.effect_by.period.and.tariff_30min_iw.dp.mp_clustered.ses,
  model_treatment.effect_by.period.and.tariff_30min_iw.dw.mw =
    model_treatment.effect_by.period.and.tariff_30min_iw.dw.mw_clustered.ses
)

# # 2.2. Modified econometric models in the list created above
# # 2.2.1. Models for HDDs whose reference temperature is 65 degrees Fahrenheit
modified.models_based.on.65f <-
  lapply(
    models_treatment.effect_by.period.and.tariff_30min,
    change_terms.in.formula,
    is_terms.in.dep.var = FALSE,
    term_old_in.str = "hdd", term_new_in.str = "hdd_all"
  )
# # 2.2.2. Models for HDDs whose reference temperature is 60 degrees Fahrenheit
modified.models_based.on.60f <-
  lapply(
    models_treatment.effect_by.period.and.tariff_30min,
    change_terms.in.formula,
    is_terms.in.dep.var = FALSE,
    term_old_in.str = "hdd", term_new_in.str = "hdd_all_based.on.60f"
  )


# ------- Run regressions -------
# ## Note:
# ## Run regression, save as .RData format, remove regression results, and
# ## make memory clean. Each model takes about 20 minutes
# ## (10 minutes + 10 minutes).

# # 1. Run regressions by using HDDs whose reference temperature is 65 degrees
# #    Fahrenheit
for (idx in 1:length(modified.models_based.on.65f)) {
  # ## Create temperature objects
  tmp_obj.name <-
    names(models_treatment.effect_by.period.and.tariff_30min)[idx] %>%
      str_replace(., "^model", "results") %>%
      str_replace(., "30min", "30min_based.on.65f")
  tmp_model <-
    names(models_treatment.effect_by.period.and.tariff_30min)[idx] %>%
      str_replace(., "(?>^.+_)", "") %>%
      str_replace_all(., "\\.", "-")

  # ## Run regressions
  print("Running Regressions ...")
  assign(
    tmp_obj.name,
    lapply(
      list_cases,
      get_reg.result,
      formula = modified.models_based.on.65f[[idx]]
    )
  )

  # ## Save regression results in .RData format
  print("Saving Regression Results ...")
  save(
    list = tmp_obj.name,
    file = paste(
      DIR_TO.SAVE_CER,
      paste(
        "CER_Regression-Results_Rate-Period-Level-Treatment-Effect",
        "Based-on-65F",
        "By-Season-and-Tariff",
        paste0(tmp_model, ".RData"),
        sep = "_"
      ),
      sep = "/"
    )
  )

  # ## Remove regression results from memory
  rm(list = tmp_obj.name)
  gc(reset = TRUE, full = TRUE)

  # ## Print a message that shows the current work progress
  print(
    paste0(
      "Estimation is completed : Model ", idx, " out of ",
      length(models_treatment.effect_by.period.and.tariff_30min)
    )
  )
}


# # 2. Run regressions by using HDDs whose reference temperature is 60 degrees
# #    Fahrenheit
for (idx in 1:length(modified.models_based.on.60f)) {
  # ## Create temperature objects
  tmp_obj.name <-
    names(models_treatment.effect_by.period.and.tariff_30min)[idx] %>%
      str_replace(., "^model", "results") %>%
      str_replace(., "30min", "30min_based.on.60f")
  tmp_model <-
    names(models_treatment.effect_by.period.and.tariff_30min)[idx] %>%
      str_replace(., "(?>^.+_)", "") %>%
      str_replace_all(., "\\.", "-")

  # ## Run regressions
  print("Running Regressions ...")
  assign(
    tmp_obj.name,
    lapply(
      list_cases,
      get_reg.result,
      formula = modified.models_based.on.60f[[idx]]
    )
  )

  # ## Save regression results in .RData format
  print("Saving Regression Results ...")
  save(
    list = tmp_obj.name,
    file = paste(
      DIR_TO.SAVE_CER,
      paste(
        "CER_Regression-Results_Rate-Period-Level-Treatment-Effect",
        "Based-on-60F",
        "By-Season-and-Tariff",
        paste0(tmp_model, ".RData"),
        sep = "_"
      ),
      sep = "/"
    )
  )

  # ## Remove regression results from memory
  rm(list = tmp_obj.name)
  gc(reset = TRUE, full = TRUE)

  # ## Print a message that shows the current work progress
  print(
    paste0(
      "Estimation is completed : Model ", idx, " out of ",
      length(models_treatment.effect_by.period.and.tariff_30min)
    )
  )
}

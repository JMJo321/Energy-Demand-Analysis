# < Description > *
# > Date created
# # : 2021-11-20
# #
# > Purpose of the script(s)
# # : To make figure(s) that show the amount of reduction in electricity
# #   consumption by tariff structure.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(lfe)
library(ggplot2)
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

# TODO: 1. Define regression models
rate.periods_detail1_modified <-
  c("night", "day_pre.peak", "peak", "day_post.peak")
tariffs <- c("A", "B", "C", "D")
tariffs_modified <- tolower(tariffs)

# TODO: 1.1. ATE
dep.var_treatment.effect_by.period.and.tariff <- "kwh"
indep.var_covariates_treatment.effect_by.period.and.tariff <- paste(
  "is_treated_r",
  "hdd_by_is_treated_r",
  "is_treatment.period",
  "hdd_by_is_treatment.period",
  "is_treatment.and.post",
  "hdd_by_is_treatment.and.post",
  sep = " + "
)
indep.var_ivs_treatment.effect_by.period.and.tariff <- "0"
indep.var_clustered.ses_treatment.effect_by.period.and.tariff <-
  "id_in.factor + day_in.factor"

# TODO: 1.1.1. Without "HDD" term
model_ate_old <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      # "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )

# TODO: 1.1.2. With "HDD" term
model_ate_new <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates = paste(
      "hdd",
      indep.var_covariates_treatment.effect_by.period.and.tariff,
      sep = " + "
    ),
    indep.var_fes = paste(
      # "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )

# TODO: 1.2. Rate changes
dep.var_treatment.effect_w.rate.change <- "kwh"
indep.var_covariates_treatment.effect_w.rate.change.by.rate.period <-
  paste(
    "treatment.times.rate.change_by.rate.period",
    "hdd.times.treatment.times.rate.change_by.rate.period",
    "is_treatment.period",
    "hdd.times.post",
    "treatment.and.post.times.rate.change_by.rate.period",
    "hdd.times.treatment.and.post.times.rate.change_by.rate.period",
    sep = " + "
  )
indep.var_ivs_treatment.effect_w.rate.change <- "0"
indep.var_clustered.ses_treatment.effect_w.rate.change <-
  "id_in.factor + day_in.factor"

# TODO: 1.2.1. Without "HDD" term
model_rate.change_old <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_w.rate.change,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_w.rate.change.by.rate.period,
    indep.var_fes = paste(
      # "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_w.rate.change,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_w.rate.change
  )

# TODO: 1.2.2. With "HDD" term
model_rate.change_new <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_w.rate.change,
    indep.var_covariates = paste(
      "is_treated_r",
      "is_treatment.and.post",
      "hdd_all_60f",
      "hdd_all_60f_by_is_treated_r",
      "hdd_all_60f_by_is_treatment.and.post",
      indep.var_covariates_treatment.effect_w.rate.change.by.rate.period,
      sep = " + "
    ),
    indep.var_fes = paste(
      # "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_w.rate.change,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_w.rate.change
  )

# TODO: 1.3. Make lists that include regression models
models <- list(
  model_ate_old = model_ate_old,
  model_ate_new = model_ate_new
)


# TODO: 2. Run regressions
# TODO: 2.1. Create a DT for regression analysis
# TODO: 2.1.1. Load a DT
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

# TODO: 2.1.2. Modity the DT loaded
cols_indicator <-
  c("is_treated_r", "is_treatment.period", "is_treatment.and.post")
cols_hdd <- "hdd_all_60f"
for (col in cols_indicator) {
  for (col_ in cols_hdd) {
    tmp_col.name <- paste(col_, col, sep = "_by_")
    dt_for.reg[, (tmp_col.name) := as.numeric(get(col)) * get(col_)]
  }
}

basis_rate <- dt_for.reg[
  group == "Control", .N, by = .(rate_cents.per.kwh)
]$rate_cents.per.kwh
dt_for.reg[
  ,
  rate.change_by.period.and.rate.period := rate_cents.per.kwh - basis_rate
]
dt_rate.change <- dt_for.reg[
  period == "Treatment",
  .N,
  by = .(alloc_r_tariff, rate.period, rate.change_by.period.and.rate.period)
][
  , N := NULL
]
setnames(
  dt_rate.change,
  old = "rate.change_by.period.and.rate.period",
  new = "rate.change_by.rate.period"
)
dt_for.reg <- merge(
  x = dt_for.reg,
  y = dt_rate.change,
  by = c("alloc_r_tariff", "rate.period"),
  all.x = TRUE
)
dt_for.reg[, hdd.times.post := hdd_all_60f * is_treatment.period]
dt_for.reg[
  ,
  treatment.times.rate.change_by.rate.period :=
    is_treated_r * rate.change_by.rate.period
]
dt_for.reg[
  ,
  hdd.times.treatment.times.rate.change_by.rate.period :=
    hdd_all_60f * treatment.times.rate.change_by.rate.period
]
dt_for.reg[
  ,
  treatment.and.post.times.rate.change_by.rate.period :=
    is_treatment.and.post * rate.change_by.rate.period
]
dt_for.reg[
  ,
  hdd.times.treatment.and.post.times.rate.change_by.rate.period :=
    hdd_all_60f * treatment.and.post.times.rate.change_by.rate.period
]


# TODO 2.1.3. Check subscriptions in regression models
dt_for.reg[
  is_in.sample_incl.control == TRUE,
  .N,
  by = .(id, day, rate.period, interval_30min, kwh)
][, .N, by = .(id, day, rate.period, interval_30min)][N > 1]
dt_for.reg[
  is_in.sample_incl.control == TRUE, .N, by = .(id, is_treated_r)
][, .N, by = .(id)][N > 1]
dt_for.reg[
  is_in.sample_incl.control == TRUE, .N, by = .(id, day, is_treatment.period)
][, .N, by = .(id, day)][N > 1]
dt_for.reg[
  is_in.sample_incl.control == TRUE, .N, by = .(day, hdd_all_60f)
][, .N, by = .(day)][N > 1]
dt_for.reg[
  is_in.sample_incl.control == TRUE,
  .N,
  by = .(day, rate.period, alloc_r_tariff, rate.change_by.rate.period)
][, .N, by = .(day, rate.period, alloc_r_tariff)][N > 1]

# TODO: 2.2. Run regressions by using felm
# TODO: 2.2.1. Create a DT that include conditions
rate.periods <- "peak"
list_rate.periods <- list(peak = "Peak (17-18)")
list_tariffs <- as.list(tariffs)
names(list_tariffs) <- tariffs_modified
seasons <- "Both"
dt_cases <-
  expand.grid(seasons, rate.periods, tariffs_modified) %>%
    setDT(.)
names(dt_cases) <- c("season", "rate.period", "tariff")

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

# TODO: 2.2.2. Modify variable names in regression models
models_modified <- lapply(
  models,
  change_terms.in.formula,
  is_terms.in.dep.var = FALSE,
  term_old_in.str = "hdd", term_new_in.str = "hdd_all_60f"
)

# TODO: 2.2.3. Run regressions
# ## Note:
# ## It takes about 2 hours.
gc(reset = TRUE)

# TODO: 2.2.3.1. ATE
get_condition.in.str <- function (case) {
  if (case[1] %in% c("warm", "cold")) {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0("season == '", (case[1] %>% stringr::str_to_title(.)), "'"),
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

timestamp()
for (idx in 1:4) {
  tmp_rate.code <- list_cases[[idx]][3]
  tmp_obj.name <- paste("reg.results_ate_wo.hdd.term", tmp_rate.code, sep = "_")
  tmp_file.name <- paste(
    PATH_DATA_ANALYSIS, "04_CER/From-Scratches/2021-11-20",
    paste0(
      "Regression-Results_ATE_Without-HDD-Term_Rate-Period-",
      toupper(tmp_rate.code),
      ".RData"
    ),
    sep = "/"
  )
  assign(
    tmp_obj.name,
    get_reg.result(list_cases[[idx]], models_modified[[1]])
  )
  save(list = tmp_obj.name, file = tmp_file.name)
  rm(list = tmp_obj.name)
  gc(reset = TRUE)
}

timestamp()
for (idx in 1:4) {
  tmp_rate.code <- list_cases[[idx]][3]
  tmp_obj.name <- paste("reg.results_ate_w.hdd.term", tmp_rate.code, sep = "_")
  tmp_file.name <- paste(
    PATH_DATA_ANALYSIS, "04_CER/From-Scratches/2021-11-20",
    paste0(
      "Regression-Results_ATE_With-HDD-Term_Rate-Period-",
      toupper(tmp_rate.code),
      ".RData"
    ),
    sep = "/"
  )
  assign(
    tmp_obj.name,
    get_reg.result(list_cases[[idx]], models_modified[[2]])
  )
  save(list = tmp_obj.name, file = tmp_file.name)
  rm(list = tmp_obj.name)
  gc(reset = TRUE)
}

# TODO: 2.2.3.2. Rate Change
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
      sep = " & "
    )
  }
  return (condition_in.str)
}

timestamp()
for (idx in 1) { # Rate code is not required.
  tmp_obj.name <- "reg.results_rate.change_wo.hdd.term"
  tmp_file.name <- paste(
    PATH_DATA_ANALYSIS, "04_CER/From-Scratches/2021-11-20",
    paste0(
      "Regression-Results_Function-of-Rate-Change_Without-HDD-Term_Rate-Period",
      ".RData"
    ),
    sep = "/"
  )
  assign(
    tmp_obj.name,
    get_reg.result(list_cases[[idx]][1:2], model_rate.change_old)
  )
  save(list = tmp_obj.name, file = tmp_file.name)
  rm(list = tmp_obj.name)
  gc(reset = TRUE)
}

timestamp()
for (idx in 1) {
  tmp_obj.name <- "reg.results_rate.change_w.hdd.term"
  tmp_file.name <- paste(
    PATH_DATA_ANALYSIS, "04_CER/From-Scratches/2021-11-20",
    paste0(
      "Regression-Results_Function-of-Rate-Change_With-HDD-Term_Rate-Period-",
      ".RData"
    ),
    sep = "/"
  )
  assign(
    tmp_obj.name,
    get_reg.result(list_cases[[idx]][1:2], model_rate.change_new)
  )
  save(list = tmp_obj.name, file = tmp_file.name)
  rm(list = tmp_obj.name)
  gc(reset = TRUE)
}

timestamp()


# TODO: 3. Extract the estimates from regression results
# TODO: 3.1. Load files
path_to.load <- paste(
  PATH_DATA_ANALYSIS, "04_CER/From-Scratches/2021-11-20", sep = "/"
)
files <- list.files(path_to.load)
for (file in files) {
  tmp_path <- paste(path_to.load, file, sep = "/")
  load(tmp_path)
}

# TODO: 3.2. Make lists that include regression results
reg.results_ate_wo.hdd.term <-
  mget(ls()[str_detect(ls(), "^reg.results_ate_wo.hdd")])
reg.results_ate_w.hdd.term <-
  mget(ls()[str_detect(ls(), "^reg.results_ate_w.hdd")])

# TODO: 3.3. Make a DT that includes estimates
dt_estimates_ate_wo.hdd.term <- setDT(NULL)
for (idx in 1:length(reg.results_ate_wo.hdd.term)) {
  tmp_rate.code <-
    names(reg.results_ate_wo.hdd.term)[idx] %>% str_extract(., ".$")
  tmp_dt <- get_estimates.from.felm(
    reg.results_ate_wo.hdd.term[[idx]],
    level = 0.95, fe = FALSE, se.type = "cluster"
  )
  tmp_dt[, model := "ATE"]
  tmp_dt[, rate.code := tmp_rate.code]
  tmp_dt[, is_include.hdd.term := FALSE]
  dt_estimates_ate_wo.hdd.term <- rbind(dt_estimates_ate_wo.hdd.term, tmp_dt)
}

dt_estimates_ate_w.hdd.term <- setDT(NULL)
for (idx in 1:length(reg.results_ate_w.hdd.term)) {
  tmp_rate.code <-
    names(reg.results_ate_w.hdd.term)[idx] %>% str_extract(., ".$")
  tmp_dt <- get_estimates.from.felm(
    reg.results_ate_w.hdd.term[[idx]],
    level = 0.95, fe = FALSE, se.type = "cluster"
  )
  tmp_dt[, model := "ATE"]
  tmp_dt[, rate.code := tmp_rate.code]
  tmp_dt[, is_include.hdd.term := TRUE]
  dt_estimates_ate_w.hdd.term <- rbind(dt_estimates_ate_w.hdd.term, tmp_dt)
}

dt_estimates_rate.change_wo.hdd.term <- setDT(NULL)
for (idx in 1) {
  tmp_dt <- get_estimates.from.felm(
    reg.results_rate.change_wo.hdd.term,
    level = 0.95, fe = FALSE, se.type = "cluster"
  )
  tmp_dt[, model := "Rate Change"]
  tmp_dt[, rate.code := NA]
  tmp_dt[, is_include.hdd.term := FALSE]
  dt_estimates_rate.change_wo.hdd.term <-
    rbind(dt_estimates_rate.change_wo.hdd.term, tmp_dt)
}

dt_estimates_rate.change_w.hdd.term <- setDT(NULL)
for (idx in 1) {
  tmp_dt <- get_estimates.from.felm(
    reg.results_rate.change_w.hdd.term,
    level = 0.95, fe = FALSE, se.type = "cluster"
  )
  tmp_dt[, model := "Rate Change"]
  tmp_dt[, rate.code := NA]
  tmp_dt[, is_include.hdd.term := TRUE]
  dt_estimates_rate.change_w.hdd.term <-
    rbind(dt_estimates_rate.change_w.hdd.term, tmp_dt)
}

dt_estimates <- rbind(
  dt_estimates_ate_wo.hdd.term,
  dt_estimates_ate_w.hdd.term,
  dt_estimates_rate.change_wo.hdd.term,
  dt_estimates_rate.change_w.hdd.term
)


# TODO: 4. Create a DT that includes simulation results
tmp_dt_hdds <- data.table(hdd = seq(0, 32, by = 0.01))

# TODO: 4.1. Simulation based on ATE
rate.codes <- dt_estimates[!is.na(rate.code), .N, by = .(rate.code)]$rate.code
dt_simulation_dynamic_ate <- setDT(NULL)
for (i in c(TRUE, FALSE)) {
  tmp_dt <-
    copy(tmp_dt_hdds) %>%
      .[, is_include.hdd.term := i]
  tmp_dt[, `:=` (tariff = "A", rate.change = 6)]
  tmp_dt[8 <= hdd & hdd <= 16, `:=` (tariff = "B", rate.change = 12)]
  tmp_dt[16 <= hdd & hdd <= 24, `:=` (tariff = "C", rate.change = 18)]
  tmp_dt[24 <= hdd & hdd <= 32, `:=` (tariff = "D", rate.change = 24)]
  for (code in rate.codes) {
    tmp_dt[
      tariff == toupper(code),
      `:=` (
        estimate_temp =
          dt_estimates[
            str_detect(term, "^hdd_all.+?treatment.+?post") &
              rate.code == code &
              is_include.hdd.term == i
          ]$estimate,
        estimate_temp_w.rate.change = NA,
        estimate_non.temp =
          dt_estimates[
            str_detect(
              term, "^is_treatment.and.post"
            ) &
              rate.code == code &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp_w.rate.change = NA
      )
    ]
  }
  dt_simulation_dynamic_ate <- rbind(dt_simulation_dynamic_ate, tmp_dt)
}
dt_simulation_dynamic_ate[
  ,
  `:=` (
    kwh_temp = estimate_temp * hdd,
    kwh_non.temp = estimate_non.temp
  )
]
dt_simulation_dynamic_ate[, kwh_total := kwh_temp + kwh_non.temp]
dt_simulation_dynamic_ate[, structure := "Dynamic: ATE"]

# TODO: 4.2. Simulation based on rate change
# TODO: 4.2.1. Static TOU
dt_simulation_static_rate.change <- setDT(NULL)
for (i in c(TRUE, FALSE)) {
  tmp_dt <-
    copy(tmp_dt_hdds) %>%
      .[
        ,
        `:=` (
          is_include.hdd.term = i,
          tariff = NA
        )
      ]
  tmp_dt[, rate.change := 15]
  if (i == FALSE) {
    tmp_dt[
      ,
      `:=` (
        estimate_temp = 0,
        estimate_temp_w.rate.change =
          dt_estimates[
            str_detect(term, "hdd.times.+?treatment.+?post") &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp = 0,
        estimate_non.temp_w.rate.change =
          dt_estimates[
            str_detect(
              term, "^treatment.+?post"
            ) &
              is_include.hdd.term == i
          ]$estimate
      )
    ]
  } else {
    tmp_dt[
      ,
      `:=` (
        estimate_temp =
          dt_estimates[
            str_detect(term, "hdd_all_60f_by_is_treatment.and.post") &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_temp_w.rate.change =
          dt_estimates[
            str_detect(term, "hdd.times.+?treatment.+?post") &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp =
          dt_estimates[
            str_detect(
              term, "is_treatment.and.postTRUE"
            ) &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp_w.rate.change =
          dt_estimates[
            str_detect(
              term, "^treatment.+?post"
            ) &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate
      )
    ]
  }
  dt_simulation_static_rate.change <-
    rbind(dt_simulation_static_rate.change, tmp_dt)
}
dt_simulation_static_rate.change[
  ,
  `:=` (
    kwh_temp =
      (estimate_temp + (estimate_temp_w.rate.change * rate.change)) * hdd,
    kwh_non.temp =
      estimate_non.temp + (estimate_non.temp_w.rate.change * rate.change)
  )
]
dt_simulation_static_rate.change[, kwh_total := kwh_temp + kwh_non.temp]
dt_simulation_static_rate.change[, structure := "Static: Rate Change"]

# TODO: 4.2.2. Dynamic TOU
dt_simulation_dynamic_rate.change <- setDT(NULL)
for (i in c(TRUE, FALSE)) {
  tmp_dt <-
    copy(tmp_dt_hdds) %>%
      .[
        ,
        `:=` (
          is_include.hdd.term = i,
          tariff = NA
        )
      ]
  tmp_dt[, rate.change := 6]
  tmp_dt[8 <= hdd & hdd <= 16, rate.change := 12]
  tmp_dt[16 <= hdd & hdd <= 24, rate.change := 18]
  tmp_dt[24 <= hdd & hdd <= 32, rate.change := 24]
  if (i == FALSE) {
    tmp_dt[
      ,
      `:=` (
        estimate_temp = 0,
        estimate_temp_w.rate.change =
          dt_estimates[
            str_detect(term, "hdd.times.+?treatment.+?post") &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp = 0,
        estimate_non.temp_w.rate.change =
          dt_estimates[
            str_detect(
              term, "^treatment.+?post"
            ) &
              is_include.hdd.term == i
          ]$estimate
      )
    ]
  } else {
    tmp_dt[
      ,
      `:=` (
        estimate_temp =
          dt_estimates[
            str_detect(term, "hdd_all_60f_by_is_treatment.and.post") &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_temp_w.rate.change =
          dt_estimates[
            str_detect(term, "hdd.times.+?treatment.+?post") &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp =
          dt_estimates[
            str_detect(
              term, "is_treatment.and.postTRUE"
            ) &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate,
        estimate_non.temp_w.rate.change =
          dt_estimates[
            str_detect(
              term, "^treatment.+?post"
            ) &
              model == "Rate Change" &
              is_include.hdd.term == i
          ]$estimate
      )
    ]
  }
  dt_simulation_dynamic_rate.change <-
    rbind(dt_simulation_dynamic_rate.change, tmp_dt)
}
dt_simulation_dynamic_rate.change[
  ,
  `:=` (
    kwh_temp =
      (estimate_temp + (estimate_temp_w.rate.change * rate.change)) * hdd,
    kwh_non.temp =
      estimate_non.temp + (estimate_non.temp_w.rate.change * rate.change)
  )
]
dt_simulation_dynamic_rate.change[, kwh_total := kwh_temp + kwh_non.temp]
dt_simulation_dynamic_rate.change[, structure := "Dynamic: Rate Change"]

# TODO: 4.3. Combine DTs created above
dt_simulation <- rbind(
  dt_simulation_dynamic_ate,
  dt_simulation_static_rate.change,
  dt_simulation_dynamic_rate.change
)

# TODO: 4.4. Add columns to compute differences
cols_to.extract <- c(
  "hdd", "is_include.hdd.term", "kwh_temp", "kwh_non.temp", "kwh_total"
)
tmp_dt <- dt_simulation[rate.change == 15, .SD, .SDcols = cols_to.extract]
setnames(
  tmp_dt,
  old = c("kwh_temp", "kwh_non.temp", "kwh_total"),
  new = c("ref.kwh_temp", "ref.kwh_non.temp", "ref.kwh_total")
)
dt_simulation_w.difference <- merge(
  x = dt_simulation,
  y = tmp_dt,
  on = c("hdd", "is_include.hdd.term"),
  all.x = TRUE
)
dt_simulation_w.difference[
  ,
  `:=` (
    diff.in.kwh_temp = kwh_temp - ref.kwh_temp,
    diff.in.kwh_non.temp = kwh_non.temp - ref.kwh_non.temp,
    diff.in.kwh_total = kwh_total - ref.kwh_total
  )
]


# TODO: 5. Create figures from the estimates
# TODO: 5.1. Set plot options
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)
color.pal_signal <- unikn::usecol(pal_signal, n = 3)

# TODO: 5.2. Modify the DT
levels <- c("Static: Rate Change", "Dynamic: Rate Change", "Dynamic: ATE")
dt_simulation_w.difference[, structure := factor(structure, levels = levels)]

# TODO: 5.3. Create ggplot objects
plot_reduction <-
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dotdash", alpha = 0.5) +
    geom_vline(
      xintercept = seq(8, 24, by = 8),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_line(
      data = dt_simulation_w.difference,
      aes(
        x = hdd, y = kwh_total * 2,
        color = structure, linetype = is_include.hdd.term
      )
    ) +
    scale_x_continuous(breaks = seq(0, 32, by = 1)) +
    scale_y_continuous(labels = scales::label_comma(accuracy = 0.001)) +
    scale_color_manual(values = color.pal_signal) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    labs(
      x = "\nHeating Degree Days",
      y = "Reduction in Consumption (kWh per 1-Hour Interval)\n",
      color = "Tariff Structures",
      linetype = "Incl. HDD Terms?",
      subtitle = "\nPanel A: Reduction in Consumption"
    ) +
    plot.options

plot_difference <-
  ggplot() +
    geom_vline(
      xintercept = seq(8, 24, by = 8),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_line(
      data = dt_simulation_w.difference,
      aes(
        x = hdd, y = diff.in.kwh_total,
        color = structure, linetype = is_include.hdd.term
      )
    ) +
    scale_x_continuous(breaks = seq(0, 32, by = 1)) +
    scale_y_continuous(labels = scales::label_comma(accuracy = 0.001)) +
    scale_color_manual(values = color.pal_signal) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    labs(
      x = "\nHeating Degree Days",
      y = "Difference in Consumption Reduction (kWh per 1-Hour Interval)\n",
      color = "Tariff Structures",
      linetype = "Incl. HDD Terms?",
      subtitle = "\n\nPanel B: Difference in Consumption Reduction"
    ) +
    plot.options

export_plot_in.png(
  paste(
    "/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/07_CER-Trials/02_Figures",
    "CER_Simulation_Reductions.png",
    sep = "/"
  ),
  plot = gridExtra::grid.arrange(plot_reduction, plot_difference, nrow = 2),
  width = 40, height = 35, units = "cm"
)

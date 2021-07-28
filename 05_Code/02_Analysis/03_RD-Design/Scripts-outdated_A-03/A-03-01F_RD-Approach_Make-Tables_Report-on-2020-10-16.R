# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02F
# #
# > Purpose of the script(s)
# # : Make tables that are included in the report on RD design at Oct. 16, 2020.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(stargazer)
library(sandwich)
library(stringr)
library(lfe)
library(parallel)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <-
    paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep= "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_RD <- "RD-Approach"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Approach.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Plots", sep = "/")
# # 2.2. Path at which Table(s) will be saved
DIR_TO.SAVE_TABLES <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Tables", sep = "/")


# ------- Define parameter(s) -------
# # 1. For parallel computing
n_cores <- detectCores() / 2


# ------- Define function(s) -------
# (Not Applicable)
estimate.by.bw <- function (model, bw) {
  if (bw == 0) {
    tmp_results <-
      felm(get(model), data = dt_for.reg[is_balanced.ids_bw_na == TRUE])
    return(tmp_results)
  } else {
    tmp_condition <-
      paste0(
        bw * -1,
        paste0(
          " <= kwh_total_in.percent_normalize_period0 & ",
          "kwh_total_in.percent_normalize_period0 <= "
        ),
        bw,
        paste0(" & is_balanced.ids_bw_", bw, " == TRUE")
      )

    tmp_results <-
      felm(get(model), data = dt_for.reg[eval(parse(text = tmp_condition))])
    return(tmp_results)
  }
}


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date= "(^date)|(_from$)|(_to$)",
    is_drop.index_cols= TRUE
  )

# # 2. Check primary keys of the DTs
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Define Regression Models
# --------------------------------------------------
# ------- Define regression models -------
# # 1. Models with FEs
# # 1.1. Define models with FEs
model_1p_wo.time.fe <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period |
      factor(ids)
  )
model_1p <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )
model_1p_inter <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )
model_2p <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        I(kwh_total_in.percent_normalize_period0^2) + cdd_daily.avg_period +
        hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )
model_2p_inter <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^2):is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )
model_3p <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^3) + cdd_daily.avg_period +
        hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )
model_3p_inter <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^2):is_treated +
        I(kwh_total_in.percent_normalize_period0^3) +
        I(kwh_total_in.percent_normalize_period0^3):is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period |
      factor(ids) + factor(billing.ym_mid)
  )

# # 1.2. Create a vector including names of models
models <-
  c(
    "model_1p_wo.time.fe", "model_1p", "model_1p_inter",
    "model_2p", "model_2p_inter",
    "model_3p", "model_3p_inter"
  )


# # 2. Models without FEs
# # 2.1. Define models without FEs
model_1p_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )
model_1p_inter_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )
model_2p_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        I(kwh_total_in.percent_normalize_period0^2) + cdd_daily.avg_period +
        hdd_daily.avg_period
  )
model_2p_inter_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^2):is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )
model_3p_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^3) + cdd_daily.avg_period +
        hdd_daily.avg_period
  )
model_3p_inter_wo.fes <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        kwh_total_in.percent_normalize_period0:is_treated +
        I(kwh_total_in.percent_normalize_period0^2) +
        I(kwh_total_in.percent_normalize_period0^2):is_treated +
        I(kwh_total_in.percent_normalize_period0^3) +
        I(kwh_total_in.percent_normalize_period0^3):is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )

# # 2.2. Create a vector including names of models
models_wo.fes <-
  c(
    "model_1p_wo.fes", "model_1p_inter_wo.fes",
    "model_2p_wo.fes", "model_2p_inter_wo.fes",
    "model_3p_wo.fes", "model_3p_inter_wo.fes"
  )


# --------------------------------------------------
# Run regressions
# --------------------------------------------------
# ------- Run regressions: Models with FEs -------
# # 1. Run regressions
list_results_5 <- mclapply(models, estimate.by.bw, bw = 5, mc.cores = n_cores)
list_results_10 <- mclapply(models, estimate.by.bw, bw = 10, mc.cores = n_cores)
# ## Note:
# ## For bandwidth N/A, 15%, and 20%, "mclapply" does not return regression
# ## results properly.

vct_bw <- c(0, 15, 20)
for (bw in vct_bw) {
  if (bw == 0) {
    for (model in models) {
      tmp_name_results <-
      paste("results", bw, str_replace(model, "^model_", ""), sep = "_")
      assign(tmp_name_results, estimate.by.bw(model, bw))
    }
  } else {
    for (model in models) {
      tmp_name_results <-
      paste("results", bw, str_replace(model, "^model_", ""), sep = "_")
      assign(tmp_name_results, estimate.by.bw(model, bw))
    }
  }
}

# # 2. Generate lists of regression results
list_results_0 <-
  list(
    results_0_1p_wo.time.fe, results_0_1p, results_0_1p_inter,
    results_0_2p, results_0_2p_inter,
    results_0_3p, results_0_3p_inter
  )
list_results_15 <-
list(
  results_15_1p_wo.time.fe, results_15_1p, results_15_1p_inter,
  results_15_2p, results_15_2p_inter,
  results_15_3p, results_15_3p_inter
)
list_results_20 <-
  list(
    results_20_1p_wo.time.fe, results_20_1p, results_20_1p_inter,
    results_20_2p, results_20_2p_inter,
    results_20_3p, results_20_3p_inter
  )


# ------- Run regressions: Models without FEs -------
# # 1. Run regressions
list_results_5_wo.fes <-
  mclapply(models_wo.fes, estimate.by.bw, bw = 5, mc.cores = n_cores)
list_results_10_wo.fes <-
  mclapply(models_wo.fes, estimate.by.bw, bw = 10, mc.cores = n_cores)
# ## Note:
# ## For bandwidth N/A and 20%, "mclapply" does not return regression
# ## results properly.

vct_bw <- c(0, 20)
for (bw in vct_bw) {
  if (bw == 0) {
    for (model in models) {
      tmp_name_results <-
      paste(
        "results", bw, str_replace(model, "^model_", ""), "wo.fes", sep = "_"
      )
      assign(tmp_name_results, estimate.by.bw(model, bw))
    }
  } else {
    for (model in models) {
      tmp_name_results <-
      paste(
        "results", bw, str_replace(model, "^model_", ""), "wo.fes", sep = "_"
      )
      assign(tmp_name_results, estimate.by.bw(model, bw))
    }
  }
}

# # 2. Generate lists of regression results
list_results_0_wo.fes <-
  list(
    results_0_1p_wo.fes, results_0_1p_inter_wo.fes,
    results_0_2p_wo.fes, results_0_2p_inter_wo.fes,
    results_0_3p_wo.fes, results_0_3p_inter_wo.fes
  )
list_results_20_wo.fes <-
  list(
    results_20_1p_wo.fes, results_20_1p_inter_wo.fes,
    results_20_2p_wo.fes, results_20_2p_inter_wo.fes,
    results_20_3p_wo.fes, results_20_3p_inter_wo.fes
  )


# ------- Run regressions: Models with FEs for various bandwidths -------
# # 1. Run regressions
vct_bw <- c(0, 5, 10, 15, seq(20, 90, by = 10))
for (bw in vct_bw) {
  if (bw == 0) {
    tmp_name_results <- paste("results", bw, sep = "_")
    assign(tmp_name_results, estimate.by.bw("model_1p_inter", bw))
  } else {
    tmp_name_results <- paste("results", bw, sep = "_")
    assign(tmp_name_results, estimate.by.bw("model_1p_inter", bw))
  }
}

# # 2.  Generate a list of regression results
list_results_bws <-
  list(
    results_0, results_90, results_80, results_70, results_60,
    results_50, results_40, results_30, results_20, results_15,
    results_10, results_5
  )


# ------- Run regressions: Models without FEs for various bandwidths -------
# # 1. Run regressions
vct_bw <- c(0, 5, 10, 15, seq(20, 90, by = 10))
for (bw in vct_bw) {
  if (bw == 0) {
    tmp_name_results <- paste("results", bw, "wo.fes", sep = "_")
    assign(tmp_name_results, estimate.by.bw("model_1p_inter_wo.fes", bw))
  } else {
    tmp_name_results <- paste("results", bw, "wo.fes", sep = "_")
    assign(tmp_name_results, estimate.by.bw("model_1p_inter_wo.fes", bw))
  }
}

# # 2. Generate a list of regression results
list_results_bws_wo.fes <-
  list(
    results_0_wo.fes, results_90_wo.fes, results_80_wo.fes, results_70_wo.fes,
    results_60_wo.fes, results_50_wo.fes, results_40_wo.fes, results_30_wo.fes,
    results_20_wo.fes, results_15_wo.fes, results_10_wo.fes, results_5_wo.fes
  )


# --------------------------------------------------
# Generate Regression Tables
# --------------------------------------------------
# ------- Export tables -------
# # 1. Generate objects that will be used to export the regression results
# # 1.1. Rename variable names
vct_covariate.labels <-
  c(
    "NC0",
    "1[Treated]",
    "NC02",
    "NC03",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "NC0 * 1[Treated]",
    "NC02 * 1[Treated]",
    "NC03 * 1[Treated]"
  )
vct_covariate.labels_wo.fes <-
  c(
    "NC0",
    "1[Treated]",
    "NC02",
    "NC03",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "NC0 * 1[Treated]",
    "NC02 * 1[Treated]",
    "NC03 * 1[Treated]",
    "Constant"
  )
vct_covariate.labels_bws <-
  c(
    "NC0",
    "1[Treated]",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "NC0 * 1[Treated]"
  )
vct_covariate.labels_bws_wo.fes <-
  c(
    "NC0",
    "1[Treated]",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "NC0 * 1[Treated]",
    "Constant"
  )

# # 1.2. Add lines
# # 1.2.1. Models with FEs
list_add.lines_NA <-
  list(
    c("Bandwidth", rep("N/A", times = 7)),
    c("FEs: Account-Premise IDs", rep("Yes", times = 7)),
    c("FEs: Billing Year-Month", c("No", rep("Yes", times = 6)))
  )
list_add.lines_5 <-
  list(
    c("Bandwidth", rep("5%", times = 7)),
    c("FEs: Account-Premise IDs", rep("Yes", times = 7)),
    c("FEs: Billing Year-Month", c("No", rep("Yes", times = 6)))
  )
list_add.lines_10 <-
  list(
    c("Bandwidth", rep("10%", times = 7)),
    c("FEs: Account-Premise IDs", rep("Yes", times = 7)),
    c("FEs: Billing Year-Month", c("No", rep("Yes", times = 6)))
  )
list_add.lines_15 <-
  list(
    c("Bandwidth", rep("15%", times = 7)),
    c("FEs: Account-Premise IDs", rep("Yes", times = 7)),
    c("FEs: Billing Year-Month", c("No", rep("Yes", times = 6)))
  )
list_add.lines_20 <-
  list(
    c("Bandwidth", rep("20%", times = 7)),
    c("FEs: Account-Premise IDs", rep("Yes", times = 7)),
    c("FEs: Billing Year-Month", c("No", rep("Yes", times = 6)))
  )

list_add.lines_na_wo.fes <-
  list(
    c("Bandwidth", rep("N/A", times = 6)),
    c("FEs: Account-Premise IDs", rep("No", times = 6)),
    c("FEs: Billing Year-Month", rep("No", times = 6))
  )
list_add.lines_5_wo.fes <-
  list(
    c("Bandwidth", rep("5%", times = 6)),
    c("FEs: Account-Premise IDs", rep("No", times = 6)),
    c("FEs: Billing Year-Month", rep("No", times = 6))
  )
list_add.lines_10_wo.fes <-
  list(
    c("Bandwidth", rep("10%", times = 6)),
    c("FEs: Account-Premise IDs", rep("No", times = 6)),
    c("FEs: Billing Year-Month", rep("No", times = 6))
  )
list_add.lines_20_wo.fes <-
  list(
    c("Bandwidth", rep("20%", times = 6)),
    c("FEs: Account-Premise IDs", rep("No", times = 6)),
    c("FEs: Billing Year-Month", rep("NO", times = 6))
  )
# # 1.2.2. Models without FEs
list_add.lines_bws <-
  list(
    c("Bandwidth", "N/A", paste0(c(seq(90, 20, by = -10), 15, 10, 5), "%")),
    c("FEs: Account-Premise IDs", rep("Yes", times = 12)),
    c("FEs: Billing Year-Month", rep("Yes", times = 12))
  )
list_add.lines_bws_wo.fes <-
  list(
    c("Bandwidth", "N/A", paste0(c(seq(90, 20, by = -10), 15, 10, 5), "%")),
    c("FEs: Account-Premise IDs", rep("No", times = 12)),
    c("FEs: Billing Year-Month", rep("No", times = 12))
  )


# # 2. Export tables
# # 2.1. Print tables
# # 2.1.1. For models with FEs
stargazer(
  list_results_0,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_na,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_20,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_20,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_15,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_15,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_10,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_10,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_5,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_5,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_bws,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_bws,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_bws,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
# # 2.1.2. For models without FEs
stargazer(
  list_results_0_wo.fes,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_wo.fes,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_na_wo.fes,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_20_wo.fes,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_wo.fes,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_20_wo.fes,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_10_wo.fes,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_wo.fes,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_10_wo.fes,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_5_wo.fes,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_wo.fes,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_5_wo.fes,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)
stargazer(
  list_results_bws_wo.fes,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_bws_wo.fes,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines_bws_wo.fes,
  #se = list_se,
  font.size = "tiny",
  #omit = c(""),
  omit.stat = c("ser", "f")
)

# # 2.2. Export table(s) in LaTex format
# # 2.2.1. For models with FEs
PATH_TO.SAVE_TABLE_NA <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-NA.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_0,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_NA,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_NA
)

PATH_TO.SAVE_TABLE_20 <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-20.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_20,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_20,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_20
)

PATH_TO.SAVE_TABLE_15 <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-15.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_15,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_15,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_15
)

PATH_TO.SAVE_TABLE_10 <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-10.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_10,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_10,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_10
)

PATH_TO.SAVE_TABLE_5 <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-5.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_5,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_5,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_5
)

PATH_TO.SAVE_TABLE_BWS <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BWs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_bws,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_bws,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_bws,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_BWS
)

# # 2.2.2. For models without FEs
PATH_TO.SAVE_TABLE_NA_WO.FES <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-NA-without-FEs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_0_wo.fes,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_wo.fes,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_NA_wo.fes,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_NA_WO.FES
)

PATH_TO.SAVE_TABLE_20_WO.FES <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-20-without-FEs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_20_wo.fes,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_wo.fes,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_20_wo.fes,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_20_WO.FES
)

PATH_TO.SAVE_TABLE_10_WO.FES <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-10-without-FEs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_10_wo.fes,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_wo.fes,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_10_wo.fes,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_10_WO.FES
)

PATH_TO.SAVE_TABLE_5_WO.FES <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BW-5-without-FEs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_5_wo.fes,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_wo.fes,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_5_wo.fes,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_5_WO.FES
)

PATH_TO.SAVE_TABLE_BWS_WO.FES <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Design_BWs-without-FEs.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_bws_wo.fes,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_bws_wo.fes,
    dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines_bws_wo.fes,
    #se = list_se,
    font.size = "small",
    #omit = c(""),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_BWS_WO.FES
)

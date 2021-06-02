# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02V
# #
# > Purpose of the script(s)
# # : To

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
library(quantreg)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data for RD Design
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design_Reduced.RData"
PATH_TO.LOAD_RD <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep = "/"
)

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")
FILE_TO.SAVE_RESULTS <-
  "DT_For-Regression_RD-Design_Quantile-Regression-Results.parquet"
PATH_TO.SAVE_RESULTS <-
  paste(DIR_TO.SAVE_RESULTS, FILE_TO.SAVE_RESULTS, sep = "/")


# ------- Define parameter(s) -------
YEAR_LOWER <- 2005
YEAR_UPPER <- 2011


# ------- Define function(s) -------
estimate.wBW_quantile <- function(
  formula, tau, bw, rate.codes, year_lower, year_upper, season
) {
  tmp_results <- rq(
    formula = formula,
    tau = tau,
    data = dt_for.reg[
      bw * -1 <= kwh_total_in.percent_normalize_period0 &
        kwh_total_in.percent_normalize_period0 <= bw &
        rate_code_normalize_period0 %in% rate.codes &
        billing.year_mid_period0 %in% c(year_lower:year_upper) &
        season_before_period0 == season_after_period0 &
        season_before_period0 == season
    ],
    method = "fn"
  ) %>%
    .$coefficients %>%
    as.data.table(., keep.rownames = TRUE) %>%
    setnames(., "rn", "var_independent")

  tmp_results[
    ,
    `:=` (
      bw_in.str = paste0(bw, "%"),
      bw_in.percent = bw,
      rate.code = str_c(rate.codes, collapse = " \\& "),
      season = season
    )
  ]

  return(tmp_results)
}


run.regressions_by.model <- function(
  model_wo.inter, model_w.inter, bw, tau, rate.codes, year_lower, year_upper,
  season
) {
  tmp_dt_results_wo.inter <- lapply(
    X = bw,
    FUN = estimate.wBW_quantile,
    formula = model_wo.inter,
    tau = tau,
    rate.codes = rate.codes,
    year_lower = year_lower, year_upper = year_upper,
    season = season
  ) %>% rbindlist(.)

  tmp_dt_results_w.inter <- lapply(
    X = seq(10, 90, by = 10),
    FUN = estimate.wBW_quantile,
    formula = model_w.inter,
    tau = seq(0.1, 0.9, by = 0.1),
    rate.codes = rate.codes,
    year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
    season = season
  ) %>% rbindlist(.)

  return(
    rbind(
      tmp_dt_results_wo.inter[, model := "Without Interaction"],
      tmp_dt_results_w.inter[, model := "With Interaction"]
    )
  )
}



# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
load(PATH_TO.LOAD_RD)

# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------
lm_rd_wo.fes_linear <-
  formula(
    kwh_daily.avg ~
      is_treated_period0 +
      kwh_total_in.percent_normalize_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )

lm_rd_wo.fes_linear.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_period0 +
      kwh_total_in.percent_normalize_period0 +
      is_treated_period0:kwh_total_in.percent_normalize_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )


bws <- seq(10, 90, by = 10)
taus <- seq(0.1, 0.9, by = 0.1)


dt_results_g.code_summer <- run.regressions_by.model(
  model_wo.inter = lm_rd_wo.fes_linear,
  model_w.inter = lm_rd_wo.fes_linear.w.inter,
  bw = bws, tau = taus,
  rate.codes = c("RSGH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
)

dt_results_g.code_winter <- run.regressions_by.model(
  model_wo.inter = lm_rd_wo.fes_linear,
  model_w.inter = lm_rd_wo.fes_linear.w.inter,
  bw = bws, tau = taus,
  rate.codes = c("RSGH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
)



dt_results_ce.codes_summer <- run.regressions_by.model(
  model_wo.inter = lm_rd_wo.fes_linear,
  model_w.inter = lm_rd_wo.fes_linear.w.inter,
  bw = bws, tau = taus,
  rate.codes = c("RSCH", "RSEH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
)

dt_results_ce.codes_winter <- run.regressions_by.model(
  model_wo.inter = lm_rd_wo.fes_linear,
  model_w.inter = lm_rd_wo.fes_linear.w.inter,
  bw = bws, tau = taus,
  rate.codes = c("RSCH", "RSEH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
)


# -------  -------
dt_rd_results_quantile_unmelted <- rbind(
  dt_results_g.code_summer, dt_results_g.code_winter,
  dt_results_ce.codes_summer, dt_results_ce.codes_winter
)



# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------
select <- str_detect(names(dt_rd_results_quantile_unmelted), "^tau=")
id.vars <- names(dt_rd_results_quantile_unmelted)[!select]
measure.vars <- names(dt_rd_results_quantile_unmelted)[select]
dt_rd_results_quantile <- melt(
  dt_rd_results_quantile_unmelted,
  id.vars = id.vars,
  measure.vars = measure.vars,
  variable.name = "tau",
  value.name = "estimates"
)

dt_rd_results_quantile[, tau := str_replace(tau, "tau= ", "") %>% as.numeric(.)]


list_var_independent <- list(
  `is_treated_period0TRUE` =
    "1[Treated]:0",
  `kwh_total_in.percent_normalize_period0` =
    "NC:0",
  `is_treated_period0TRUE:kwh_total_in.percent_normalize_period0` =
    "NC:0 * 1[Treated]:0",
  `cdd_daily.avg_period` =
    "Daily Average CDDs",
  `hdd_daily.avg_period` =
    "Daily Average HDDs",
  `(Intercept)` =
    "(Constant)"
)
dt_rd_results_quantile[
  ,
  var_independent :=
    sapply(var_independent, function(x) list_var_independent[[x]])
]


order_model <- c(
  "Without Interaction",
  "With Interaction"
)
order_bw <- paste0(bws, "%")
order_var_independent <- unlist(list_var_independent, use.names = FALSE)

dt_rd_results_quantile[
  ,
  `:=` (
    model = factor(model, levels = order_model, ordered = TRUE),
    bw_in.str = factor(bw_in.str, levels = order_bw, ordered = TRUE),
    var_independent = factor(
      var_independent, levels = order_var_independent
    )
  )
]


# --------------------------------------------------
# Save the DT modified
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
  dt_rd_results_quantile,
  sink = PATH_TO.SAVE_RESULTS,
  version = "1.0",
  compression = "snappy",
  use_dictionary = TRUE
)

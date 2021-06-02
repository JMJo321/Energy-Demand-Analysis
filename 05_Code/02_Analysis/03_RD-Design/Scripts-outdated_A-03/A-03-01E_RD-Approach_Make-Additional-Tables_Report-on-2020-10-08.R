# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02E
# #
# > Purpose of the script(s)
# # : Generate additional Tables showing Regression Results,
# #   based on Kevin's Comments.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(stargazer)
library(sandwich)
library(stringr)
library(lfe)
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
# # 1. Set bandwidths
for (bw in 1:9) {
  tmp_name_upper <- paste0("NORMALIZED.QTY_UPPER_", bw)
  tmp_name_lower <- paste0("NORMALIZED.QTY_LOWER_", bw)

  assign(tmp_name_upper, paste0(bw, "0") %>% as.numeric(.))
  assign(tmp_name_lower, get(tmp_name_upper) * -1)
}


# ------- Define function(s) -------
# (Not Applicable)


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
# Run regressions
# --------------------------------------------------
# ------- Set Model(s) -------
# # 1. A model having daily average consumption as the dependent variable
felm.model_daily_monthly_controls.wFEs <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period | factor(ids)
  )


# ------- Run Regression(s), and calculate robust standard errors -------
# # 1. Dependent variable: Daily average consumption
for (case in 1:10) {
  tmp.name_results <- paste0("results_daily_monthly_controls.wFEs_bw", case)
  tmp.name_cov <- paste0("cov_daily_monthly_controls.wFEs_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_monthly_controls.wFEs_bw", case)

  if (case == 10) {
    assign(
      tmp.name_results,
      felm(felm.model_daily_monthly_controls.wFEs, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      felm(
        felm.model_daily_monthly_controls.wFEs,
        data =
          dt_for.reg[
            get(paste("NORMALIZED.QTY_LOWER", case, sep = "_")) <=
              kwh_total_in.percent_normalize_period0 &
              kwh_total_in.percent_normalize_period0 <=
                get(paste("NORMALIZED.QTY_UPPER", case, sep = "_"))
         ]
      )
    )
  }
  assign(
    tmp.name_robust.se,
    get(tmp.name_results) %>% summary(.) %>% coef(.) %>% .[, "Std. Error"]
  )
}


# --------------------------------------------------
# Generate Simple Tables
# --------------------------------------------------
# ------- Make a table w.r.t. bandwidth -------
for (case in 1:9) {
  tmp_name_below <- paste0("n_bw", case, "_below")
  tmp_name_above <- paste0("n_bw", case, "_above")

  assign(
    tmp_name_below,
    dt_for.reg[
      get(paste("NORMALIZED.QTY_LOWER", case, sep = "_")) <=
        kwh_total_in.percent_normalize_period0 &
        kwh_total_in.percent_normalize_period0 <= 0,
      .N
    ]
  )

  assign(
    tmp_name_above,
    dt_for.reg[
      0 < kwh_total_in.percent_normalize_period0 &
        kwh_total_in.percent_normalize_period0 <=
          get(paste("NORMALIZED.QTY_UPPER", case, sep = "_")),
      .N
    ]
  )
}
n_bw10_below <- dt_for.reg[kwh_total_in.percent_normalize_period0 <= 0, .N]
n_bw10_above <- dt_for.reg[kwh_total_in.percent_normalize_period0 > 0, .N]

tbl_over.years <-
  data.table(
    "Bandwidth" =
      c("N/A", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%"),
    "Control" =
      c(
        n_bw10_below, n_bw9_below, n_bw8_below, n_bw7_below, n_bw6_below,
        n_bw5_below, n_bw4_below, n_bw3_below, n_bw2_below, n_bw1_below
      ),
    "Treatment" =
      c(
        n_bw10_above, n_bw9_above, n_bw8_above, n_bw7_above, n_bw6_above,
        n_bw5_above, n_bw4_above, n_bw3_above, n_bw2_above, n_bw1_above
      )
  )

tbl_over.years[, Total := Control + Treatment]
hux.tbl_over.years <- as_hux(tbl_over.years)
number_format(hux.tbl_over.years)[2:11, 2:4] <- fmt_pretty()
bottom_border(hux.tbl_over.years)[1,] <- 1
right_border(hux.tbl_over.years)[,1] <- 1
right_border(hux.tbl_over.years)[,3] <- 1
right_border_style(hux.tbl_over.years)[, 1] <- "solid"


# --------------------------------------------------
# Export Results
# --------------------------------------------------
# ------- Export table(s) -------
# # 1. Generate objects that will be used to export the regression results
# # 1.1. For model with relative monthly consumption as the dependent variable
list_results <-
  list(
    results_daily_monthly_controls.wFEs_bw1,
    results_daily_monthly_controls.wFEs_bw2,
    results_daily_monthly_controls.wFEs_bw3,
    results_daily_monthly_controls.wFEs_bw4,
    results_daily_monthly_controls.wFEs_bw5,
    results_daily_monthly_controls.wFEs_bw6,
    results_daily_monthly_controls.wFEs_bw7,
    results_daily_monthly_controls.wFEs_bw8,
    results_daily_monthly_controls.wFEs_bw9,
    results_daily_monthly_controls.wFEs_bw10
  )
list_se <-
  list(
    robust.se_daily_monthly_controls.wFEs_bw1,
    robust.se_daily_monthly_controls.wFEs_bw2,
    robust.se_daily_monthly_controls.wFEs_bw3,
    robust.se_daily_monthly_controls.wFEs_bw4,
    robust.se_daily_monthly_controls.wFEs_bw5,
    robust.se_daily_monthly_controls.wFEs_bw6,
    robust.se_daily_monthly_controls.wFEs_bw7,
    robust.se_daily_monthly_controls.wFEs_bw8,
    robust.se_daily_monthly_controls.wFEs_bw9,
    robust.se_daily_monthly_controls.wFEs_bw10
  )
vct_covariate.labels <-
  c(
    "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
    "1[Treated]",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "Constant"
  )
list_add.lines <-
  list(
    c(
      "Bandwidth",
      paste0(NORMALIZED.QTY_UPPER_1, "%"), paste0(NORMALIZED.QTY_UPPER_2, "%"),
      paste0(NORMALIZED.QTY_UPPER_3, "%"), paste0(NORMALIZED.QTY_UPPER_4, "%"),
      paste0(NORMALIZED.QTY_UPPER_5, "%"), paste0(NORMALIZED.QTY_UPPER_6, "%"),
      paste0(NORMALIZED.QTY_UPPER_7, "%"), paste0(NORMALIZED.QTY_UPPER_8, "%"),
      paste0(NORMALIZED.QTY_UPPER_9, "%"),
      "N/A"
    ),
    c("FEs: Account-Premise IDs", rep("Yes", times = 10))
  )


# # 2. Export tables
# # 2.1. Print tables
# # 2.1.1. Model with daily average consumption as the dependent variable
stargazer(
  list_results,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels,
  dep.var.labels =
    "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines,
  se = list_se,
  font.size = "tiny",
  omit.stat = c("ser", "f")
)

# # 2.2. Export table(s) in LaTex format
# # 2.2.1. Model with relative monthly consumption as the dependent variable
PATH_TO.SAVE_TABLE <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Approach_Daily-Consumption_Additional-Bandwidths.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels,
    dep.var.labels =
      "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines,
    se = list_se,
    font.size = "small",
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE
)

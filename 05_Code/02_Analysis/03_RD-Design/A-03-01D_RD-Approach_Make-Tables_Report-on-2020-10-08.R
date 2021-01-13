# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02D
# #
# > Purpose of the script(s)
# # : Generate Tables showing Regression Results, based on Kevin's Comments.

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
# # 1. For making plots
# # 1.1. Set bandwidth
NORMALIZED.QTY_UPPER_1 <- 20
NORMALIZED.QTY_LOWER_1 <- NORMALIZED.QTY_UPPER_1 * -1

NORMALIZED.QTY_UPPER_2 <- 15
NORMALIZED.QTY_LOWER_2 <- NORMALIZED.QTY_UPPER_2 * -1

NORMALIZED.QTY_UPPER_3 <- 10
NORMALIZED.QTY_LOWER_3 <- NORMALIZED.QTY_UPPER_3 * -1

NORMALIZED.QTY_UPPER_4 <- 5
NORMALIZED.QTY_LOWER_4 <- NORMALIZED.QTY_UPPER_4 * -1

# # 1.2. Set the number of observations randomly sampled
SAMPLE.SIZE <- 10^4


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
# ------- With no controls -------
# # 1. Set a model
# # 1.1. A model having monthly consumption as the dependent variable
# # 1.1.1. A model without control(s)
model_monthly_no.controls <-
  formula(
    kwh_total ~
      kwh_total_in.percent_normalize_period0 + is_treated
  )
# # 1.1.2. A model with control(s)
model_monthly_controls <-
  formula(
    kwh_total ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_period + hdd_period
  )
# # 1.1.3. A model with control(s) and FEs
felm.model_monthly_controls.wFEs <-
  formula(
    kwh_total ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_period + hdd_period | factor(ids)
  )

# # 1.2. A model having daily average consumption as the dependent variable
# # 1.2.1. A model without control(s)
model_daily_mothly_no.controls <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated
  )
# # 1.2.2. A model with control(s)
model_daily_monthly_controls <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )
# # 1.2.3. A model with control(s) and FEs
felm.model_daily_monthly_controls.wFEs <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period | factor(ids)
  )

# # 1.3. A model having daily average consumption as the dependent variable
# # 1.3.1. A model without control(s)
model_daily_daily_no.controls <-
  formula(
    kwh_daily.avg ~
      kwh_daily.avg_in.percent_normalize_period0 + is_treated
  )
# # 1.3.2. A model with control(s)
model_daily_daily_controls <-
  formula(
    kwh_daily.avg ~
      kwh_daily.avg_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period
  )
# # 1.3.3. A model with control(s) and FEs
felm.model_daily_daily_controls.wFEs <-
  formula(
    kwh_daily.avg ~
      kwh_daily.avg_in.percent_normalize_period0 + is_treated +
        cdd_daily.avg_period + hdd_daily.avg_period | factor(ids)
  )


# # 2. Run regressions and calculate robust standard errors
# # 2.1. Dependent variable: Monthly consumption
# # 2.1.1. Case 1: Without Control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_monthly_no.controls_bw", case)
  tmp.name_cov <- paste0("cov_monthly_no.controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_monthly_no.controls_bw", case)

  if (case == 0) {
    assign(tmp.name_results, lm(model_monthly_no.controls, data = dt_for.reg))
  } else {
    assign(
      tmp.name_results,
      lm(
        model_monthly_no.controls,
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

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.1.2. Case 2: With control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_monthly_controls_bw", case)
  tmp.name_cov <- paste0("cov_monthly_controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_monthly_controls_bw", case)

  if (case == 0) {
    assign(tmp.name_results, lm(model_monthly_controls, data = dt_for.reg))
  } else {
    assign(
      tmp.name_results,
      lm(
        model_monthly_controls,
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

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.1.3. Case 3: With control(s) and FEs
for (case in 0:4) {
  tmp.name_results <- paste0("results_monthly_controls.wFEs_bw", case)
  tmp.name_cov <- paste0("cov_monthly_controls.wFEs_bw", case)
  tmp.name_robust.se <- paste0("robust.se_monthly_controls.wFEs_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results,
      felm(felm.model_monthly_controls.wFEs, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      felm(
        felm.model_monthly_controls.wFEs,
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

# # 2.2. Dependent variable: Daily average consumption and
# #      Independent variable: `is_treated`
# # 2.2.1. Case 1: Without control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_monthly_no.controls_bw", case)
  tmp.name_cov <- paste0("cov_daily_monthly_no.controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_monthly_no.controls_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results, lm(model_daily_mothly_no.controls, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      lm(
        model_daily_mothly_no.controls,
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

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.2.2. Case 2: With control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_monthly_controls_bw", case)
  tmp.name_cov <- paste0("cov_daily_monthly_controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_monthly_controls_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results, lm(model_daily_monthly_controls, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      lm(
        model_daily_monthly_controls,
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

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.2.3. Case 3: With control(s) and FEs
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_monthly_controls.wFEs_bw", case)
  tmp.name_cov <- paste0("cov_daily_monthly_controls.wFEs_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_monthly_controls.wFEs_bw", case)

  if (case == 0) {
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

# # 2.3. Dependent variable: Daily average consumption and
# #      Independent variable: `is_treated_daily.avg`
# # 2.3.1. Case 1: Without control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_daily_no.controls_bw", case)
  tmp.name_cov <- paste0("cov_daily_daily_no.controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_daily_no.controls_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results, lm(model_daily_daily_no.controls, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      lm(
        model_daily_daily_no.controls,
        data =
          dt_for.reg[
            get(paste("NORMALIZED.QTY_LOWER", case, sep = "_")) <=
              kwh_daily.avg_in.percent_normalize_period0 &
              kwh_daily.avg_in.percent_normalize_period0 <=
                get(paste("NORMALIZED.QTY_UPPER", case, sep = "_"))
         ]
      )
    )
  }

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.2.2. Case 2: With control(s)
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_daily_controls_bw", case)
  tmp.name_cov <- paste0("cov_daily_daily_controls_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_daily_controls_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results, lm(model_daily_daily_controls, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      lm(
        model_daily_daily_controls,
        data =
          dt_for.reg[
            get(paste("NORMALIZED.QTY_LOWER", case, sep = "_")) <=
              kwh_daily.avg_in.percent_normalize_period0 &
              kwh_daily.avg_in.percent_normalize_period0 <=
                get(paste("NORMALIZED.QTY_UPPER", case, sep = "_"))
         ]
      )
    )
  }

  assign(tmp.name_cov, get(tmp.name_results) %>% vcovHC(., type = "HC1"))
  assign(tmp.name_robust.se, get(tmp.name_cov) %>% diag(.) %>% sqrt(.))
}
# # 2.2.3. Case 3: With control(s) and FEs
for (case in 0:4) {
  tmp.name_results <- paste0("results_daily_daily_controls.wFEs_bw", case)
  tmp.name_cov <- paste0("cov_daily_daily_controls.wFEs_bw", case)
  tmp.name_robust.se <- paste0("robust.se_daily_daily_controls.wFEs_bw", case)

  if (case == 0) {
    assign(
      tmp.name_results,
      felm(felm.model_daily_daily_controls.wFEs, data = dt_for.reg)
    )
  } else {
    assign(
      tmp.name_results,
      felm(
        felm.model_daily_daily_controls.wFEs,
        data =
          dt_for.reg[
            get(paste("NORMALIZED.QTY_LOWER", case, sep = "_")) <=
              kwh_daily.avg_in.percent_normalize_period0 &
              kwh_daily.avg_in.percent_normalize_period0 <=
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
# ------- Make a table w.r.t. `is_no.sign.change` -------
tbl_sign.change <-
  dt_for.reg[, .N, by = .(is_treated, is_no.sign.change)]
names(tbl_sign.change) <- c("Treated?", "No Sign Change?", "Observations")
hux.tbl_sign.change <- as_hux(tbl_sign.change)
number_format(hux.tbl_sign.change)[2:3, 3] <- fmt_pretty()
bottom_border(hux.tbl_sign.change)[1,] <- 1
right_border(hux.tbl_sign.change)[, 2] <- 1
right_border_style(hux.tbl_sign.change)[, 2] <- "solid"


# ------- Make a table w.r.t. `is_over.years` -------
n_bw0_below <- dt_for.reg[kwh_total_in.percent_normalize_period0 <= 0, .N]
n_bw0_above <- dt_for.reg[kwh_total_in.percent_normalize_period0 > 0, .N]
for (case in 1:4) {
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
tbl_over.years <-
  data.table(
    "Bandwidth" = c("N/A", "20%", "15%", "10%", "5%"),
    "Control" =
      c(n_bw0_below, n_bw1_below, n_bw2_below, n_bw3_below, n_bw4_below),
    "Treatment" =
      c(n_bw0_above, n_bw1_above, n_bw2_above, n_bw3_above, n_bw4_above)
  )

tbl_over.years[, Total := Control + Treatment]
hux.tbl_over.years <- as_hux(tbl_over.years)
number_format(hux.tbl_over.years)[2:6, 2:4] <- fmt_pretty()
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
list_results_monthly <-
  list(
    results_monthly_no.controls_bw0,results_monthly_no.controls_bw1,
    results_monthly_no.controls_bw2, results_monthly_no.controls_bw3,
    results_monthly_no.controls_bw4,
    results_monthly_controls_bw0, results_monthly_controls_bw1,
    results_monthly_controls_bw2, results_monthly_controls_bw3,
    results_monthly_controls_bw4,
    results_monthly_controls.wFEs_bw0, results_monthly_controls.wFEs_bw1,
    results_monthly_controls.wFEs_bw2, results_monthly_controls.wFEs_bw3,
    results_monthly_controls.wFEs_bw4
  )

list_se_monthly <-
list(
  robust.se_monthly_no.controls_bw0, robust.se_monthly_no.controls_bw1,
  robust.se_monthly_no.controls_bw2, robust.se_monthly_no.controls_bw3,
  robust.se_monthly_no.controls_bw4,
  robust.se_monthly_controls_bw0, robust.se_monthly_controls_bw1,
  robust.se_monthly_controls_bw2, robust.se_monthly_controls_bw3,
  robust.se_monthly_controls_bw4,
  robust.se_monthly_controls.wFEs_bw0, robust.se_monthly_controls.wFEs_bw1,
  robust.se_monthly_controls.wFEs_bw2, robust.se_monthly_controls.wFEs_bw3,
  robust.se_monthly_controls.wFEs_bw4
)

vct_covariate.labels_monthly <-
  c(
    "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
    "1[Treated]",
    "CDDs",
    "HDDs",
    "Constant"
  )

list_add.lines <-
  list(
    c(
      "Bandwidth",
      rep(
        c(
          "N/A",
          paste0(NORMALIZED.QTY_UPPER_1, "%"),
          paste0(NORMALIZED.QTY_UPPER_2, "%"),
          paste0(NORMALIZED.QTY_UPPER_3, "%"),
          paste0(NORMALIZED.QTY_UPPER_4, "%")
        ),
        times = 3
      )
    ),
    c(
      "FEs: Account-Premise IDs",
      c(rep("No", times = 10), rep("Yes", times = 5))
    )
  )

# # 1.3. For model with daily average consumption as the dependent variable
# #      with normalized monthly consumption as an independent variable
list_results_daily_monthly <-
  list(
    results_daily_monthly_no.controls_bw0,
    results_daily_monthly_no.controls_bw1,
    results_daily_monthly_no.controls_bw2,
    results_daily_monthly_no.controls_bw3,
    results_daily_monthly_no.controls_bw4,
    results_daily_monthly_controls_bw0, results_daily_monthly_controls_bw1,
    results_daily_monthly_controls_bw2, results_daily_monthly_controls_bw3,
    results_daily_monthly_controls_bw4,
    results_daily_monthly_controls.wFEs_bw0,
    results_daily_monthly_controls.wFEs_bw1,
    results_daily_monthly_controls.wFEs_bw2,
    results_daily_monthly_controls.wFEs_bw3,
    results_daily_monthly_controls.wFEs_bw4
  )

list_se_daily_monthly <-
  list(
    robust.se_daily_monthly_no.controls_bw0,
    robust.se_daily_monthly_no.controls_bw1,
    robust.se_daily_monthly_no.controls_bw2,
    robust.se_daily_monthly_no.controls_bw3,
    robust.se_daily_monthly_no.controls_bw4,
    robust.se_daily_monthly_controls_bw0, robust.se_daily_monthly_controls_bw1,
    robust.se_daily_monthly_controls_bw2, robust.se_daily_monthly_controls_bw3,
    robust.se_daily_monthly_controls_bw4,
    robust.se_daily_monthly_controls.wFEs_bw0,
    robust.se_daily_monthly_controls.wFEs_bw1,
    robust.se_daily_monthly_controls.wFEs_bw2,
    robust.se_daily_monthly_controls.wFEs_bw3,
    robust.se_daily_monthly_controls.wFEs_bw4
  )

vct_covariate.labels_daily_monthly <-
  c(
    "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
    "1[Treated]",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "Constant"
  )

# # 1.3. For model with daily average consumption as the dependent variable
# #      with normalized daily consumption as an independent variable
list_results_daily_daily <-
  list(
    results_daily_daily_no.controls_bw0,
    results_daily_daily_no.controls_bw1,
    results_daily_daily_no.controls_bw2,
    results_daily_daily_no.controls_bw3,
    results_daily_daily_no.controls_bw4,
    results_daily_daily_controls_bw0, results_daily_daily_controls_bw1,
    results_daily_daily_controls_bw2, results_daily_daily_controls_bw3,
    results_daily_daily_controls_bw4,
    results_daily_daily_controls.wFEs_bw0,
    results_daily_daily_controls.wFEs_bw1,
    results_daily_daily_controls.wFEs_bw2,
    results_daily_daily_controls.wFEs_bw3,
    results_daily_daily_controls.wFEs_bw4
  )

list_se_daily_daily <-
  list(
    robust.se_daily_daily_no.controls_bw0,
    robust.se_daily_daily_no.controls_bw1,
    robust.se_daily_daily_no.controls_bw2,
    robust.se_daily_daily_no.controls_bw3,
    robust.se_daily_daily_no.controls_bw4,
    robust.se_daily_daily_controls_bw0, robust.se_daily_daily_controls_bw1,
    robust.se_daily_daily_controls_bw2, robust.se_daily_daily_controls_bw3,
    robust.se_daily_daily_controls_bw4,
    robust.se_daily_daily_controls.wFEs_bw0,
    robust.se_daily_daily_controls.wFEs_bw1,
    robust.se_daily_daily_controls.wFEs_bw2,
    robust.se_daily_daily_controls.wFEs_bw3,
    robust.se_daily_daily_controls.wFEs_bw4
  )

vct_covariate.labels_daily_daily <-
  c(
    paste0(
      "Normalized Daily Average Consumption in Period 0 ",
      "relative to Daily Base Usage Qty (%)"
    ),
    "1[Treated]",
    "Daily Average CDDs",
    "Daily Average HDDs",
    "Constant"
  )


# # 2. Export tables
# # 2.1. Print tables
# # 2.1.1. Model with relative monthly consumption as the dependent variable
stargazer(
  list_results_monthly,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_monthly,
  dep.var.labels =
    "Consumption in Period 1 (kWh)",
  add.lines = list_add.lines,
  se = list_se_monthly,
  font.size = "tiny",
  omit = c("Constant"),
  omit.stat = c("ser", "f")
)
# # 2.1.2. Model with daily average consumption as the dependent variable
# #        and normalized monthly consumption as an independent variable
stargazer(
  list_results_daily_monthly,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_daily_monthly,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines,
  se = list_se_daily_monthly,
  font.size = "tiny",
  omit = c("Constant"),
  omit.stat = c("ser", "f")
)
# # 2.1.3. Model with daily average consumption as the dependent variable
# #        and normalized daily average consumption as an independent variable
stargazer(
  list_results_daily_daily,
  type = "text",
  title = "",
  covariate.labels = vct_covariate.labels_daily_daily,
  dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
  add.lines = list_add.lines,
  se = list_se_daily_daily,
  font.size = "tiny",
  omit = c("Constant"),
  omit.stat = c("ser", "f")
)

# # 2.2. Export table(s) in LaTex format
# # 2.2.1. Model with relative monthly consumption as the dependent variable
PATH_TO.SAVE_TABLE_MONTHLY <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Approach_Monthly-Consumption.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_monthly,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_monthly,
    dep.var.labels =
      "Consumption in Period 1 (kWh)",
    add.lines = list_add.lines,
    se = list_se_monthly,
    font.size = "small",
    omit = c("Constant"),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_MONTHLY
)
# # 2.2.2. Model with daily average consumption as the dependent variable
# #        and normalized monthly consumption as an independent variable
PATH_TO.SAVE_TABLE_DAILY.MONTHLY <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Approach_Daily-Consumption_Monthly-Independent.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_daily_monthly,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_daily_monthly,
    dep.var.labels =
      "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines,
    se = list_se_daily_monthly,
    font.size = "small",
    omit = c("Constant"),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_DAILY.MONTHLY
)
# # 2.2.3. Model with daily average consumption as the dependent variable
# #        and normalized daily average consumption as an independent variable
PATH_TO.SAVE_TABLE_DAILY.DAILY <-
  paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_RD-Approach_Daily-Consumption_Daily-Independent.tex",
    sep = "/"
  )
capture.output(
  stargazer(
    list_results_daily_daily,
    type = "latex",
    title = "",
    covariate.labels = vct_covariate.labels_daily_daily,
    dep.var.labels =
      "Daily Average Consumption in Period 1 (kWh/Day)",
    add.lines = list_add.lines,
    se = list_se_daily_daily,
    font.size = "small",
    omit = c("Constant"),
    omit.stat = c("ser", "f")
  ),
  file = PATH_TO.SAVE_TABLE_DAILY.DAILY
)

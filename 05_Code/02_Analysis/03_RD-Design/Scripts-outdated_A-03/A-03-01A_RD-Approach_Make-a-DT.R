# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02A
# #
# > Purpose of the script(s)
# # : Generate DTs for descriptive analysis.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
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
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_BILLING <- "SMUD/Billing-Data"
FILE_TO.LOAD_BILLING <- "SMUD_Billing-Data_Extended_With-Degree-Days.parquet"
PATH_TO.LOAD_BILLING <- paste(
  PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_BILLING, FILE_TO.LOAD_BILLING, sep = "/"
)

# # 2. Path at which the DT created will be saved
DIR_TO.SAVE_RD <- "01_RD-Design"
FILE_TO.SAVE_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.SAVE_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, FILE_TO.SAVE_RD, sep = '/')


# ------- Define parameter(s) -------
# # 1. For constructing a sample
# # 1.1. With respect to billing periods
# # 1.1.1. To include observations for households that have enought length of
# #        billing history
N_PERIODS_LOWER <- 12 * 2
# # 1.1.2. To use observations that are serially well connected
DAYS.BETWEEN.PERIODS_UPPER <- 7 * 2
# # 1.1.3. To drop observations that are not standard billing periods in either
# #        period
STANDARD.BILLING.PERIOD_UPPER <- 34
STANDARD.BILLING.PERIOD_LOWER <- 27

# # 1.2. With respect to the distribution of observation around the threshold
# # 1.2.1. To use observations that are balanced around the threshold
SHARE_LOWER <- 30


# ------- Define function(s) -------
# # 1. Shift columns
shift_cols <-
  function (dt, cols_to.shift, suffix_to.col.names, n_for.shift, cols_by) {
  tmp_cols <- paste(cols_to.shift, suffix_to.col.names, sep = "_")
  dt[
     ,
    (tmp_cols) := lapply(.SD, shift, n = n_for.shift), .SDcols = cols_to.shift,
    by = cols_by
  ]
}


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
billing <-
  pq.to.dt(
    PATH_TO.LOAD_BILLING,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )
gc(reset = TRUE, full = TRUE)

# # 2. Check primary keys of the DTs
stopifnot(
  billing[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Make a DT for running regressions
# --------------------------------------------------
# ------- Add columns to SMUD billing data -------
# # 1. Add data fields regarding IDs
# # 1.1. Add columns showing Account-Premise level IDs
cols <- c("id_account", "id_premise")
billing <-
  billing[
    billing[, .N, by = cols][, N := NULL][, ids := seq_len(.N)], on = cols
  ]
billing[, ids_in.factor := factor(ids)]

# # 1.2. Add a column indicating the number of periods for each Account-Premise
# #      ID
billing[, n_periods_total := .N, by = .(ids)]


# # 2. Add data fields regarding billing periods
# # 2.1. Add a column showing year-month of billing periods
billing[
  ,
  billing.ym_mid :=
    (
      period_from +
      floor(as.numeric(period_to - period_from, units = "days") / 2)
    ) %>%
    zoo::as.yearmon(.)
]
billing[, billing.ym_mid_in.factor := factor(billing.ym_mid)]

# # 2.2. Add columns showing year and month of billing periods, respectively
billing[, billing.year_mid := year(billing.ym_mid)]
billing[, billing.month_mid := month(billing.ym_mid)]

# # 2.3. Add a data field showing the length of days between periods
billing[
  ,
  days_between.periods := as.numeric(period_from - shift(period_to)),
  by = .(ids)
]

# # 3. Add data fields regarding electricity consumption
# # 3.1. Add columns showing normalized consumption
billing[, kwh_total_in.percent_normalize_t1 := kwh_total_in.percent_t1 - 100]
billing[, kwh_total_in.percent_normalize_t2 := kwh_total_in.percent_t2 - 100]

# # 3.2. Add a column showing daily average consumption
billing[, kwh_daily.avg := kwh_total / period_len]


# ------- Add Indicator Variables to SMUD billing data -------
# # 1. Add an indicator variable showing whether a pair of Account and
# #    Premise IDs experiences any sign changes in normalized monthly
# #    consumption in Period 0
billing[
  !is.na(kwh_total_in.percent_t1),
  tmp.col_min := min(kwh_total_in.percent_normalize_t1, na.rm = TRUE),
  by = .(ids)
]
billing[
  !is.na(kwh_total_in.percent_t1),
  tmp.col_max := max(kwh_total_in.percent_normalize_t1, na.rm = TRUE),
  by = .(ids)
]
billing[tmp.col_max < 0 | 0 < tmp.col_min, is_no.sign.change := TRUE]
billing[tmp.col_min < 0 & 0 < tmp.col_max, is_no.sign.change := FALSE]


# # 2. Add an indicator variable showing treatment status
# # 2.1. For Period 1
# # 2.1.1.
billing[kwh_total_in.percent_normalize_t1 <= 0, is_treated_t1 := FALSE]
billing[kwh_total_in.percent_normalize_t1 > 0, is_treated_t1 := TRUE]
# # 2.1.2.
billing[kwh_total_in.percent_normalize_t2 <= 0, is_treated_t2 := FALSE]
billing[kwh_total_in.percent_normalize_t2 > 0, is_treated_t2 := TRUE]


# # 3. Add an indicator variable showing whether an observation is for the
# #    Account-Premise IDs that have more than or equal to a centain number
# #    of periods
billing[n_periods_total >= N_PERIODS_LOWER, is_over.years := TRUE]
billing[n_periods_total <  N_PERIODS_LOWER, is_over.years := FALSE]


# # 4. Add an indicator variable showing whether an observation is a successive
# #    billing period or not
billing[
  days_between.periods <= DAYS.BETWEEN.PERIODS_UPPER,
  is_successive.period := TRUE
]
billing[
  days_between.periods > DAYS.BETWEEN.PERIODS_UPPER,
  is_successive.period := FALSE
]


# # 5. Add data fields showing basic information about billing data in
# #    other periods
# # 5.1. Data fields related to Period 0
cols_to.shift <- c(
  "rate_code_normalize",
  #"period_len",
  "season_before",
  "season_after",
  #"billing.ym_mid",
  "billing.year_mid",
  "billing.month_mid",
  #"tier_1_qty_prorate",
  #"tier_1_qty_upto_in_kwh_before",
  #"tier_1_qty_upto_in_kwh_after",
  #"kwh_total",
  "kwh_total_in.percent_normalize_t1",
  "kwh_total_in.percent_normalize_t2",
  "kwh_daily.avg",
  "is_treated_t1",
  "is_treated_t2"
)
shift_cols(billing, cols_to.shift, "period0", 1, c("ids"))
shift_cols(billing, c("period_len"), "period0", 1, c("ids"))

billing[
  is.finite(kwh_total_in.percent_normalize_t1_period0),
  range_kwh_total_normalize_period0 :=
    cut(
      kwh_total_in.percent_normalize_t1_period0,
      breaks = seq(
        -100,
        max(kwh_total_in.percent_normalize_t1_period0, na.rm = TRUE), by = 1
      )
    )
]

# # 5.2. Data fields related to Period -n
# # 5.2.1. -n = -1
shift_cols(billing, cols_to.shift, "periodm1", 2, c("ids"))
# # 5.2.2. -n = -2
shift_cols(billing, cols_to.shift, "periodm2", 3, c("ids"))
# # 5.2.3. -n = -3
shift_cols(billing, cols_to.shift, "periodm3", 4, c("ids"))
# # 5.2.4. -n = -4
shift_cols(billing, cols_to.shift, "periodm4", 5, c("ids"))
# # 5.2.5. -n = -5
shift_cols(billing, cols_to.shift, "periodm5", 6, c("ids"))

# # 5.3. Data fields related to Period +n
# # 5.3.1. +n = +1
shift_cols(billing, cols_to.shift, "periodp1", -1, c("ids"))
# # 5.3.2. +n = +2
shift_cols(billing, cols_to.shift, "periodp2", -2, c("ids"))
# # 5.3.3. +n = +3
shift_cols(billing, cols_to.shift, "periodp3", -3, c("ids"))
# # 5.3.4. +n = +4
shift_cols(billing, cols_to.shift, "periodp4", -4, c("ids"))
# # 5.3.5. +n = +5
shift_cols(billing, cols_to.shift, "periodp5", -5, c("ids"))


# # 6. Add data fields regarding weather data
# # 6.1. Add columns showing HDDs/CDDs in Other Periods
# (NOT Applicable)

# # 6.2. Add columns indicating daily average HDDs/CDDs
billing[
  ,
  `:=` (
    cdd_daily.avg_period = cdd_period / period_len,
    hdd_daily.avg_period = hdd_period / period_len
  )
]

billing[
  ,
  `:=` (
    cdd_daily.avg_periodp1 = shift(cdd_daily.avg_period, n = -1),
    hdd_daily.avg_periodp1 = shift(hdd_daily.avg_period, n = -1),
    cdd_daily.avg_periodp2 = shift(cdd_daily.avg_period, n = -2),
    hdd_daily.avg_periodp2 = shift(hdd_daily.avg_period, n = -2),
    cdd_daily.avg_periodp3 = shift(cdd_daily.avg_period, n = -3),
    hdd_daily.avg_periodp3 = shift(hdd_daily.avg_period, n = -3),
    cdd_daily.avg_periodp4 = shift(cdd_daily.avg_period, n = -4),
    hdd_daily.avg_periodp4 = shift(hdd_daily.avg_period, n = -4),
    cdd_daily.avg_periodp5 = shift(cdd_daily.avg_period, n = -5),
    hdd_daily.avg_periodp5 = shift(hdd_daily.avg_period, n = -5)
  ),
  by = .(ids)
]


# ------- Generate a DT from SMUD billing data -------
# # 0. Drop temporary column(s)
cols_to.drop <- names(billing)[str_detect(names(billing), "^tmp.col_")]
billing[, (cols_to.drop) := NULL]

# # 1. Make a DT
dt_for.reg <-
  billing[
    is_in.sample == TRUE &
      #kwh_total_period0 > 0 &
      kwh_daily.avg_period0 > 0 &
      !is.na(kwh_total_in.percent_normalize_t1_period0) &
      !is.infinite(kwh_total_in.percent_normalize_t1_period0) &
      (STANDARD.BILLING.PERIOD_LOWER <= period_len_period0 &
        period_len_period0 <= STANDARD.BILLING.PERIOD_UPPER) &
      is_no.sign.change == FALSE &
      is_over.years == TRUE &
      is_successive.period == TRUE
  ]


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
  dt_for.reg,
  sink = PATH_TO.SAVE_RD,
  version = "1.0",
  compression = "snappy",
  use_dictionary = TRUE
)

# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-01B
# #
# > Purpose of the script(s)
# # : To make a DT including information about PV adoption and Greenergy
# #   Program participation.

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
DIR_TO.LOAD_GN <- "02_Greenergy-Program"
FILE_TO.LOAD_GN_BILLING <- "DT_Greenergy-Program_Billing-Data.RData"
PATH_TO.LOAD_GN_BILLING <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_BILLING, sep = "/")
# # 1.2. Data for Changes in Greenergy Program Participation and Residential
# #      Rate
FILE_TO.LOAD_GN_CHANGES <- "DT_Greenergy-Program_Changes.RData"
PATH_TO.LOAD_GN_CHANGES <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_CHANGES, sep = "/")

# # 2. Path at which the DT created will be saved
DIR_TO.SAVE <- DIR_TO.LOAD_GN
FILE_TO.SAVE <- "DT_Greenergy-Program_Changes_Extended.RData"
PATH_TO.SAVE <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE, FILE_TO.SAVE, sep = '/')


# ------- Define parameter(s) -------
# (NOT Applicable)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load Datasets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_BILLING)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_CHANGES)

# # 2. Check primary keys of the DT
stopifnot(dt_rate.change[, .N, by = .(ids)][N > 1, .N] == 0)
stopifnot(dt_suffix.change[, .N, by = .(ids)][N > 1, .N] == 0)


# --------------------------------------------------
# Create a DT incl. Info. about PV Adoption,
# Greenergy Program Participation, and Residential
# Rate
# --------------------------------------------------
# ------- Modify Two DTs about Greenergy Program Participation & Rate -------
# # 1. Modify DTs before merging them
# # 1.1. Add columns showing year-month info.
# # 1.1.1. For "dt_suffix.change"
cols_date <-
  names(dt_suffix.change)[str_detect(names(dt_suffix.change), "^first.date_")]
dt_suffix.change[
  ,
  (str_replace(cols_date, "^first.date", "year.month")) :=
    lapply(.SD, as.yearmon), .SDcols = cols_date
]
# # 1.1.2. For "dt_rate.change"
cols_date <-
  names(dt_rate.change)[str_detect(names(dt_rate.change), "^first.date_")]
dt_rate.change[
  ,
  (str_replace(cols_date, "^first.date", "year.month")) :=
    lapply(.SD, as.yearmon), .SDcols = cols_date
]


# ------- Make temporary DTs -------
# # 1. Make a temporary DT containing info. about PV Adoption
tmp_dt_pv.adoption_date <- dt_billing[
  , .N, by = .(ids, is_pv.adoption, first.date_pv.adoption_by.ids)
][
  , N := NULL
]
tmp_dt_pv.adoption_date[
  , year.month_pv.adoption_by.ids := as.yearmon(first.date_pv.adoption_by.ids)
]


# # 2. Make temporary DTs showing, for each "ids", the number of billng
# #    cycles
# # 2.1. W.R.T. suffix changes
# # 2.1.1. Add a temporary column
dt_billing[
  ,
  tmp_first.date_suffix.change :=
    max(first.date_suffix.change_by.ids.and.suffix, na.rm = TRUE),
  by = .(ids)
]
dt_billing[
  period_from <= tmp_first.date_suffix.change,
  tmp_suffix_before := TRUE
]
dt_billing[is.na(tmp_suffix_before), tmp_suffix_before := FALSE]
# # 2.1.2. Create a temporary DT
tmp_dt_suffix.change <- dcast(
  data = dt_billing[, .N, by = .(ids, tmp_suffix_before)],
  formula = ids ~ tmp_suffix_before
)
names_old <- c("FALSE", "TRUE")
names_new <- c(
  "n_billing.cycle_suffix.change_after", "n_billing.cycle_suffix.change_before"
)
setnames(tmp_dt_suffix.change, names_old, names_new)

# # 2.2. W.R.T. rate changes
# # 2.2.1. Add a temporary column
dt_billing[
  ,
  tmp_first.date_rate.change :=
    max(first.date_rate.change_by.ids.and.rate, na.rm = TRUE),
  by = .(ids)
]
dt_billing[
  period_from <= tmp_first.date_rate.change,
  tmp_rate_before := TRUE
]
dt_billing[is.na(tmp_rate_before), tmp_rate_before := FALSE]
# # 2.2.2. Create a temporary DT
tmp_dt_rate.change <- dcast(
  data = dt_billing[, .N, by = .(ids, tmp_rate_before)],
  formula = ids ~ tmp_rate_before
)
names_old <- c("FALSE", "TRUE")
names_new <- c(
  "n_billing.cycle_rate.change_after", "n_billing.cycle_rate.change_before"
)
setnames(tmp_dt_rate.change, names_old, names_new)

# # 2.3. W.R.T. PV adoption
# # 2.3.1. Add a temporary column
dt_billing[
  period_from <= first.date_pv.adoption_by.ids,
  tmp_pv.adoption_before := TRUE
]
dt_billing[is.na(tmp_pv.adoption_before), tmp_pv.adoption_before := FALSE]
# # 2.3.2. Create a temporary DT
tmp_dt_pv.adoption <- dcast(
  data = dt_billing[, .N, by = .(ids, tmp_pv.adoption_before)],
  formula = ids ~ tmp_pv.adoption_before
)
names_old <- c("FALSE", "TRUE")
names_new <- c(
  "n_billing.cycle_pv.adoption_after", "n_billing.cycle_pv.adoption_before"
)
setnames(tmp_dt_pv.adoption, names_old, names_new)


# ------- Create a DT by merging DTs -------
# # 1. Merge DTs
tmp_dt_merge.1 <- dt_suffix.change[dt_rate.change, on = .(ids)]
tmp_dt_merge.2 <- tmp_dt_merge.1[tmp_dt_pv.adoption, on = .(ids)]
tmp_dt_merge.3 <- tmp_dt_merge.2[tmp_dt_pv.adoption_date, on = .(ids)]
tmp_dt_merge.4 <- tmp_dt_merge.3[tmp_dt_rate.change, on = .(ids)]
dt_pv.and.greenergy <- tmp_dt_merge.4[tmp_dt_suffix.change, on = .(ids)]


# # 2. Modify the DT created by merging temporary DTs
# # 2.1. Change columns' order
cols_order <- c(
  "ids",
  "is_pv.adoption",
    "first.date_pv.adoption_by.ids", "year.month_pv.adoption_by.ids",
    "n_billing.cycle_pv.adoption_before",
    "n_billing.cycle_pv.adoption_after",
  "category_greenergy", "n_changes_suffix",
    "initial_suffix_greenergy",
      "first.date_initial_suffix_greenergy",
      "year.month_initial_suffix_greenergy",
    "lag1_suffix_greenergy",
      "first.date_lag1_suffix_greenergy", "year.month_lag1_suffix_greenergy",
    "lag2_suffix_greenergy",
      "first.date_lag2_suffix_greenergy", "year.month_lag2_suffix_greenergy",
    "lag3_suffix_greenergy",
      "first.date_lag3_suffix_greenergy", "year.month_lag3_suffix_greenergy",
    "final_suffix_greenergy",
      "first.date_final_suffix_greenergy", "year.month_final_suffix_greenergy",
    "n_billing.cycle_suffix.change_before",
    "n_billing.cycle_suffix.change_after",
  "category_rate.change", "n_changes_rate",
    "initial_rate.code",
      "first.date_initial_rate.code", "year.month_initial_rate.code",
    "lag1_rate.code",
      "first.date_lag1_rate.code", "year.month_lag1_rate.code",
    "lag2_rate.code",
      "first.date_lag2_rate.code", "year.month_lag2_rate.code",
    "final_rate.code",
      "first.date_final_rate.code", "year.month_final_rate.code",
    "n_billing.cycle_rate.change_before",
    "n_billing.cycle_rate.change_after"
)
setcolorder(dt_pv.and.greenergy, cols_order)


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in .RData format -------
save(dt_pv.and.greenergy, file = PATH_TO.SAVE)

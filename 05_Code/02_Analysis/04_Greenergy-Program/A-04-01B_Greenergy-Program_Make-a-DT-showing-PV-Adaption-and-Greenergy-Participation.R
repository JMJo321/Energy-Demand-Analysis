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
FILE_TO.LOAD_GN <- "DT_Greenergy-Program.RData"
PATH_TO.LOAD_GN <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN, sep = "/")

# # 2. Path at which the DT created will be saved
DIR_TO.SAVE <- DIR_TO.LOAD_GN
FILE_TO.SAVE <- "DT_Greenergy-Program_With-PV-Adoption.RData"
PATH_TO.SAVE <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE, FILE_TO.SAVE, sep = '/')


# ------- Define parameter(s) -------
# (NOT Applicable)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Create a DT incl. Info. about PV Adoption and Greenergy Program Participation
# --------------------------------------------------
# ------- Make temporary DTs containing Greenergy-Program-related Info. -------
# # 1. Make a DT showing each "ids"' category of program participation
stopifnot(
  dt_billing[
    !is.na(category_greenergy), .N, by = .(ids, category_greenergy)
  ][
    , .N, by = .(ids)
  ][N > 1] == 0
)
# ## Note: Only consider non-na values for "category_greenergy".

tmp_dt_category <- dt_billing[
  !is.na(category_greenergy), .N, by = .(ids, category_greenergy)
][
  , N := NULL
]

# # 2. Make a DT showing, for each "ids", suffix code(s) & the number of billng
# #    cycles
tmp_dt_suffix_code <- dt_billing[
  !is.na(category_greenergy), .N, by = .(ids, suffix_greenergy)
][
  ,
  `:=` (
    suffix_greenergy_after = shift(suffix_greenergy, n = -1),
    n_billing.cycle_greenergy.suffix.change_after = shift(N, n = -1)
  ),
  by = .(ids)
][
  , tmp_col_sn := 1:.N, by = .(ids)
][
  tmp_col_sn == 1
][
  , tmp_col_sn := NULL
]

names_old <- c("N", "suffix_greenergy")
names_new <- c(
  "n_billing.cycle_greenergy.suffix.change_before", "suffix_greenergy_before"
)
setnames(tmp_dt_suffix_code, names_old, names_new)

# # 3. Make a DT showing, for each "ids", the date of suffix code change
tmp_dt_suffix_date <- dt_billing[
  !is.na(category_greenergy), .(ids, suffix_greenergy, period_from)
][
  ,
  tmp_col_suffix.change := min(period_from, na.rm = TRUE),
  by = .(ids, suffix_greenergy)
][
  , date_greenergy.suffix.change := max(tmp_col_suffix.change), by = .(ids)
][
  , .N, by = .(ids, date_greenergy.suffix.change)
][
  ,
  `:=` (
    year.month_greenergy.suffix.change = as.yearmon(
      date_greenergy.suffix.change
    ),
    N = NULL
  )
]


# ------- Make temporary DTs containing Pv-Adoption-related Info. -------
# # 1. Make a DT showing whether a household adopts PV system or not
tmp_dt_pv.install <- dt_billing[, .N, by = .(ids, is_pv.install)][, N := NULL]

# # 2. Make a DT showing, for each "ids", the date of PV system installation
tmp_dt_pv.install.date <- dt_billing[
  , .(ids, is_pv, period_from)
][
  ,
  tmp_col_pv.install.date := min(period_from, na.rm = TRUE),
  by = .(ids, is_pv)
][
  , date_pv.install := max(tmp_col_pv.install.date, na.rm = TRUE), by = .(ids)
][
  , .N, by = .(ids, date_pv.install)
][
  , N := NULL
]
tmp_dt_pv.install.date[, year.month_pv.install := as.yearmon(date_pv.install)]

# # 3. Make a DT showing the numbers of billing cycles base on the date of PV
# #    system installation
tmp_dt_pv.install_billing.cycle <- dt_billing[
  , .N, by = .(ids, is_pv)
][
  , n_billing.cycle_pv.install_after := shift(N, n = -1), by = .(ids)
][
  , tmp_col_sn := 1:.N, by = .(ids)
][
  tmp_col_sn == 1
][
  , `:=` (is_pv = NULL, tmp_col_sn = NULL)
]
setnames(
  tmp_dt_pv.install_billing.cycle, "N", "n_billing.cycle_pv.install_before"
)


# ------- Create a DT by merging temporary DTs -------
# # 1. Merge temporary DTs
tmp_dt_merge.1 <- tmp_dt_pv.install.date[tmp_dt_pv.install, on = .(ids)]
tmp_dt_merge.2 <- tmp_dt_pv.install_billing.cycle[tmp_dt_merge.1, on = .(ids)]
tmp_dt_merge.3 <- tmp_dt_category[tmp_dt_merge.2, on = .(ids)]
tmp_dt_merge.4 <- tmp_dt_suffix_code[tmp_dt_merge.3, on = .(ids)]
dt_pv.and.greenergy <- tmp_dt_suffix_date[tmp_dt_merge.4, on = .(ids)]


# # 2. Modify the DT created by merging temporary DTs
# # 2.1. Change values of data fields
dt_pv.and.greenergy[
  is_pv.install == FALSE,
  `:=` (date_pv.install = NA, year.month_pv.install = NA)
]
# ## Note: When "is_pv.install == FALSE", the values of "date_pv.install" and
# ## "year.month_pv.install" are meaningless.

dt_pv.and.greenergy[
  category_greenergy %in% c("Greenergy", "Non-Greenergy", NA),
  `:=` (
    date_greenergy.suffix.change = NA,
    year.month_greenergy.suffix.change = NA
  )
]
# ## Note: To make date- and year-month-realted date fields mean the change in
# ## status.

# # 2.2. Change columns' order
cols_order <- c(
  "ids",
  "is_pv.install", "date_pv.install", "year.month_pv.install",
    "n_billing.cycle_pv.install_before", "n_billing.cycle_pv.install_after",
  "category_greenergy", "suffix_greenergy_before", "suffix_greenergy_after",
    "date_greenergy.suffix.change", "year.month_greenergy.suffix.change",
    "n_billing.cycle_greenergy.suffix.change_before",
    "n_billing.cycle_greenergy.suffix.change_after"
)
setcolorder(dt_pv.and.greenergy, cols_order)


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in .RData format -------
save(dt_pv.and.greenergy, file = PATH_TO.SAVE)

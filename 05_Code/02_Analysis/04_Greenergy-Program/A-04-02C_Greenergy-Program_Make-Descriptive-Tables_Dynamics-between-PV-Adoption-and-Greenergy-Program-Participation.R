# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-02C
# #
# > Purpose of the script(s)
# # : To make tables related to PV adoption and Greenergy Program participation.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
library(huxtable)
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
DIR_TO.LOAD_GN <- "02_Greenergy-Program"
FILE_TO.LOAD_GN_BILLING <- "DT_Greenergy-Program_Billing-Data.RData"
PATH_TO.LOAD_GN_BILLING <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_BILLING, sep = "/")

# # 1.2. PV-Adoption and Greenergy-Program-Participation Data
FILE_TO.LOAD_GN_CHANGES <- "DT_Greenergy-Program_Changes_Extended.RData"
PATH_TO.LOAD_GN_CHANGES <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_CHANGES, sep = "/")

# # 1.3.
FILE_TO.LOAD_GN_CHANGES_DERIVED <- "DT_Greenergy-Program_Changes_Derived.RData"
PATH_TO.LOAD_GN_CHANGES_DERIVED <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_CHANGES_DERIVED, sep = "/"
)


# ------- Define parameter(s) -------
# # 1. The First Four Year-Month in 2010
year.month_early2010 <-
  c("Jan 2010", "Feb 2010", "Mar 2010", "Apr 2010") %>% as.yearmon(.)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Datasets
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


# ------- Load SMUD Data w.r.t. Changes in Greenergy Program, etc. -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_CHANGES)
load(file = PATH_TO.LOAD_GN_CHANGES_DERIVED)

# 2. Check primary keys of the DT
stopifnot(
  dt_greenergy.participation[
    , .N, by = .(year.month_final_suffix_greenergy, category_greenergy)
  ][N > 1, .N] == 0
)
stopifnot(
  dt_pv.adoption[, .N, by = .(year.month_pv.adoption_by.ids, category_greenergy)
  ][N > 1, .N] == 0
)
stopifnot(dt_pv.and.greenergy[, .N, by = .(ids)][N > 1, .N] == 0)
stopifnot(
  dt_rate.change[
    , .N, by = .(year.month_final_rate.code, category_rate.change)
  ][N > 1, .N] == 0
)
stopifnot(dt_suffix.change[, .N, by = .(ids)][N > 1, .N] == 0)
stopifnot(
  dt_suffix.change_to.plot[
    ,
    .N,
    by = .(
      category_greenergy, initial_suffix_greenergy, final_suffix_greenergy,
      mean.decile_by.ids
    )
  ][N > 1, .N] == 0
)


# --------------------------------------------------
# Create DTs
# --------------------------------------------------
# ------- DT incl. numbers of households partipated in Greenergy Program initially -------
# # 1. For entire time range
# ## Note: "Non-Greenergy to Greenergy" cases are excluded.

# # 1.1. Make a temporary DT
tmp_dt_entire <- dt_pv.and.greenergy[
  , .N, keyby = .(initial_suffix_greenergy, final_suffix_greenergy)
]

# # 1.2. Add a column showing the category based on both initial and final
# #      Greenergy Program options
tmp_dt_entire[
  is.na(initial_suffix_greenergy) & is.na(final_suffix_greenergy),
  category_change := "Always Non-Greenergy"
]
tmp_dt_entire[
  is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy),
  category_change := "Opt In Greenergy"
]
tmp_dt_entire[
  !is.na(initial_suffix_greenergy) & is.na(final_suffix_greenergy),
  category_change := "Opt Out of Greenergy"
]
tmp_dt_entire[
  !is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy) &
    initial_suffix_greenergy == final_suffix_greenergy,
  category_change := "Always Greenergy staying on the same option"
]
tmp_dt_entire[
  !is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy) &
    initial_suffix_greenergy != final_suffix_greenergy,
  category_change := "Always Greenergy choosing other option"
]

# # 1.3. Compute household numbers based on the category
tmp_dt_entire[, N_by.category := sum(N, na.rm = TRUE), by = .(category_change)]

# # 1.4. Compute ratio of each group constructed based on options and categories
N_initial.g <- tmp_dt_entire[
  !is.na(initial_suffix_greenergy), sum(N, na.rm = TRUE)
]
tmp_dt_entire[
  !is.na(initial_suffix_greenergy), ratio_initial.g := N / N_initial.g
]

# # 1.5. Create a DT from the temporary DT
dt_entire_initial.g <- tmp_dt_entire[
  !is.na(initial_suffix_greenergy),
  lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N", "ratio_initial.g"),
  keyby = .(initial_suffix_greenergy, category_change)
]
dt_entire_initial.g[
  ,
  `:=` (
    N_initial.g_by.suffix = sum(N, na.rm = TRUE),
    ratio_initial.g_by.suffix = sum(ratio_initial.g, na.rm = TRUE)
  ),
  by = .(initial_suffix_greenergy)
]


# # 2. Only for the first four months in 2010
# ## Note: The code utilized to generate a DT above is re-used.
tmp_dt_early2010 <- dt_pv.and.greenergy[
  year.month_final_suffix_greenergy %in% year.month_early2010,
  .N, keyby = .(initial_suffix_greenergy, final_suffix_greenergy)
]

tmp_dt_early2010[
  is.na(initial_suffix_greenergy) & is.na(final_suffix_greenergy),
  category_change := "Always Non-Greenergy"
]
tmp_dt_early2010[
  is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy),
  category_change := "Opt In Greenergy"
]
tmp_dt_early2010[
  !is.na(initial_suffix_greenergy) & is.na(final_suffix_greenergy),
  category_change := "Opt Out of Greenergy"
]
tmp_dt_early2010[
  !is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy) &
    initial_suffix_greenergy == final_suffix_greenergy,
  category_change := "Always Greenergy staying on the same option"
]
tmp_dt_early2010[
  !is.na(initial_suffix_greenergy) & !is.na(final_suffix_greenergy) &
    initial_suffix_greenergy != final_suffix_greenergy,
  category_change := "Always Greenergy choosing other option"
]

tmp_dt_early2010[
  , N_by.category := sum(N, na.rm = TRUE), by = .(category_change)
]

N_initial.g_early2010 <- tmp_dt_early2010[
  !is.na(initial_suffix_greenergy), sum(N, na.rm = TRUE)
]
tmp_dt_early2010[
  !is.na(initial_suffix_greenergy),
  ratio_initial.g := N / N_initial.g_early2010
]

dt_early2010_initial.g <- tmp_dt_early2010[
  !is.na(initial_suffix_greenergy),
  lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N", "ratio_initial.g"),
  keyby = .(initial_suffix_greenergy, category_change)
]
dt_early2010_initial.g[
  ,
  `:=` (
    N_initial.g_by.suffix = sum(N, na.rm = TRUE),
    ratio_initial.g_by.suffix = sum(ratio_initial.g, na.rm = TRUE)
  ),
  by = .(initial_suffix_greenergy)
]


# --------------------------------------------------
# Create Hux Tables
# --------------------------------------------------
# ------- Make Hux Tables from DTs created above  -------
# # 1. For "dt_entire_initial.g"
# # 1.1. Create a hux table
huxtable_entire_initial.g <- as_hux(dt_entire_initial.g)

# # 1.2. Modify the hux table
# # 1.2.1. Convert formats of numbers
number_format(huxtable_entire_initial.g)[, c(3, 5)] <- fmt_pretty()
number_format(huxtable_entire_initial.g)[, c(4, 6)] <- fmt_percent()
# # 1.2.2. Rename the first row
contents(huxtable_entire_initial.g)[1, ] <- c(
  "Initial Option", "Category", "N", "(%)", "N by Category", "(%)"
)
# # 1.2.3. Change columns' widths
col_width(huxtable_entire_initial.g) <- c(2, 7, 2, 2, 2, 2)


# # 2. For "dt_early2010_initial.g"
# # 2.1. Create a hux table
huxtable_early2010_initial.g <- as_hux(dt_early2010_initial.g)

# # 2.2. Modify the hux table
# # 2.2.1. Convert formats of numbers
number_format(huxtable_early2010_initial.g)[, c(3, 5)] <- fmt_pretty()
number_format(huxtable_early2010_initial.g)[, c(4, 6)] <- fmt_percent()
# # 2.2.2. Rename the first row
contents(huxtable_early2010_initial.g)[1, ] <- c(
  "Initial Option", "Category", "N", "(%)", "N by Category", "(%)"
)
# # 2.2.3. Change columns' widths
col_width(huxtable_early2010_initial.g) <- c(2, 7, 2, 2, 2, 2)

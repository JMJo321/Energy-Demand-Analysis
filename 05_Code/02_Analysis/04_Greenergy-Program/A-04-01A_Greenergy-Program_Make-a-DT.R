# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-01A
# #
# > Purpose of the script(s)
# # : To make a DT based on the DT for RD Design.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
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
# # 1.1. SMUD Billing Data for RD Design
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Path at which the DT created will be saved
DIR_TO.SAVE <- "02_Greenergy-Program"
FILE_TO.SAVE <- "DT_Greenergy-Program.RData"
PATH_TO.SAVE <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE, FILE_TO.SAVE, sep = '/')


# ------- Define parameter(s) -------
# (NOT Applicable)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a parquet file
dt_billing <- pq.to.dt(
  PATH_TO.LOAD_RD,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)
gc(reset = TRUE, full = TRUE)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Add columns to the DT
# --------------------------------------------------
# ------- Add a column showing category of each "ids" w.r.t. Participation to Greenergy Program -------
# # 1. Add column(s) to the DT
# # 1.1. Add a column showing suffixes for Greenergy Program
dt_billing[
  rate_code %like% "[0-9]+$",
  suffix_greenergy := str_extract(rate_code, "[0-9]+$") %>% as.integer(.)
]


# # 2. Make objects
# # 2.1. List of "ids"s in which the normalized rate code changed
ids_change.in.nrc <- dt_billing[
  , .N, by = .(ids, rate_code_normalize)
][
  , .N, by = .(ids)
][
  N > 1
]$ids

length(ids_change.in.nrc) / dt_billing[, .N, by = .(ids)][, .N] * 100
# ## Note: Ignore those households because they are very minor.

# # 2.2. List of "ids"s in which the suffix for Greenergy Program changed
# # 2.2.1. For any number of changes
ids_change.in.suffix <- dt_billing[
  !ids %in% ids_change.in.nrc, .N, by = .(ids, suffix_greenergy)
][
  , .N, by = .(ids)
][
  N > 1
]$ids
# # 2.2.2. For only one change
ids_change.in.suffix_only.once <- dt_billing[
  !ids %in% ids_change.in.nrc, .N, by = .(ids, suffix_greenergy)
][
  , .N, by = .(ids)
][
  N == 2
]$ids

length(ids_change.in.suffix_only.once) / length(ids_change.in.suffix) * 100
# ## Note: As shown from this, ignoring more than one changes in the suffix
# ## for Greenergy Program within each "ids" seems not to cause any problems.

# # 2.3. List of "ids"s in which the suffix for Greenergy Program did NOT change
ids_no.change.in.suffix <- dt_billing[
  !ids %in% ids_change.in.nrc, .N, by = .(ids, suffix_greenergy)
][
  , .N, by = .(ids)
][
  N == 1
]$ids
# # 2.3.1. For all time non-participants
ids_no.change.in.suffix_non.greenergy <- dt_billing[
  ids %in% ids_no.change.in.suffix
][
  is.na(suffix_greenergy), .N, by = .(ids)
]$ids
# # 2.3.2. For all time participants
ids_no.change.in.suffix_greenergy <- dt_billing[
  ids %in% ids_no.change.in.suffix
][
  !is.na(suffix_greenergy), .N, by = .(ids)
]$ids

# # 2.4. DT that will be used to make lists of "ids"s, which categorize
# # households
tmp_dt <- dt_billing[
  !ids %in% ids_change.in.nrc, .N, by = .(ids, suffix_greenergy)
][
  , N := NULL
]

dt_suffix.change <- tmp_dt[
  tmp_dt[ids %in% ids_change.in.suffix_only.once],
  on = .(ids, suffix_greenergy)
][
  ,
  `:=` (
    lag_suffix_greenergy = shift(suffix_greenergy, -1),
    n = 1:.N
  ),
  by = .(ids)
][
  n == 1
]
dt_suffix.change[, n := NULL]

# # 2.5. Create lists of "ids"s that categorize households
ids_change.in.suffix_ng.to.g <- dt_suffix.change[
  is.na(suffix_greenergy) & !is.na(lag_suffix_greenergy), .N, by = .(ids)
]$ids
ids_change.in.suffix_g.to.ng <- dt_suffix.change[
  !is.na(suffix_greenergy) & is.na(lag_suffix_greenergy), .N, by = .(ids)
]$ids
ids_change.in.suffix_g.to.g <- dt_suffix.change[
  !is.na(suffix_greenergy) & !is.na(lag_suffix_greenergy), .N, by = .(ids)
]$ids

# # 2.6. Conduct simple tests
stopifnot(
  ids_no.change.in.suffix_non.greenergy[
    ids_no.change.in.suffix_non.greenergy %in% ids_change.in.suffix_ng.to.g
  ] %>% length(.) == 0
)
stopifnot(
  ids_no.change.in.suffix_non.greenergy[
    ids_no.change.in.suffix_non.greenergy %in% ids_change.in.suffix_g.to.ng
  ] %>% length(.) == 0
)
stopifnot(
  ids_no.change.in.suffix_non.greenergy[
    ids_no.change.in.suffix_non.greenergy %in% ids_change.in.suffix_g.to.g
  ] %>% length(.) == 0
)

stopifnot(
  ids_no.change.in.suffix_greenergy[
    ids_no.change.in.suffix_greenergy %in% ids_change.in.suffix_ng.to.g
  ] %>% length(.) == 0
)
stopifnot(
  ids_no.change.in.suffix_greenergy[
    ids_no.change.in.suffix_greenergy %in% ids_change.in.suffix_g.to.ng
  ] %>% length(.) == 0
)
stopifnot(
  ids_no.change.in.suffix_greenergy[
    ids_no.change.in.suffix_greenergy %in% ids_change.in.suffix_g.to.g
  ] %>% length(.) == 0
)


# # 3. Add a column showing category of each "ids"
# # 3.1. Add a column
dt_billing[
  ids %in% ids_no.change.in.suffix_non.greenergy,
  category_greenergy := "Non-Greenergy"
]
dt_billing[
  ids %in% ids_no.change.in.suffix_greenergy,
  category_greenergy := "Greenergy"
]
dt_billing[
  ids %in% ids_change.in.suffix_ng.to.g,
  category_greenergy := "Non-Greenergy to Greenergy"
]
dt_billing[
  ids %in% ids_change.in.suffix_g.to.ng,
  category_greenergy := "Greenergy to Non-Greenergy"
]
dt_billing[
  ids %in% ids_change.in.suffix_g.to.g,
  category_greenergy := "Greenergy to Greenergy"
]

levels_greenergy <- c(
  "Non-Greenergy", "Non-Greenergy to Greenergy", "Greenergy to Non-Greenergy",
  "Greenergy to Greenergy", "Greenergy"
)
dt_billing[
  , category_greenergy := factor(category_greenergy, levels = levels_greenergy)
]
# # 3.2. Conduct simple tests
stopifnot(dt_billing[, .N, by = .(category_greenergy)][, .N] == 6)
stopifnot(
  dt_billing[
    , .N, by = .(ids, category_greenergy)
  ][
    , .N, by = .(ids)
  ][
    N > 1, .N
  ] == 0
)


# ------- Add a column showing category of each "ids" w.r.t. Installation of PV system -------
# # 1. Add a column showing category of each "ids"
# # 1.1. Conduct simple check before adding the column
dt_billing[, .N, by = .(ids, is_pv)][, .N, by = .(ids)][N > 1, .N]
# ## Note: This result shows there are households installing/dismantling
# ## PV systems additionally.

# # 1.2. Add a column
dt_billing[, is_pv.install := any(is_pv), by = .(ids)]

# # 1.3. Conduct a simple test
dt_billing[, .N, by = .(is_pv, is_pv.install)]


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
save(dt_billing, file = PATH_TO.SAVE)
# ## Note: Parquet file makes error when loading it.

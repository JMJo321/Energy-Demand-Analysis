# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, SMUD
# #
# > Script Number(s)
# # : B-01-03B
# #
# > Purpose of the script(s)
# # : Add columns showing Heating Degree Days (HDDs) and
# #   Cooling Degree Days (CDDs) of billing periods.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(parallel)
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
# # 1. Paths of data files that will be loaded
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_BILLING <- "SMUD/Billing-Data"
FILE_TO.LOAD_BILLING <- "SMUD_Billing-Data_Extended.parquet"
PATH_TO.LOAD_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_BILLING, FILE_TO.LOAD_BILLING, sep= "/"
  )
# # 1.2. NOAA Local Climatological Data
DIR_TO.LOAD_LCD <- "NOAA/LCD"
FILE_TO.LOAD_LCD <- "NOAA_Local-Climatological-Data.parquet"
PATH_TO.LOAD_LCD <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_LCD, FILE_TO.LOAD_LCD, sep = "/")

# # 2. Path at which a merged data will be saved
DIR_TO.SAVE_BILLING <- DIR_TO.LOAD_BILLING
FILE_TO.SAVE_BILLING <- "SMUD_Billing-Data_Extended_With-Degree-Days.parquet"
PATH_TO.SAVE_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.SAVE_BILLING, FILE_TO.SAVE_BILLING, sep = "/"
  )


# ------- Define parameter(s) -------
# # 1. For "mcmapply" function of "parallel" library
N_cores <- detectCores() / detectCores() * 6


# ------- Define function(s) -------
accumulate_degree.days <- function(col_to.aggregate, period_from, period_to) {
  degree.days <-
    lcd[
      period_from <= date & date <= period_to,
      sum(get(col_to.aggregate), na.rm = TRUE)
    ]
  return(degree.days)
}


# --------------------------------------------------
# Load Datasets required
# --------------------------------------------------
# ------- Load datasets required -------
# # 1. Load datasets
# # 1.1. SMUD Billing Data
billing <-
  pq.to.dt(
    PATH_TO.LOAD_BILLING,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )
# # 1.2. SMUD Residential Rate Schedules
lcd <-
  pq.to.dt(
    PATH_TO.LOAD_LCD,
    reg.ex_date = "(^date)|(^season_)",
    is_drop.index_cols = TRUE
  )
# # 1.3. Do garbage collection
gc(reset = TRUE, full = TRUE)

# # 2. Check primary keys of the DTs
stopifnot(
  billing[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)
stopifnot(lcd[, .N, by = .(date)][N > 1, .N] == 0)


# --------------------------------------------------
# Add columns showing HDDs and CDDs of billing periods
# --------------------------------------------------
# ------- Add columns showing HDDs and CDDs of billing periods -------
interval.length <- 10^7
interval_begin <- seq(1, interval.length * 5 + 1, by = interval.length)
interval_end <-
  c(seq(interval.length, billing[, .N], by = interval.length), billing[, .N])

for (idx in 1:length(interval_end)) {
  tmp_begin <- interval_begin[idx]
  tmp_end <- interval_end[idx]

  paste0("Computation begins: ", Sys.time()) %>% print(.)
  billing[
    tmp_begin:tmp_end,
    `:=` (
      cdd_period = mcmapply(
        accumulate_degree.days, "cdd_daily", period_from, period_to,
        mc.cores = N_cores
      ),
      hdd_period = mcmapply(
        accumulate_degree.days, "hdd_daily", period_from, period_to,
        mc.cores = N_cores
      )
    )
  ]
  paste0("Computation ends: ", Sys.time()) %>% print(.)

  paste0("Writing File begins: ", Sys.time()) %>% print(.)
  arrow::write_parquet(
    billing,
    sink = PATH_TO.SAVE_BILLING,
    version = "1.0",
    compression = "snappy",
    use_dictionary = TRUE
  )
  # ## Note: Version 2.0 is not supported as of September 6, 2020.
  paste0("Writing File ends: ", Sys.time()) %>% print(.)
  gc(reset = TRUE, full = TRUE)

  cat("Press [Enter] to continue...")
  readline()
}
# ## Note: This calculation takes about 112 minutes.

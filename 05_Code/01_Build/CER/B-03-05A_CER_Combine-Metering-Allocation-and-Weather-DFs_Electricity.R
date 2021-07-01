# < Description > *
# > Script Group Indicator Number and Name
# # : B-03, CER
# #
# > Script Number(s)
# # : B-03-05A
# #
# > Purpose of the script(s)
# # : Create a DT by merging metering and allocation datasets.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(lubridate)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Dataset(s) is(are) loaded
# # 1.1. For Metering Data
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_METERING_ELECTRICITY <- "CER_Metering_Electricity.RData"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.LOAD_CER_METERING_ELECTRICITY,
  sep = "/"
)
# # 1.2. For Allocation Data
FILE_TO.LOAD_CER_ALLOCATION_ELECTRICITY <- "CER_Allocation_Electricity.parquet"
PATH_TO.LOAD_CER_ALLOCATION_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Allocation",
  FILE_TO.LOAD_CER_ALLOCATION_ELECTRICITY,
  sep = "/"
)
# # 1.3. For Weather Data
DIR_TO.LOAD_WEATHER <- "Met-Eireann"
# # 1.3.1. Hourly-Level Data
FILE_TO.LOAD_WEATHER_HOURLY <- "Met-Eireann_Weather-Data_Hourly.parquet"
PATH_TO.LOAD_WEATHER_HOURLY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_WEATHER,
  FILE_TO.LOAD_WEATHER_HOURLY,
  sep = "/"
)
# # 1.3.2. Daily-Level Data
FILE_TO.LOAD_WEATHER_DAILY <- "Met-Eireann_Weather-Data_Daily.parquet"
PATH_TO.LOAD_WEATHER_DAILY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_WEATHER,
  FILE_TO.LOAD_WEATHER_DAILY,
  sep = "/"
)

# # 2. Path(s) to which output will be saved
FILE_TO.SAVE_CER_COMBINED_ELECTRICITY <-
  "CER_Extended-Metering_Electricity.RData"
PATH_TO.SAVE_CER_COMBINED_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.SAVE_CER_COMBINED_ELECTRICITY,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Treatement Begin Date
DATE_BEGIN.OF.TREATMENT <- as.Date("2010-01-01")

# # 2. Periods of Time-Of-Use Rates
TOU.PERIOD_NIGHT <- c(0L:7L, 23L)
TOU.PERIOD_DAY <- c(8L:16L, 19L:22L)
TOU.PERIOD_PEAK <- c(17L:18L)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a Combined DT
# ------------------------------------------------------------------------------
# ------- Combine Metering and Allocation Datasets -------
# # 1. Load Datasets
# # 1.1. Metering Dataset
load(PATH_TO.LOAD_CER_METERING_ELECTRICITY)

# # 1.2. Allocation Dataset
dt_allocation_e <- pq.to.dt(
  PATH_TO.LOAD_CER_ALLOCATION_ELECTRICITY,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)

# # 1.3. Weather Dataset
# # 1.3.1. Hourly-Level Dataset
dt_weather_hourly <-
  arrow::read_parquet(PATH_TO.LOAD_WEATHER_HOURLY) %>% setDT(.)
# # 1.3.2. Daily-Level Dataset
dt_weather_daily <-
  arrow::read_parquet(PATH_TO.LOAD_WEATHER_DAILY) %>% setDT(.)


# # 2. Merge the two datasets
# # 2.1. Merge Metering and Aloocation datasets
tmp_dt_merge_1 <- merge(
  x = dt_metering_hourly[!is.na(datetime)],
  # ## Note: Do NOT use observations with weird inverval
  y = dt_allocation_e,
  by = "id",
  all.x = TRUE
)

# # 2.2. Merge with weather datasets
# # 2.2.1. Merge with hourly-level weather dataset
tmp_dt_merge_2 <- merge(
  x = tmp_dt_merge_1,
  y = dt_weather_hourly[station %like% "Dublin", .(datetime, temp_c, temp_f)],
  by = "datetime",
  all.x = TRUE
)
# # 2.2.2. Merge with daily-level weather dataset
dt_metering_e <- merge(
  x = tmp_dt_merge_2,
  y = dt_weather_daily[
    station %like% "Dublin",
    .(date, maxtp_c, maxtp_f, mintp_c, mintp_f, gmin_c, gmin_f, soil_c, soil_f)
  ],
  by = "date",
  all.x = TRUE
)


# # 3. Modify the DT merged
# # 3.1. Add column(s)
# # 3.1.1. Add a column showing whether each household is treated or not
dt_metering_e[
  alloc_group == 1 & alloc_r_tariff != "E", is_treated_r := TRUE
]
dt_metering_e[
  alloc_group == 1 & alloc_r_tariff == "E", is_treated_r := FALSE
]
dt_metering_e[
  alloc_group == 2 & alloc_sme != "C", is_treated_sme := TRUE
]
dt_metering_e[
  alloc_group == 2 & alloc_sme == "C", is_treated_sme := FALSE
]
# ## Note:
# ## Observations that `alloc_group == 3` would have NA values.
# # 3.1.2. Add a column showing whether each date is in treated period or not
dt_metering_e[, is_treatment.period := date >= DATE_BEGIN.OF.TREATMENT]
# # 3.1.3. Add a column that shows periods of TOU rates
dt_metering_e[interval_hour %in% TOU.PERIOD_NIGHT, rate.period := "Night"]
dt_metering_e[interval_hour %in% TOU.PERIOD_DAY, rate.period := "Day"]
dt_metering_e[interval_hour %in% TOU.PERIOD_PEAK, rate.period := "Peak"]
dt_metering_e[
  ,
  rate.period := factor(rate.period, levels = c("Night", "Day", "Peak"))
]

# # 3.2. Sort Observations
keys <- c("id", "datetime")
setkeyv(dt_metering_e, keys)

# # 3.3. Reorder columns
cols_order <- c(
  "id", "alloc_group", "alloc_group_desc", "alloc_sme", "alloc_sme_desc",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "is_treated_r", "is_treated_sme", "is_treatment.period",
  "day", "date", "interval_hour", "rate.period", "datetime",
  "day.of.week", "is_weekend", "is_holiday",
  "kwh"
)
setcolorder(dt_metering_e, cols_order)


# ------- Save the combined DT in parquet format -------
save(dt_metering_e, file = PATH_TO.SAVE_CER_COMBINED_ELECTRICITY)

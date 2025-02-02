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
FILE_TO.LOAD_CER_METERING_ELECTRICITY <- "CER_Metering_Electricity.parquet"
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
  "CER_Extended-Metering_Electricity.parquet"
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
TOU.PERIOD_NIGHT_POST.AND.TRANSITION <- c(23L, 0L:2L)
TOU.PERIOD_NIGHT_STEADY <- 3L:4L
TOU.PERIOD_NIGHT_PRE.AND.TRANSITION <- 5L:7L
TOU.PERIOD_DAY_PRE.AND.STEADY <- 8L:14L
TOU.PERIOD_DAY_PRE.AND.TRANSITION <- 15L:16L
TOU.PERIOD_PEAK <- 17L:18L
TOU.PERIOD_DAY_POST.AND.STEADY <- 19L:21L
TOU.PERIOD_DAY_POST.AND.TRANSITION <- 22L


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a Combined DT
# ------------------------------------------------------------------------------
# ------- Combine Metering and Allocation Datasets -------
# # 1. Load Datasets
# # 1.1. Metering Dataset
dt_metering_elec_30min <-
  read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY) %>% setDT(.)

# # 1.2. Allocation Dataset
dt_allocation_elec <-
  read_parquet(PATH_TO.LOAD_CER_ALLOCATION_ELECTRICITY) %>% setDT(.)

# # 1.3. Weather Dataset
# # 1.3.1. Hourly-Level Dataset
dt_weather_hourly <- read_parquet(PATH_TO.LOAD_WEATHER_HOURLY) %>% setDT(.)
# # 1.3.2. Daily-Level Dataset
dt_weather_daily <- read_parquet(PATH_TO.LOAD_WEATHER_DAILY) %>% setDT(.)


# # 2. Merge the two datasets
# # 2.1. Merge Metering and Aloocation datasets
tmp_dt_merge_1 <- merge(
  x = dt_metering_elec_30min,
  y = dt_allocation_elec,
  by = "id",
  all.x = TRUE
)

# # 2.2. Merge with weather datasets
# # 2.2.1. Merge with hourly-level weather dataset
# # 2.2.1.1. Add columns, which are used when merging DTs, to
# #          `dt_weather_hourly`
dt_weather_hourly[
  ,
  `:=` (
    date = date(datetime),
    interval_hour = hour(datetime)
  )
]
# # 2.2.1.2. Create a temporary DT by merging DTs
tmp_dt_merge_2 <- merge(
  x = tmp_dt_merge_1,
  y = dt_weather_hourly[
    station %like% "Dublin",
    .(date, interval_hour, temp_c, temp_f)
  ],
  by = c("date", "interval_hour"),
  all.x = TRUE
)
# # 2.2.2. Merge with daily-level weather dataset
dt_metering_elec <- merge(
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
dt_metering_elec[
  alloc_group == 1 & alloc_r_tariff != "E", is_treated_r := TRUE
]
dt_metering_elec[
  alloc_group == 1 & alloc_r_tariff == "E", is_treated_r := FALSE
]
dt_metering_elec[
  alloc_group == 2 & alloc_sme != "C", is_treated_sme := TRUE
]
dt_metering_elec[
  alloc_group == 2 & alloc_sme == "C", is_treated_sme := FALSE
]
# ## Note:
# ## Observations that `alloc_group == 3` would have NA values.
# # 3.1.2. Add a column showing whether each date is in treated period or not
dt_metering_elec[, is_treatment.period := date >= DATE_BEGIN.OF.TREATMENT]
# # 3.1.3. Add columns that show periods of TOU rates
# # 3.1.3.1. For detailed rate periods
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_NIGHT_POST.AND.TRANSITION,
  `:=` (
    rate.period_detail = "Night: Post-Day Transition (23-2)",
    length_rate.period_detail = length(TOU.PERIOD_NIGHT_POST.AND.TRANSITION)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_NIGHT_STEADY,
  `:=` (
    rate.period_detail = "Night: Steady (3-4)",
    length_rate.period_detail = length(TOU.PERIOD_NIGHT_STEADY)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_NIGHT_PRE.AND.TRANSITION,
  `:=` (
    rate.period_detail = "Night: Pre-Day Transition (5-7)",
    length_rate.period_detail = length(TOU.PERIOD_NIGHT_PRE.AND.TRANSITION)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_DAY_PRE.AND.STEADY,
  `:=` (
    rate.period_detail = "Day: Pre-Peak Steady (8-14)",
    length_rate.period_detail = length(TOU.PERIOD_DAY_PRE.AND.STEADY)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_DAY_PRE.AND.TRANSITION,
  `:=` (
    rate.period_detail = "Day: Pre-Peak Transition (15-16)",
    length_rate.period_detail = length(TOU.PERIOD_DAY_PRE.AND.TRANSITION)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_PEAK,
  `:=` (
    rate.period_detail = "Peak (17-18)",
    length_rate.period_detail = length(TOU.PERIOD_PEAK)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_DAY_POST.AND.STEADY,
  `:=` (
    rate.period_detail = "Day: Post-Peak Steady (19-21)",
    length_rate.period_detail = length(TOU.PERIOD_DAY_POST.AND.STEADY)
  )
]
dt_metering_elec[
  interval_hour %in% TOU.PERIOD_DAY_POST.AND.TRANSITION,
  `:=` (
    rate.period_detail = "Day: Post-Peak Transition (22)",
    length_rate.period_detail = length(TOU.PERIOD_DAY_POST.AND.TRANSITION)
  )
]
dt_metering_elec[
  ,
  rate.period_detail := factor(
    rate.period_detail,
    levels = c(
      "Night: Post-Day Transition (23-2)",
      "Night: Steady (3-4)",
      "Night: Pre-Day Transition (5-7)",
      "Day: Pre-Peak Steady (8-14)",
      "Day: Pre-Peak Transition (15-16)",
      "Peak (17-18)",
      "Day: Post-Peak Steady (19-21)",
      "Day: Post-Peak Transition (22)"
    )
  )
]
# # 3.1.3.2. For rate period
dt_metering_elec[
  ,
  tmp_rate.period := str_extract(rate.period_detail, "(^Night)|(^Day)|(^Peak)")
]
dt_metering_elec[
  ,
  rate.period := factor(
    tmp_rate.period,
    levels = c("Night", "Day", "Peak")
  )
]
dt_metering_elec[rate.period == "Peak", length_rate.period := 2]
dt_metering_elec[rate.period == "Day", length_rate.period := 13]
dt_metering_elec[rate.period == "Night", length_rate.period := 9]
dt_metering_elec[, tmp_rate.period := NULL]

# # 3.2. Sort Observations
keys <- c("id", "datetime")
stopifnot(dt_metering_elec[, .N, by = keys][N > 1] == 0)
setkeyv(dt_metering_elec, keys)

# # 3.3. Reorder columns
cols_reorder <- c(
  "id", "alloc_group", "alloc_group_desc", "alloc_sme", "alloc_sme_desc",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "is_treated_r", "is_treated_sme", "is_treatment.period",
  "day", "date", "datetime", "interval_hour", "interval_30min",
  "rate.period", "length_rate.period",
  "rate.period_detail", "length_rate.period_detail",
  "day.of.week", "is_weekend", "is_holiday",
  "kwh"
)
setcolorder(dt_metering_elec, cols_reorder)


# ------- Save the combined DT in parquet format -------
write_parquet(
  dt_metering_elec,
  sink = PATH_TO.SAVE_CER_COMBINED_ELECTRICITY,
  compression = "snappy",
  use_dictionary = TRUE
)

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
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <-
    paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
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
FILE_TO.LOAD_WEATHER <- "Met-Eireann_Weather-Data_Hourly.parquet"
DIR_TO.LOAD_WEATHER <- "Met-Eireann"
PATH_TO.LOAD_WEATHER <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_WEATHER,
  FILE_TO.LOAD_WEATHER,
  sep = "/"
)

# # 2. Path(s) to which output will be saved
FILE_TO.SAVE_CER_COMBINED <- "CER_Extended-Metering_Electricity.parquet"
PATH_TO.SAVE_CER_COMBINED <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.SAVE_CER_COMBINED,
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a Combined DT
# ------------------------------------------------------------------------------
# ------- Combine Metering and Allocation Datasets -------
# # 1. Load Datasets
# # 1.1. Metering Dataset
dt_metering_e <- pq.to.dt(
  PATH_TO.LOAD_CER_METERING_ELECTRICITY,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)
dt_metering_e[
  !is.na(interval_hour),
  datetime := lubridate::ymd_h(
    paste(as.character(date), interval_hour, sep = " ")
  )
]
# ## Note:
# ## Add a column including datetime info, which will be used when merging
# ## datasets.
# ## There are observations that `interval_hour` >= 24, which are inconsistent
# ## with CER's data dictionary. I will not use those observations

# # 1.2. Allocation Dataset
dt_allocation_e <- pq.to.dt(
  PATH_TO.LOAD_CER_ALLOCATION_ELECTRICITY,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)

# # 1.3. Weather Dataset
dt_weather_hourly <- arrow::read_parquet(PATH_TO.LOAD_WEATHER) %>% setDT(.)


# # 2. Merge the two datasets
# # 2.1. Merge Metering and Aloocation datasets
cols_extract.from.allocation <- c(
  "id", "alloc_group", "alloc_group_desc", "alloc_r_tariff",
  "alloc_r_tariff_desc", "alloc_r_stimulus", "alloc_r_stimulus_desc"
)
tmp_dt_merge_1 <- merge(
  x = dt_metering_e[!is.na(datetime)],
  # ## Note: Do NOT use observations with weird inverval
  y = dt_allocation_e[, .SD, .SDcols = cols_extract.from.allocation],
  by = "id",
  all.x = TRUE
)

# # 2.2. Merge with weather dataset
dt_metering_ext_e <- merge(
  x = tmp_dt_merge_1,
  y = dt_weather_hourly[station %like% "Dublin", .(datetime, temp)],
  by = "datetime",
  all.x = TRUE
)


# ------- Save the combined DT in parquet format -------
write_parquet(
  dt_metering_ext_e,
  sink = PATH_TO.SAVE_CER_COMBINED,
  compression = "snappy",
  use_dictionary = TRUE
)

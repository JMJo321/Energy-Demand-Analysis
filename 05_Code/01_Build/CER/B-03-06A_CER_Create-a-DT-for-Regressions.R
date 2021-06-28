# < Description > *
# > Script Group Indicator Number and Name
# # : B-03, CER
# #
# > Script Number(s)
# # : B-03-06A
# #
# > Purpose of the script(s)
# # : Create a DT for running Regressions

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
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


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Dataset(s) is(are) loaded
# # 1.1. For Metering Data
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_METERING_ELECTRICITY <-
  "CER_Extended-Metering_Electricity.parquet"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.LOAD_CER_METERING_ELECTRICITY,
  sep = "/"
)

# # 2. Path(s) to which Ouputs will be stored
# # 2.1. For the DT created to run regressions
DIR_TO.SAVE_CER <- DIR_TO.LOAD_CER
FILE_TO.SAVE_CER_DT <- "CER_DT-for-Regressions.RData"
PATH_TO.SAVE_CER_DT <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.SAVE_CER,
  FILE_TO.SAVE_CER_DT,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. A list of reference temperatures by Hour of Day
# ## Note:
# ## Reference temperatures means the temperature at which electricity
# ## consumption is minimized. Those temperatures are chosen manually.
list_ref.temperature_by.hour <- list(
  `0` = 64, `1` = 66, `2` = 66, `3` = 66, `4` = 66, `5` = 66,
  `6` = 66, `7` = 66, `8` = 66, `9` = 68, `10` = 68, `11` = 68,
  `12` = 70, `13` = 72, `14` = 72, `15` = 70, `16` = 70, `17` = 68,
  `18` = 68, `19` = 68, `20` = 66, `21` = 66, `22` = 66, `23` = 66
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT for running Regressions
# ------------------------------------------------------------------------------
# ------- Load DT(s) required -------
# # 1. Load the Combined Metering Dataset
dt_metering_e <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Create a DT to run regressions -------
# # 1. Create a DT from the combined metering dataset
# # 1.1. Create a temporary DT by subetting the combined metering dataset
# # 1.1.1. Define conditions to subset the combined metering dataset
conditions_subset_incl.control <- paste(
  "alloc_group == '1'", # Residential only
  "!is.na(interval_hour)", # There are obesrvations that `interval_hour` > 48
  "7 <= month(date)",
  sep = " & "
)
# # 1.1.2. Create a temporary DT by using the conditions
tmp_dt_for.reg <-
  dt_metering_e[eval(parse(text = conditions_subset_incl.control))]

# # 1.2. Create a DT by adding an indicator variable that shows whether an
# #      observation is for more harsh weather condition during the treatment
# #      period
# # 1.2.1. Create a DT that includes temperature ranges for each hour of day
# #        during the baseline period
breaks_temp_f <- seq(10, 80, by = 2)
tmp_dt_for.reg[
  ,
range_temp_f := cut(temp_f, breaks = breaks_temp_f)
]
dt_temp.ranges_baseline <- tmp_dt_for.reg[
  is_treatment.period == FALSE,
  .N,
  keyby = .(range_temp_f, interval_hour)
]
dt_temp.ranges_baseline[, N := NULL]
stopifnot(
  dt_temp.ranges_baseline[, .N, by = .(range_temp_f, interval_hour)][N > 1] == 0
)
# # 1.2.2. Add two columns that include the minimum and the maximum temperature
# #        ranges given a hour of day during the baseline period
hours <- dt_temp.ranges_baseline[, .N, by = .(interval_hour)]$interval_hour
for (hr in hours) {
  # ## Make an ordered vector including temperature ranges for each hour of day
  tmp_ranges <- dt_temp.ranges_baseline[
    interval_hour == hr, .N, keyby = .(range_temp_f)
  ]$range_temp_f
  # ## Add columns by using the ordered vector
  dt_temp.ranges_baseline[
    interval_hour == hr,
    `:=` (
      range_temp_f_min.in.hour = tmp_ranges[1],
      range_temp_f_max.in.hour = tmp_ranges[length(tmp_ranges)]
    )
  ]
}
# # 1.2.3. Create a DT by merging the temporary DT with the DT created above
dt_for.reg <- merge(
  x = tmp_dt_for.reg,
  y = dt_temp.ranges_baseline[
    ,
    .N,
    keyby = .(interval_hour, range_temp_f_min.in.hour, range_temp_f_max.in.hour)
    # To extract necessary information only
  ][
    , N := NULL   # This column is unnecessary.
  ],
  by = "interval_hour",
  all.x = TRUE
)
# # 1.2.4. Add an indicator variable
dt_for.reg[
  ,
  is_within.temperature.range := (
    as.numeric(range_temp_f_min.in.hour) <= as.numeric(range_temp_f) &
      as.numeric(range_temp_f) <= as.numeric(range_temp_f_max.in.hour)
  )
]
# # 1.2.5. Add a factor variable for observations with TRUE for the indicator
# #        variable
dt_for.reg[
  is_within.temperature.range == TRUE,
  range_temp_f_selected := as.character(range_temp_f)
]
dt_for.reg[, range_temp_f_selected := factor(range_temp_f_selected)]
# ## Note:
# ## This process is necessary to make plots correctly.
# # 1.2.6. Drop unnecessary columns
dt_for.reg[
  ,
  `:=` (
    range_temp_f_min.in.hour = NULL,
    range_temp_f_max.in.hour = NULL
  )
]


# # 2. Modify the temporary DT
# # 2.1. Add columns that are related to Temperature
# # 2.1.1. Add a column that shows Heating Degree Days (HDDs)
dt_for.reg[, hdd := 65 - soil_f]
dt_for.reg[hdd < 0, hdd := 0]
# # 2.1.2. Add a column that shows Heating Degree by Hour of Day
dt_for.reg[
  ,
  ref.temperature := list_ref.temperature_by.hour[as.character(interval_hour)]
]
dt_for.reg[, hd_by.hour := as.numeric(ref.temperature) - temp_f]
dt_for.reg[hd_by.hour < 0, hd_by.hour := 0]

# # 2.2. Add columns that are related to Treatment Status and/or Period
# # 2.2.1. Add an indicator variable for treatment status and period
dt_for.reg[
  !is.na(is_treated_r) & !is.na(is_treatment.period),
  treatment.and.post := is_treated_r & is_treatment.period
]
# # 2.2.2. Add a column, in factor type, that shows treatment status and period
# #        by hour of day
dt_for.reg[
  treatment.and.post == TRUE,
  treatment.and.post_by.hour := interval_hour
]
dt_for.reg[
  treatment.and.post == FALSE,
  treatment.and.post_by.hour := -1
]
dt_for.reg[
  ,
  treatment.and.post_by.hour := factor(treatment.and.post_by.hour)
]
# # 2.2.3. Add a column, in factor type, that shows treatment status and period
# #        by hour of day and range of temperature
dt_for.reg[
  ,
  tmp_hour.by.temperature := paste(interval_hour, range_temp_f, sep = "_")
]
dt_for.reg[
  treatment.and.post == TRUE,
  treatment.and.post_by.hour.and.temperature := tmp_hour.by.temperature
]
dt_for.reg[
  treatment.and.post == FALSE,
  treatment.and.post_by.hour.and.temperature := "-1"
]
dt_for.reg[
  ,
  `:=` (
    treatment.and.post_by.hour.and.temperature =
      factor(treatment.and.post_by.hour.and.temperature),
    tmp_hour.by.temperature = NULL
  )
]

# # 2.3. Add columns for Fixed-Effects Models
# # 2.3.1.
dt_for.reg[, day.of.week := lubridate::wday(date)]
dt_for.reg[
  ,
  `:=` (
    id_in.factor = factor(id),
    month_in.factor = (month(date) %>% factor(.)),
    id.and.day.of.week_in.factor = (
      paste(id, day.of.week, sep = "-") %>% factor(.)
    )
  )
]


# ------- Save the DT created above -------
# # 1. Save the DT created above in .RData format
save(dt_for.reg, file = PATH_TO.SAVE_CER_DT)

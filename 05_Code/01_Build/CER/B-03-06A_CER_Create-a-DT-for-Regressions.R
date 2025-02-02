# < Description > *
# > Script Group Indicator Number and Name
# # : B-03, CER
# #
# > Script Number(s)
# # : B-03-06A
# #
# > Purpose of the script(s)
# # : Create a DT, which includes a balanced panel data, for running Regressions

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
FILE_TO.SAVE_CER_DT <- "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.SAVE_CER_DT <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.SAVE_CER,
  FILE_TO.SAVE_CER_DT,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Vectors of Month of Year that show each month's season
season_warm <- 7:10
season_cold <- 11:12
# ## Note:
# ## Those vectors are created based on the plot generated from A-01-04B_A1.


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT for running Regressions
# ------------------------------------------------------------------------------
# ------- Load DT(s) required -------
# # 1. Load the Combined Metering Dataset
dt_metering_elec <-
  read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY) %>% setDT(.)
# load(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Create a DT to run regressions -------
# # 1. Create a DT from the combined metering dataset
# # 1.1. Create a temporary DT by subetting the combined metering dataset
# # 1.1.1. Define conditions to subset the combined metering dataset
conditions_subset <- paste(
  "alloc_group == '1'", # Residential only
  sep = " & "
)
# # 1.1.2. Create a temporary DT by using the conditions
tmp_dt_for.reg <- dt_metering_elec[eval(parse(text = conditions_subset))]

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
# # 2.1.1. Add columns that show Daily Mean Temperature
# # 2.1.1.1. Compute daily mean temperature by using the max. and the min.
# #          hourly temperature
dt_for.reg[
  ,
  mean.temp_extremes_f :=
    (max(temp_f, na.rm = TRUE) + min(temp_f, na.rm = TRUE)) / 2,
  by = .(date)
]
# # 2.1.1.2. Compute daily mean temperature by using all hourly temperatures
dt_for.reg[
  ,
  mean.temp_all_f := mean(temp_f, na.rm = TRUE), by = .(date)
]
# # 2.1.2. Add a column that shows Heating Degree Days (HDDs)
# # 2.1.2.1. By using `mean.temp_extremes`
dt_for.reg[, hdd_extremes := 65 - mean.temp_extremes_f]
dt_for.reg[hdd_extremes < 0, hdd_extremes := 0]
# # 2.1.2.2. By using `mean.temp_all`
dt_for.reg[, hdd_all := 65 - mean.temp_all_f]
dt_for.reg[hdd_all < 0, hdd_all := 0]
# # 2.1.2.3. By using `soil_f`
dt_for.reg[, hdd_soil := 65 - soil_f]
dt_for.reg[hdd_soil < 0, hdd_soil := 0]
# # 2.1.3. Add a column that shows Heating Degree by Rate Period and Season
# # 2.1.3.1. Add a column that shows each observation's season
dt_for.reg[month(date) %in% season_warm, season := "Warm"]
dt_for.reg[month(date) %in% season_cold, season := "Cold"]
# # 2.1.3.2. Compute reference temperatures
max.temp_peak.and.cold <-
  dt_for.reg[
    rate.period_detail == "Peak (17-18)" & season == "Cold"
  ]$temp_f %>% max(., na.rm = TRUE)
max.temp_others <- dt_for.reg$temp_f %>% max(., na.rm = TRUE)
# # 2.1.3.3. Add a column that shows each observation's reference temperature
dt_for.reg[
  rate.period_detail == "Peak (17-18)" & season == "Cold",
  ref.temp_by.season.and.rate.period_f := max.temp_peak.and.cold
]
dt_for.reg[
  is.na(ref.temp_by.season.and.rate.period_f),
  ref.temp_by.season.and.rate.period_f := max.temp_others
]
# # 2.1.3.4. Add a column that each observation's heating degree
dt_for.reg[
  ,
  hd_by.season.and.rate.period := ref.temp_by.season.and.rate.period_f - temp_f
]
dt_for.reg[hd_by.season.and.rate.period < 0, hd_by.season.and.rate.period := 0]

# # 2.2. Add columns that are related to Treatment Group and/or Period
# # 2.2.0. Add columns, in factor type, that are related to Treatment Group and
# #        Period
# # 2.2.0.1. Regarding treatment groups
dt_for.reg[is_treated_r == TRUE, group := "Treatment"]
dt_for.reg[is_treated_r == FALSE, group := "Control"]
dt_for.reg[
  ,
  group := factor(group, levels = c("Control", "Treatment"), ordered = TRUE)
]
# # 2.2.0.2. Regarding treatment periods
dt_for.reg[is_treatment.period == TRUE, period := "Treatment"]
dt_for.reg[is_treatment.period == FALSE, period := "Baseline"]
dt_for.reg[
  ,
  period := factor(period, levels = c("Baseline", "Treatment"), ordered = TRUE)
]
# # 2.2.1. Add an indicator variable for treatment group and period
dt_for.reg[
  !is.na(is_treated_r) & !is.na(is_treatment.period),
  treatment.and.post := is_treated_r & is_treatment.period
]
# # 2.2.2. Add a column, in factor type, that shows treatment group and period
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
# # 2.2.3. Add a column, in factor type, that shows treatment group and period
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
dt_for.reg[
  ,
  `:=` (
    id_in.factor = factor(id),
    day_in.factor = factor(day),
    day.of.week_in.factor = factor(day.of.week),
    id.and.day.of.week_in.factor = (
      paste(id, day.of.week, sep = "-") %>% factor(.)
    ),
    month_in.factor = (month(date) %>% factor(.))
  )
]

# # 2.4. Add columns that indicate whether an observation should be dropped
# #      when constructing a sample or not
# # 2.4.1. Create a vector that includes IDs with zero daily consumption
# #        at least 8 times or more
dt_daily.consumption <- dt_for.reg[,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, date)
]
ids_drop <- dt_daily.consumption[kwh == 0, .N, by = .(id)][N >= 8]$id
# # 2.4.2. Create a vector that includes dates for which several households
# #        do NOT have consumption data
# # 2.4.2.1. Make objects that will be used later
date_min <- dt_for.reg[, .N, by = .(date)]$date %>% min(., na.rm = TRUE)
date_max <- dt_for.reg[, .N, by = .(date)]$date %>% max(., na.rm = TRUE)
n_obs <- dt_for.reg[, .N, by = .(id)]$N %>% max(., na.rm = TRUE)
# # 2.4.2.2. Identify IDs that have less observations
ids_missing.obs <- dt_for.reg[, .N, by = .(id)][N != n_obs]$id
# # 2.4.2.3. Identify dates for which IDs having less obervations do NOT have
# #          observations
dt_missing.obs <- setDT(NULL)
dt_for.check <- dt_for.reg[, .N, keyby = .(datetime)][, .(datetime)]
for (id_ in ids_missing.obs) {
  tmp_datetimes <- dt_for.reg[id == id_, .N, by = .(datetime)]$datetime
  tmp_dt <- dt_for.check[!(datetime %in% tmp_datetimes)]
  tmp_dt[, id := id_]
  tmp_dt[, n := .N]
  dt_missing.obs <- rbind(dt_missing.obs, tmp_dt)
}
dt_missing.obs[, date := date(datetime)]
date_drop <- dt_missing.obs[, .N, keyby = .(date)]$date
# # 2.4.3. Add indicator variables based on vectors created above
dt_for.reg[, is_having.zero.consumption.day := id %in% ids_drop]
dt_for.reg[, is_missing.date := date %in% date_drop]
stopifnot(
  dt_for.reg[
    is_missing.date == FALSE, .N, by = .(id)
  ][
    , .N, by = .(N)
  ][
    , .N
  ] == 1
)

# # 2.5. Add columns that indicate whether an observation is included in the
# #      sample or not
# # 2.5.1. Set conditions for the indicator variable
# # 2.5.1.1. For a sample that includes the control group
conditions_for.sample.construction_incl.control <- paste(
  "7 <= month(date)", # Baseline period began July 14, 2009
  "is_weekend == FALSE", # TOU pricing was active on nonholiday weekdays
  "is_holiday == FALSE", # TOU pricing was active on nonholiday weekdays
  "is_having.zero.consumption.day == FALSE", # Days with zero kwh is unreasonable
  "is_missing.date == FALSE", # To make a balanced panel dataset
  sep = " & "
)
# # 2.5.1.2. For a sample that excludes the control group
conditions_for.sample.construction_excl.control <- paste(
  conditions_for.sample.construction_incl.control,
  "is_treated_r == TRUE",
  sep = " & "
)
# # 2.5.2. Add indicator variables
# # 2.5.2.1. For a sample that includes the control group
dt_for.reg[
  ,
  is_in.sample_incl.control := eval(
    parse(text = conditions_for.sample.construction_incl.control)
  )
]
# # 2.5.2.2. For a sample that excludes the control group
dt_for.reg[
  ,
  is_in.sample_excl.control := eval(
    parse(text = conditions_for.sample.construction_excl.control)
  )
]

# # 2.6. Drop unnecessary columns
cols_keep <-
  names(dt_for.reg)[str_detect(names(dt_for.reg), "_sme", negate = TRUE)]
dt_for.reg <- dt_for.reg[, .SD, .SDcols = cols_keep]

# # 2.7. Reorder columns
cols_reorder <- c(
  "id", "alloc_group", "alloc_group_desc",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "is_treated_r", "group", "is_treatment.period", "period",
  "treatment.and.post", "treatment.and.post_by.hour",
  "treatment.and.post_by.hour.and.temperature",
  "day", "date", "datetime", "interval_hour", "interval_30min",
  "rate.period", "length_rate.period",
  "rate.period_detail", "length_rate.period_detail",
  "day.of.week", "season",
  "is_weekend", "is_holiday",
  "is_having.zero.consumption.day", "is_missing.date",
  "is_within.temperature.range",
  "is_in.sample_incl.control", "is_in.sample_excl.control",
  "kwh",
  "temp_f", "soil_f", "range_temp_f", "range_temp_f_selected",
  "mean.temp_extremes_f", "mean.temp_all_f",
  "hdd_extremes", "hdd_all", "hdd_soil",
  "ref.temp_by.season.and.rate.period_f", "hd_by.season.and.rate.period",
  "id_in.factor", "day_in.factor", "day.of.week_in.factor",
  "id.and.day.of.week_in.factor", "month_in.factor"
)
setcolorder(dt_for.reg, cols_reorder)

# # 2.8 Sort Observations
keys <- c("id", "datetime")
setkeyv(dt_for.reg, keys)


# ------- Save the DT created above -------
# # 1. Save the DT created above in Parquet format
write_parquet(
  dt_for.reg,
  sink = PATH_TO.SAVE_CER_DT,
  compression = "snappy",
  use_dictionary = TRUE
)

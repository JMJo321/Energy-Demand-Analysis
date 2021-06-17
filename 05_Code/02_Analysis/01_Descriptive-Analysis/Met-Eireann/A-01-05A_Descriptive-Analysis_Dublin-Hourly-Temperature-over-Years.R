# < Description > *
# > Script Group Indicator Number and Name
# # : B-05, Met-Eireann
# #
# > Script Number(s)
# # : B-05-01A
# #
# > Purpose of the script(s)
# # : Ingest Met Eireann's Hourly-Level Weather Data

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(ggplot2)
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
# # 1. Path(s) from which Hourly Weather Data files will be read
DIR_TO.LOAD_WEATHER <- "Met-Eireann"
FILE_TO.LOAD_WEATHER <- "Met-Eireann_Weather-Data_Hourly.parquet"
PATH_TO.LOAD_WEATHER <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_WEATHER,
  FILE_TO.LOAD_WEATHER,
  sep = "/"
)

# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(PATH_NOTE, "05_Ireland-Temperature-Trends", sep = "/")


# ------- Define parameter(s) -------
# # 1. Trail Begin and End Dates
DATE_TRIAL_BEGIN <- as.Date("2009-07-01")
DATE_TRIAL_END <- as.Date("2010-12-31")


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT to make plot(s)
# ------------------------------------------------------------------------------
# ------- Load Weather Data -------
dt_weather_hourly <- arrow::read_parquet(PATH_TO.LOAD_WEATHER)


# ------- Create a DT that will be exploited to make plot(s) -------
# # 1. Make a DT containing temperature data for trial period
dt_weather_hourly_avg_trial.period <- dt_weather_hourly[
  station %like% "Dublin" &
    as.Date(datetime) %in%
      seq.Date(from = DATE_TRIAL_BEGIN, to = DATE_TRIAL_END, by = "1 day"),
  .(datetime, temp)
][
  ,
  category := "Trial Period"
]

# # 2. Make a DT containing average temperature for three years:
# #    from 2009 to 2011
dt_weather_hourly_avg_3year <- dt_weather_hourly[
  station %like% "Dublin" &
    year %in% seq(from = 2010 - 1, to = 2010 + 1, by = 1),
  .(month, day, hour, temp)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp",
  by = .(month, day, hour)
][
  ,
  `:=` (
    datetime = as.POSIXct(
      paste("2010", month, day, hour, sep = "-"), format = "%Y-%m-%d-%H"
    ),
    category = "3-Year Avg."
  )
][
  ,
  .(datetime, temp, category)
]

# # 3. Make a DT containing average temperature for five years:
# #    from 2008 to 2012
dt_weather_hourly_avg_5year <- dt_weather_hourly[
  station %like% "Dublin" &
    year %in% seq(from = 2010 - 2, to = 2010 + 2, by = 1),
  .(month, day, hour, temp)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp",
  by = .(month, day, hour)
][
  ,
  `:=` (
    datetime = as.POSIXct(
      paste("2010", month, day, hour, sep = "-"), format = "%Y-%m-%d-%H"
    ),
    category = "5-Year Avg."
  )
][
  ,
  .(datetime, temp, category)
]

# # 4. Make a DT containing average temperature for Seven years:
# #    from 2007 to 2013
dt_weather_hourly_avg_7year <- dt_weather_hourly[
  station %like% "Dublin" &
    year %in% seq(from = 2010 - 3, to = 2010 + 3, by = 1),
  .(month, day, hour, temp)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp",
  by = .(month, day, hour)
][
  ,
  `:=` (
    datetime = as.POSIXct(
      paste("2010", month, day, hour, sep = "-"), format = "%Y-%m-%d-%H"
    ),
    category = "7-Year Avg."
  )
][
  ,
  .(datetime, temp, category)
]

# # 5. Make a DT containing average temperature for ten years:
# #    from 2005 to 2015
dt_weather_hourly_avg_10year <- dt_weather_hourly[
  station %like% "Dublin" &
    year %in% seq(from = 2010 - 5, to = 2010 + 5, by = 1),
  .(month, day, hour, temp)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp",
  by = .(month, day, hour)
][
  ,
  `:=` (
    datetime = as.POSIXct(
      paste("2010", month, day, hour, sep = "-"), format = "%Y-%m-%d-%H"
    ),
    category = "10-Year Avg."
  )
][
  ,
  .(datetime, temp, category)
]

# # 6. Create a DT by combining DTs made above
# # 6.1. Combine DTs
dt_weather_hourly_avg <- rbind(
  dt_weather_hourly_avg_trial.period,
  dt_weather_hourly_avg_3year,
  dt_weather_hourly_avg_5year,
  dt_weather_hourly_avg_7year,
  dt_weather_hourly_avg_10year
)

# # 6.2. Convert data type from character to factor
dt_weather_hourly_avg[
  ,
  category := factor(
    category,
    levels = c(
      "Trial Period",
      "3-Year Avg.", "5-Year Avg.", "7-Year Avg.", "10-Year Avg."
    )
  )
]


# ------------------------------------------------------------------------------
# Create ggplot object(s) and save as PNG format
# ------------------------------------------------------------------------------
# ------- Create ggplot object(s) -------
# # 1. Plot showing Actual and Average Temperature in Dublin
plot_temperature <-
  ggplot() +
    geom_line(
      data = dt_weather_hourly_avg,
      aes(x = datetime, y = temp, color = category),
      alpha = 0.7
    ) +
    scale_color_brewer(palette = "Spectral") +
    scale_x_datetime(
      breaks = seq.POSIXt(
        from = as.POSIXct("2009-07-01 00:00:00"),
        to = as.POSIXct("2011-01-01 00:00:00"),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    theme_linedraw() +
    labs(x = "", y = "Temperature  (Degrees Celsius)", color = "Category")


# ------- Create a DT by importing CSV files -------
plot.save(
  paste(
    DIR_TO.SAVE_PLOT, "Met-Eireann_Average-Temperatures_Dublin.png", sep = "/"
  ),
  plot_temperature,
  width = 40, height = 20, units = "cm"
)

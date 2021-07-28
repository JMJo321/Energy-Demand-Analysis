# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, Descriptive Analysis
# #
# > Script Number(s)
# # : B-01-05B
# #
# > Purpose of the script(s)
# # : Create Plot(s) from Met Eireann's Daily-Level Weather Data.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
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
# # 1. Path(s) from which Daily Weather Data files will be read
DIR_TO.LOAD_WEATHER <- "Met-Eireann"
FILE_TO.LOAD_WEATHER <- "Met-Eireann_Weather-Data_Daily.parquet"
PATH_TO.LOAD_WEATHER <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_WEATHER,
  FILE_TO.LOAD_WEATHER,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Dates related to the Trial Period
DATE_TRIAL_BEGIN <- as.Date("2009-07-01")
DATE_TRIAL_END <- as.Date("2011-01-01")


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create Plot(s)
# ------------------------------------------------------------------------------
# ------- Create a DT for making plot(s) -------
# # 1. Load Daily Data
dt_weather_daily <- arrow::read_parquet(PATH_TO.LOAD_WEATHER)


# # 2. Create a DT to make plot(s)
# # 2.1. Melt the DT loaded
cols_measure <- c("maxtp_c", "mintp_c", "gmin_c", "soil_c")
dt_melted <- melt(
  data = dt_weather_daily[, .SD, .SDcols = c("date", cols_measure)],
  id.vars = "date",
  measure.vars = cols_measure,
  variable.name = "category",
  value.name = "temp_c"
)

# # 2.2. Add column(s)
# # 2.2.1. Add a column indicating year-months
dt_melted[, year.month := as.yearmon(date)]


# ------- Create Plot(s) -------
plot_temperatures <-
  ggplot(
    data = dt_melted[DATE_TRIAL_BEGIN <= date & date <= DATE_TRIAL_END],
    aes(x = date, y = temp_c, color = category)
  ) +
    geom_point(size = 1) +
    geom_smooth(method = "loess", formula = y ~ x, alpha = 0.2) +
    scale_x_date(
      breaks = seq.Date(
        from = DATE_TRIAL_BEGIN, to = DATE_TRIAL_END, by = "month"
      ),
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_viridis_d() +
    labs(x = "", y = "Temperature (Degrees Celsius)") +
    theme_linedraw()

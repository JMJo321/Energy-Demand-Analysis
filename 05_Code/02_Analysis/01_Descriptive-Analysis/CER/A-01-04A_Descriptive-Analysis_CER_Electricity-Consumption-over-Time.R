# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-04A
# #
# > Purpose of the script(s)
# # : Descriptive Analysis - Electricity Consumption over time

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

# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(PATH_NOTE, "07_CER-Descriptive-Analysis", sep = "/")


# ------- Define parameter(s) -------
# # 1. Treatement Begin Date
DATE_BEGIN.OF.TREATMENT <- as.Date("2010-01-01")


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create DTs that are for making plots
# ------------------------------------------------------------------------------
# ------- Load the combined metering dataset -------
# # 1. Load the dataset
dt_metering_e <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)

# # 2. Add a column in factor type
dt_metering_e[
  ,
  is_treatment.period := factor(
    date >= DATE_BEGIN.OF.TREATMENT, levels = c(TRUE, FALSE)
  )
]


# ------- Create DT(s): Consumption by Hour of Day -------
cols_by_hour <- c(
  "interval_hour",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "is_treatment.period"
)
dt_avg.kwh_hour <- dt_metering_e[
  alloc_group == '1' &  # Residential only
    !is.na(interval_hour) &  # There are obesrvations that `interval_hour` > 48
    month(date) >= 7,  # The first date of the baseline period was July 1, 2009
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_hour
]


# ------- Create DT(s): Consumption over time -------
cols_by_date <- c(
  "date",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc"
)
dt_avg.kwh_date <- dt_metering_e[
  alloc_group == '1' &  # Residential only
    !is.na(interval_hour),
    # The first date of the baseline period was July 1, 2009
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = c("id", cols_by_date)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_date
]


# ------------------------------------------------------------------------------
# Create Plots
# ------------------------------------------------------------------------------
# ------- Set common plot options -------
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Create ggplot objects -------
# # 1. For Average Consumption by Hour of Day
plot_avg.kwh_interval <-
  ggplot(
    data = dt_avg.kwh_hour[alloc_r_tariff %in% c("A", "B", "C", "D")]
  ) +
    geom_line(aes(x = interval_hour, y = kwh, color = is_treatment.period)) +
    geom_line(
      data = dt_avg.kwh_hour[
        alloc_r_tariff == "E",
        .(interval_hour, kwh, is_treatment.period)
      ],
      aes(x = interval_hour, y = kwh, color = is_treatment.period),
      linetype = 'dashed', alpha = 0.7
    ) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    facet_grid(alloc_r_stimulus_desc ~ alloc_r_tariff_desc) +
    scale_x_continuous(breaks = seq(0, 24, by = 2)) +
    labs(
      x = "Hour of Day",
      y = "Consumption (kWh per Hour)",
      color = "Treatment\nPeriod"
    ) +
    plot.options


# # 2. For Average Consumption over Time
# # 2.1. By Tariff
plot_avg.kwh_date_by.tariff <-
  ggplot(
    data = dt_avg.kwh_date[alloc_r_tariff %in% c("A", "B", "C", "D")]
  ) +
    geom_line(aes(x = date, y = kwh, color = alloc_r_stimulus_desc)) +
    geom_line(
      data = dt_avg.kwh_date[alloc_r_tariff == "E", .(date, kwh)],
      aes(x = date, y = kwh),
      linetype = 'dashed', alpha = 0.5
    ) +
    geom_vline(
      aes(xintercept = as.Date("2010-01-01")),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(. ~ alloc_r_tariff_desc) +
    scale_x_date(
      breaks = seq.Date(
        from = as.Date("2009-07-01"), to = as.Date("2011-01-01"),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    scale_color_viridis_d() +
    labs(
      x = "",
      y = "Consumption (kWh per Day)",
      color = "Stimuli"
    ) +
    plot.options

# # 2.2. By Stimulus
plot_avg.kwh_date_by.stimulus <-
  ggplot(
    data = dt_avg.kwh_date[alloc_r_tariff %in% c("A", "B", "C", "D")]
  ) +
    geom_line(aes(x = date, y = kwh, color = alloc_r_tariff_desc)) +
    geom_line(
      data = dt_avg.kwh_date[alloc_r_tariff == "E", .(date, kwh)],
      aes(x = date, y = kwh),
      linetype = 'dashed', alpha = 0.5
    ) +
    geom_vline(
      aes(xintercept = as.Date("2010-01-01")),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(. ~ alloc_r_stimulus_desc) +
    scale_x_date(
      breaks = seq.Date(
        from = as.Date("2009-07-01"), to = as.Date("2011-01-01"),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    scale_color_viridis_d() +
    labs(
      x = "",
      y = "Consumption (kWh per Day)",
      color = "Tariffs"
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save ggplot objects in PNG format
# ------------------------------------------------------------------------------
# ------- Save plots created above in PNG format -------
# # 1. For Average Consumption by Hour of Day
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Hour-Level_By-Tariff-and-Stimulus_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_interval,
  width = 50, height = 25, units = "cm"
)


# # 2. For Average Consumption over Time
# # 2.1. By Tariff
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Date-Level_By-Tariff_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_date_by.tariff,
  width = 50, height = 25, units = "cm"
)

# # 2.2. By Stimulus
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Date-Level_By-Stimulus_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_date_by.stimulus,
  width = 50, height = 25, units = "cm"
)

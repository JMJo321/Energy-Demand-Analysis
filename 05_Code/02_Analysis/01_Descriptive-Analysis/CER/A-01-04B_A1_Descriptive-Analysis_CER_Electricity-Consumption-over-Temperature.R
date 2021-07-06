# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04B_A1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Electricity Consumption over Temperature at
# # Rate Period-level Data.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(latex2exp)
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
FILE_TO.LOAD_CER_FOR.REG_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.RData"
PATH_TO.LOAD_CER_FOR.REG_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_FOR.REG_ELECTRICITY,
  sep = "/"
)

# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(
  PATH_NOTE, "07_CER-Trials", "02_Figures", "Descriptive-Analysis",
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load DT(s), and Create Object(s) from it to make Plot(s)
# ------------------------------------------------------------------------------
# ------- Load DT(s) -------
# # 1. Load the dataset for regression analysis
load(PATH_TO.LOAD_CER_FOR.REG_ELECTRICITY)


# ------- Create Object(s) -------
# # 1. Create a DT that includes rate-period-level consumption
# # 1.1. Make objects that will be use to aggregate data
cols_by_sum <- c(
  "id", "date", # These two columns determine the aggregation level
  "group", "period", "rate.period", "length_rate.period",
  "season", "month_in.factor", "mean.temp_all_f"
)
cols_by_mean <- cols_by_sum[-1]

# # 1.2. Create a DT by aggregating the DT loaded above
dt_avg.kwh_by.rate.period <-
  dt_for.reg[ # Aggregate data at ID-by-Rate-Period level
    is_in.sample_incl.control == TRUE,
    lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
    by = cols_by_sum
  ][ # Compute average consumption across households
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = cols_by_mean
  ]

# # 1.3. Modify the DT created above
# # 1.3.1. Add column(s)
# # 1.3.1.1. Add a column that shows per-hour consumption
dt_avg.kwh_by.rate.period[, kwh_per.hour := kwh / length_rate.period]
# ## Note:
# ## Each rate period has different lengths of time.

# # 1.3.1.2. Add a column that shows whether each observation is for the last
# #          five days in years
dt_avg.kwh_by.rate.period[
  period == "Treatment" & month_in.factor == "12" & 41 < mean.temp_all_f &
    2 < kwh_per.hour,
  .N, keyby = .(date)
]
# ## Note:
# ## This demonstrates that unusual high consumption given temperature was for
# ## the last five days in years.
date_last.five.days <- dt_avg.kwh_by.rate.period[
  period == "Treatment" & month_in.factor == "12" & 41 < mean.temp_all_f &
    2 < kwh_per.hour,
  .N, keyby = .(date)
]$date
date_last.five.days <-
  date_last.five.days %>%
    as.character(.) %>%
    str_replace(., "^2010-", "2009-") %>%
    as.Date(.) %>%
    c(., date_last.five.days)
dt_avg.kwh_by.rate.period[
  ,
  is_last.five.days := date %in% date_last.five.days
]


# ------------------------------------------------------------------------------
# Create Plots
# ------------------------------------------------------------------------------
# ------- Set Common Plot Options -------
# # 1. Set Common Plot Options
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# # 2. Create a Color Palette
color.palette_signal <- unikn::usecol(pal = pal_signal, n = 3)


# ------- Create ggplot object(s): W.R.T. Avg. kWh by Rate Period -------
# # 1. Plot for Average Consumption by Rate Period
plot_consumption_rate.period <-
  ggplot() +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period %in% c("Night", "Day")],
      aes(x = mean.temp_all_f, y = kwh_per.hour),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.2
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period == "Peak"],
      aes(x = mean.temp_all_f, y = kwh_per.hour, group = season),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.2
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour, color = month_in.factor,
        shape = is_last.five.days
      ),
      size = 1.5, alpha = 0.8
    ) +
    facet_grid(rate.period ~ group + period) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Hourly Average Consumption  (kWh per Hour)",
      color = "Month of Year",
      shape = "Last Five Days\nin Years"
    ) +
    plot.options


# ------- Create ggplot object(s): W.R.T. Temperature Distribution -------
# # 1. Histogram of Temperature Distribution
plot_temperature_by.month <-
  ggplot(
    data = dt_for.reg[
      is_in.sample_incl.control == TRUE,
      .N, by = .(date, month_in.factor, rate.period, temp_f)
    ]
  ) +
    geom_histogram(
      aes(x = temp_f, fill = rate.period),
      binwidth = 1.0, position = "identity",
      alpha = 0.5
    ) +
    facet_wrap(month_in.factor ~ ., nrow = 2) +
    scale_x_continuous(breaks = seq(10, 70, by = 10)) +
    scale_fill_manual(values = color.palette_signal) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Count",
      fill = "Rate Periods"
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save ggplot objects in PNG format
# ------------------------------------------------------------------------------
# ------- Save plots created above in PNG format -------
# # 1. Plot regarding Average Consumption by Rate Period
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Rate-Period_Electricity.png",
    sep = "/"
  ),
  plot_consumption_rate.period,
  width = 50, height = 40, units = "cm"
)


# # 2. Plot regarding Temperature Distribution
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Distribution-of-Temperature_By-Rate-Period.png",
    sep = "/"
  ),
  plot_temperature_by.month,
  width = 50, height = 30, units = "cm"
)

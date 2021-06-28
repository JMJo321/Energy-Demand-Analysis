# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-04B
# #
# > Purpose of the script(s)
# # : Descriptive Analysis - Electricity Consumption over temperature

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
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
FILE_TO.LOAD_CER_METERING_ELECTRICITY <-
  "CER_Extended-Metering_Electricity.parquet"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.LOAD_CER_METERING_ELECTRICITY,
  sep = "/"
)

# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(
  PATH_NOTE, "07_CER-Trials", "02_Figures", "Descriptive-Analysis",
  sep = "/"
)


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


# # 2. Modify the DT loaded
# # 2.1. Add column(s)
# # 2.1.1. Add columns indicating whether each observation is in the treatment
# #        period
dt_metering_e[
  is_treatment.period == TRUE,
  is_treatment.period_in.factor := "Treatment"
]
dt_metering_e[
  is_treatment.period == FALSE,
  is_treatment.period_in.factor := "Baseline"
]
dt_metering_e[
  ,
  is_treatment.period_in.factor := factor(is_treatment.period_in.factor)
]
# # 2.1.2. Add a column showing ranges of temperature
breaks_temp_f <- seq(10, 80, by = 2)
dt_metering_e[, range_temp_f := cut(temp_f, breaks = breaks_temp_f)]


# ------- Create DT(s) -------
# # 1. Set conditions for subsetting the DT
conditions_by.temperature <- paste(
  "alloc_group == '1'", # Residential only
  "!is.na(interval_hour)", # There are obesrvations that `interval_hour` > 48
  "7 <= month(date)", # The first date of the baseline period was July 1, 2009
  sep = " & "
)


# # 2. Create DT(s)
# # 2.1. Consumption by Range of Temperature
# # 2.1.1. Create a DT
cols_by_temperature <- c(
  "range_temp_f", "interval_hour", "is_treated_r",
  "is_treatment.period_in.factor"
)
dt_avg.kwh_temperature <- dt_metering_e[
  eval(parse(text = conditions_by.temperature)),
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh", by = cols_by_temperature
]
# # 2.1.2. Add an indicator variable
# ## Define a function to get a vector of temperature ranges
get_temprature.ranges <- function (hour) {
  ranges <- dt_temp.ranges_baseline[
    interval_hour == hour, .N, keyby = .(range_temp_f)
  ]$range_temp_f
  return (ranges)
}
# ## Create a DT that will be used to assign values for the indicator variable
dt_temp.ranges_baseline <-
  dt_avg.kwh_temperature[
    is_treatment.period_in.factor == "Baseline",
    .N,
    keyby = .(range_temp_f, interval_hour)
  ]
# ## Assign values for the indicator variable
for (row in 1:dt_avg.kwh_temperature[, .N]) {
  tmp_temperature.range <-
    get_temprature.ranges(dt_avg.kwh_temperature[row]$interval_hour)
  dt_avg.kwh_temperature[
    row,
    is_within.temperature.range := (
      as.numeric(tmp_temperature.range[1]) <= as.numeric(range_temp_f) &
        as.numeric(range_temp_f) <=
          as.numeric(tmp_temperature.range[length(tmp_temperature.range)])
    )
  ]
}
# ## Transfor the indicator variable to a brand-new factor
dt_avg.kwh_temperature[
  is_within.temperature.range == TRUE,
  range_temp_f_selected := as.character(range_temp_f)
]
dt_avg.kwh_temperature[, range_temp_f_selected := factor(range_temp_f_selected)]

# # 2.2. Percentiles of Consumption by Range of Temperature
# # 2.2.1. Make a DT that includes percentiles of kwh by treatment group,
# #        treatment period, hour of day, and temperature range
# ## Create a DT
probabilities <- c(0.9, 0.75, 0.5, 0.25, 0.1)
dt_percentile.of.kwh_temperature <- dt_metering_e[
  eval(parse(text = conditions_by.temperature))
][
  is_treatment.period == FALSE,
  lapply(.SD, quantile, prob = probabilities, na.rm = TRUE), .SDcols = "kwh",
  keyby = cols_by_temperature
]
# ## Add a column that shows percentiles
dt_percentile.of.kwh_temperature[, tmp_n := 1:.N, by = cols_by_temperature]
dt_percentile.of.kwh_temperature[, percentile := probabilities[tmp_n]]
# ## Add a column showing percentiles, whose data type is factor
dt_percentile.of.kwh_temperature[
  ,
  percentile_in.factor := factor(
    paste0(percentile * 100, "%"),
    levels = paste0(probabilities * 100, "%")
  )
]
# # 2.2.2. Create a DT that shows ID-by-Temperature Range average consumption
# #        during the baseline period
dt_avg.kwh_id.by.temperature_baseline <- dt_metering_e[
  eval(parse(text = conditions_by.temperature))
][
  is_treatment.period_in.factor == "Baseline",
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, is_treated_r, interval_hour, range_temp_f)
]
# # 2.2.3. Add a column, to `dt_avg.kwh_id.by.temperature_baseline`, that shows
# #        the percentile range of each observation
percentiles <- c(
  "[0%,10%]", "(10%,25%]", "(25%,50%]", "(50%,75%]", "(75%,90%]", "(90%,100%]"
)
conditions_for.loop <- dt_avg.kwh_id.by.temperature_baseline[
  , .N, by = .(is_treated_r, interval_hour, range_temp_f)
][
  , N := NULL
]
for (row in 1:conditions_for.loop[, .N]) {
  # ## Create temporary object that will be used later
  row_group <- conditions_for.loop[row]$is_treated_r
  row_hour <- conditions_for.loop[row]$interval_hour
  row_range <- conditions_for.loop[row]$range_temp_f
  # ## Make a vector that include percentile values
  tmp_values <- dt_percentile.of.kwh_temperature[
    is_treated_r == row_group &
      interval_hour == row_hour &
      range_temp_f == row_range
  ]$kwh %>% c(1000, ., 0)
  # ## Add a column showing percentiles
  dt_avg.kwh_id.by.temperature_baseline[
    is_treated_r == row_group &   # Group
      interval_hour == row_hour &   # Hour of Day
      range_temp_f == row_range,   # Temperature Range
    range_percentile := cut(
      kwh,
      breaks = tmp_values,
      labels = percentiles,
      include.lowest = TRUE
    )
  ]
}
# # 2.2.4. Add a column showing ID-by-Hour-by-Temerature Range percentiles to
# #        to `dt_metering_e` by merging the DT created above
cols_by <- c("id", "interval_hour", "range_temp_f")
dt_metering_e <- merge(
  x = dt_metering_e,
  y = dt_avg.kwh_id.by.temperature_baseline[
    , .SD, .SDcols = c(cols_by, "range_percentile")
  ],
  by = cols_by,
  all.x = TRUE
)
# # 2.2.5. Create a DT that includes average consumption by aggregating raw data
dt_avg.kwh_percentile <- dt_metering_e[
  eval(parse(text = conditions_by.temperature))
][
  !is.na(range_percentile),
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = .(
    is_treated_r, is_treatment.period,
    interval_hour, range_temp_f, range_percentile
  )
]
# # 2.2.6. Modify the DT created above
# ## Add a column that shows period-related info. in factor data type
dt_avg.kwh_percentile[
  is_treatment.period == FALSE,
  is_treatment.period_in.factor := "Baseline"
]
dt_avg.kwh_percentile[
  is_treatment.period == TRUE,
  is_treatment.period_in.factor := "Treatment"
]
dt_avg.kwh_percentile[
  ,
  is_treatment.period_in.factor := factor(is_treatment.period_in.factor)
]
# ## Re-define factor values for `range_temp_f`
dt_avg.kwh_percentile[
  ,
  range_temp_f := as.character(range_temp_f) %>% as.factor(.)
]
# ## Re-define factor values for `range_percentile`
dt_avg.kwh_percentile[
  ,
  range_percentile := (
    as.character(range_percentile) %>% factor(., levels = rev(percentiles))
  )
]


# ------------------------------------------------------------------------------
# Create Plots
# ------------------------------------------------------------------------------
# ------- Set common plot options -------
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Create ggplot objects: W.R.T. Average Consumption -------
# # 1. For Average Consumption by Temperature Range
# # 1.1. For Control Group
plot_avg.kwh_temperature_control <-
  ggplot(data = dt_avg.kwh_temperature[is_treated_r == FALSE]) +
    geom_point(
      aes(x = range_temp_f, y = kwh, color = is_treatment.period_in.factor),
      alpha = 0.5
    ) +
    geom_smooth(
      aes(
        x = as.numeric(range_temp_f),
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      method = "loess", formula = y ~ x,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), labels = scales::comma) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "Average Consumption (kWh per Hour)",
      color = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# # 1.2. For Treatment Group
plot_avg.kwh_temperature_treatment <-
  ggplot(data = dt_avg.kwh_temperature[is_treated_r == TRUE]) +
    geom_point(
      aes(x = range_temp_f, y = kwh, color = is_treatment.period_in.factor),
      alpha = 0.5
    ) +
    geom_smooth(
      aes(
        x = as.numeric(range_temp_f),
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      method = "loess", formula = y ~ x,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), labels = scales::comma) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "Average Consumption (kWh per Hour)",
      color = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# # 2. For Average Consumption by Temperature Range selected
# ## Note:
# ## During the treatment period, there were more warmer and more colder days.
# ## Those days may be unnecessary for further analysis because they have no
# ## counterfactual observations.

# # 2.1. For Control Group
plot_avg.kwh_temperature_control_selected <-
  ggplot(
    data = dt_avg.kwh_temperature[
      is_treated_r == FALSE & is_within.temperature.range == TRUE
    ]
  ) +
    geom_point(
      aes(
        x = range_temp_f_selected,
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      alpha = 0.5
    ) +
    geom_smooth(
      aes(
        x = as.numeric(range_temp_f_selected),
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      method = "loess", formula = y ~ x,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), labels = scales::comma) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "Average Consumption (kWh per Hour)",
      color = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# # 2.2. For Treatment Group
plot_avg.kwh_temperature_treatment_selected <-
  ggplot(
    data = dt_avg.kwh_temperature[
      is_treated_r == TRUE & is_within.temperature.range == TRUE
    ]
  ) +
    geom_point(
      aes(
        x = range_temp_f_selected,
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      alpha = 0.5
    ) +
    geom_smooth(
      aes(
        x = as.numeric(range_temp_f_selected),
        y = kwh,
        color = is_treatment.period_in.factor
      ),
      method = "loess", formula = y ~ x,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), labels = scales::comma) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "Average Consumption (kWh per Hour)",
      color = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# ------- Create ggplot objects: W.R.T. (Actual) Consumption -------
# # 1. Consumption over temperature
# # 1.1. Scatter Plots
# # 1.1.1. For Control Group
plot_kwh_temperature_scatter_control <-
  ggplot(data = dt_metering_e[is_treated_r == FALSE]) +
    geom_point(
      aes(x = temp_f, y = kwh, color = is_treatment.period_in.factor),
      size = 1, alpha = 0.2
    ) +
    facet_wrap(. ~ interval_hour + is_treatment.period_in.factor, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Periods"
    ) +
    scale_x_continuous(breaks = seq(10, 75, by = 10)) +
    scale_y_continuous(breaks = seq(0, 35, by = 10), labels = scales::comma) +
    plot.options
# # 1.1.2. For Treatment Group
plot_kwh_temperature_scatter_treatment <-
  ggplot(data = dt_metering_e[is_treated_r == TRUE]) +
    geom_point(
      aes(x = temp_f, y = kwh, color = is_treatment.period_in.factor),
      size = 1, alpha = 0.2
    ) +
    facet_wrap(. ~ interval_hour + is_treatment.period_in.factor, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Periods"
    ) +
    scale_x_continuous(breaks = seq(10, 75, by = 10)) +
    scale_y_continuous(breaks = seq(0, 35, by = 10), labels = scales::comma) +
    plot.options

# # 1.2. Plots of Smoothed Conditional Mean
# # 1.2.1. For Control Group
plot_kwh_temperature_smooth_control <-
  ggplot(data = dt_metering_e[is_treated_r == FALSE]) +
    geom_smooth(
      aes(x = temp_f, y = kwh, color = is_treatment.period_in.factor),
      method = "loess", formula = y ~ x, se = FALSE,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour + is_treatment.period_in.factor, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Periods"
    ) +
    scale_x_continuous(breaks = seq(10, 75, by = 10)) +
    scale_y_continuous(breaks = seq(0, 5, by = 0.5), labels = scales::comma) +
    plot.options
# # 1.2.2. For Treatment Group
plot_kwh_temperature_smooth_treatment <-
  ggplot(data = dt_metering_e[is_treated_r == TRUE]) +
    geom_smooth(
      aes(x = temp_f, y = kwh, color = is_treatment.period_in.factor),
      method = "loess", formula = y ~ x, se = FALSE,
      lwd = 0.7, alpha = 0.3
    ) +
    facet_wrap(. ~ interval_hour + is_treatment.period_in.factor, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Periods"
    ) +
    scale_x_continuous(breaks = seq(10, 75, by = 10)) +
    scale_y_continuous(breaks = seq(0, 5, by = 0.5), labels = scales::comma) +
    plot.options


# # 2. Percentiles of Consumption by Temperature Range
# # 2.1. For Control Group
plot_percentiles.of.kwh_temperature_control <-
  ggplot(data = dt_avg.kwh_percentile[is_treated_r == FALSE]) +
    geom_line(
      aes(
        x = range_temp_f, y = kwh,
        group =
          interaction(range_percentile, is_treatment.period_in.factor),
        color = range_percentile,
        linetype = is_treatment.period_in.factor
      )
    ) +
    geom_point(
      aes(
        x = range_temp_f, y = kwh,
        color = range_percentile,
        shape = is_treatment.period_in.factor
      )
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    scale_color_brewer(palette = "Spectral") +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    scale_shape_manual(values = c(15, 16)) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Percentiles",
      shape = "Periods",
      linetype = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# # 2.2. For Treatment Group
plot_percentiles.of.kwh_temperature_treatment <-
  ggplot(data = dt_avg.kwh_percentile[is_treated_r == TRUE]) +
    geom_line(
      aes(
        x = range_temp_f, y = kwh,
        group =
          interaction(range_percentile, is_treatment.period_in.factor),
        color = range_percentile,
        linetype = is_treatment.period_in.factor
      )
    ) +
    geom_point(
      aes(
        x = range_temp_f, y = kwh,
        color = range_percentile,
        shape = is_treatment.period_in.factor
      )
    ) +
    scale_color_brewer(palette = "Spectral") +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    scale_shape_manual(values = c(15, 16)) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "kWh per Hour",
      color = "Percentiles",
      shape = "Periods",
      linetype = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# ------- Create ggplot objects: Others -------
# # 1. For the Distribution of Ranges of Temperature
plot_histogram_temperature <-
  ggplot() +
    geom_bar(
      data = dt_metering_e[
        7 <= month(date),
        .N,
        by = .(date, interval_hour, range_temp_f, is_treatment.period_in.factor)
      ],
      aes(range_temp_f, fill = is_treatment.period_in.factor),
      alpha = 0.5, position = "dodge"
    ) +
    facet_wrap(. ~ interval_hour, nrow = 6) +
    labs(
      x = TeX(r'(Ranges of Temperature $(\degree F)$)'),
      y = "Number of Days",
      fill = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# ------------------------------------------------------------------------------
# Save ggplot objects in PNG format
# ------------------------------------------------------------------------------
# ------- Save plots created above in PNG format -------
# # 1. Plots regarding Average Consumption over Temperature
# # 1.1. For all Temperature Ranges
# # 1.1.1. For Control Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Temperature-Level_Electricity_Control-Group.png",
    sep = "/"
  ),
  plot_avg.kwh_temperature_control,
  width = 70, height = 50, units = "cm"
)
# # 1.1.2. For Treatment Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Temperature-Level_Electricity_Treatment-Group.png",
    sep = "/"
  ),
  plot_avg.kwh_temperature_treatment,
  width = 70, height = 50, units = "cm"
)

# # 1.2. For Temperature Ranges selected
# # 1.2.1. For Control Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste(
      "CER_Average-Consumption_Selected-Temperature-Level",
      "Electricity_Control-Group.png",
      sep = "_"
    ),
    sep = "/"
  ),
  plot_avg.kwh_temperature_control_selected,
  width = 70, height = 50, units = "cm"
)
# # 1.2.2. For Treatment Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste(
      "CER_Average-Consumption_Selected-Temperature-Level",
      "Electricity_Treatment-Group.png",
      sep = "_"
    ),
    sep = "/"
  ),
  plot_avg.kwh_temperature_treatment_selected,
  width = 70, height = 50, units = "cm"
)


# # 2. Plots regarding Consumption over Temperature
# # 2.1. Scatter Plots
# # 2.1.1. For Control Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Consumption-over-Temperature_Electricity_Scatter_Control-Group.png",
    sep = "/"
  ),
  plot_kwh_temperature_scatter_control,
  width = 100, height = 50, units = "cm"
)
# # 2.1.2. For Treatment Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Consumption-over-Temperature_Electricity_Scatter_Treatment-Group.png",
    sep = "/"
  ),
  plot_kwh_temperature_scatter_treatment,
  width = 100, height = 50, units = "cm"
)

# # 2.2. Plots for Smoothed Conditional Mean
# # 2.2.1. For Control Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Consumption-over-Temperature_Electricity_Smooth_Control-Group.png",
    sep = "/"
  ),
  plot_kwh_temperature_smooth_control,
  width = 100, height = 50, units = "cm"
)
# # 2.2.2. For Treatment Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Consumption-over-Temperature_Electricity_Smooth_Treatment-Group.png",
    sep = "/"
  ),
  plot_kwh_temperature_smooth_treatment,
  width = 100, height = 50, units = "cm"
)

# # 2.3. Plots for Percentiles of Consumption
# # 2.3.1. For Control Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste(
      "CER_Consumption-over-Temperature_Electricity",
      "By-Percentiles_Control-Group.png",
      sep = "_"
    ),
    sep = "/"
  ),
  plot_percentiles.of.kwh_temperature_control,
  width = 100, height = 50, units = "cm"
)
# # 2.3.2. For Treatment Group
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste(
      "CER_Consumption-over-Temperature_Electricity",
      "By-Percentiles_Treatment-Group.png",
      sep = "_"
    ),
    sep = "/"
  ),
  plot_percentiles.of.kwh_temperature_treatment,
  width = 100, height = 50, units = "cm"
)


# # 3. Other Plots
# # 3.1. For the Distribution of Ranges of Temperature
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Distribution-of-Ranges-of-Temperature.png",
    sep = "/"
  ),
  plot_histogram_temperature,
  width = 70, height = 50, units = "cm"
)

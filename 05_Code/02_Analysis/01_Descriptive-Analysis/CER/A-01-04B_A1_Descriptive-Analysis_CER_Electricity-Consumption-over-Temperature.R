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
# # 1. Dates, only in October, after ending daylight saving time
DATE_AFTER.ENDING.DAYLIGHT.SAVING.TIME <- c(
  seq.Date(
    from = as.Date("2009-10-25"), to = as.Date("2009-10-31"), by = "day"
  ),
  as.Date("2010-10-31")
)


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
# #          five days in years or days after ending daylight
# #          saving in Octoer
dt_avg.kwh_by.rate.period[
  period == "Treatment" & month_in.factor == "12" & 41 < mean.temp_all_f &
    2 < kwh_per.hour,
  .N, keyby = .(date)
]
# ## Note:
# ## This demonstrates that unusual high consumption given temperature was for
# ## the last five days in years.
date_last.five.days.of.year <- dt_avg.kwh_by.rate.period[
  period == "Treatment" & month_in.factor == "12" & 41 < mean.temp_all_f &
    2 < kwh_per.hour,
  .N, keyby = .(date)
]$date
date_last.five.days.of.year <-
  date_last.five.days.of.year %>%
    as.character(.) %>%
    str_replace(., "^2010-", "2009-") %>%
    as.Date(.) %>%
    c(., date_last.five.days.of.year)
dt_avg.kwh_by.rate.period[
  ,
  is_last.five.days.of.year := date %in% date_last.five.days.of.year
]
# # 1.3.1.3. Add a column that shows whether each observation is for
# #          after-ending-daylight-saving-time days in October
dt_avg.kwh_by.rate.period[
  period == "Baseline" & month_in.factor == "10" & rate.period %like% "^Peak" &
    57 < mean.temp_all_f,
  .N, keyby = .(date)
]
# ## Note:
# ## This demonstrates that unusual high consumption given temperature was for
# ## the last four days in October.
# ## Ending daylight saving time seems to cause such high consumption.
# ## Dates on which daylight saving time ended are October 25, 2009 and
# ## October 31, 2010.
# ## Because October 25, 2009 and October 31, 2010 were Sunday, observations
# ## for those dates are not in my sample. In addition, since October 26, 2009
# ## was a holiday, my sample does not include the observation for this date.
dt_avg.kwh_by.rate.period[
  ,
  is_after.ending.daylight.saving.time_in.oct :=
    date %in% DATE_AFTER.ENDING.DAYLIGHT.SAVING.TIME
]
# # 1.3.1.4. Add a column that shows the category of each date from the
# #          perspective of exceptionally high consumption
dt_avg.kwh_by.rate.period[
  is_last.five.days.of.year == TRUE,
  category_high.consumption := "Last Five Days\nof Year"
]
dt_avg.kwh_by.rate.period[
  is_after.ending.daylight.saving.time_in.oct == TRUE,
  category_high.consumption := "Days after ending\nDaylight Saving Time"
]
dt_avg.kwh_by.rate.period[
  is.na(category_high.consumption),
  category_high.consumption := "Others"
]
levels_high.consumption <- c(
  "Last Five Days\nof Year",
  "Days after ending\nDaylight Saving Time",
  "Others"
)
dt_avg.kwh_by.rate.period[
  ,
  category_high.consumption := factor(
    category_high.consumption,
    levels = levels_high.consumption
  )
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
      data = dt_avg.kwh_by.rate.period[
        str_detect(category_high.consumption, "Others")
      ],
      aes(x = mean.temp_all_f, y = kwh_per.hour),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.2
    ) +
    geom_smooth(
      data =
        dt_avg.kwh_by.rate.period[
          str_detect(category_high.consumption, "Others") &
            rate.period %like% "(15-16)|(17-18)|(19-21)"
        ],
      aes(x = mean.temp_all_f, y = kwh_per.hour, group = season),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.2, linetype = "dotdash"
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour, color = month_in.factor
      ),
      size = 1.5, alpha = 0.8
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period[
        str_detect(category_high.consumption, "Others", negate = TRUE)
      ],
      aes(
        x = mean.temp_all_f, y = kwh_per.hour, shape = category_high.consumption
      ),
      size = 1.5, color = "black"
    ) +
    facet_grid(rate.period ~ group + period) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    scale_shape_manual(values = 0:1) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Hourly Average Consumption  (kWh per Hour)",
      color = "Month of Year",
      shape = "Exceptional\nObservations"
    ) +
    plot.options +
    theme(legend.key.size = unit(0.8, "cm"))


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
      aes(x = temp_f, y = ..density..),
      binwidth = 1.0, position = "identity",
      color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      aes(x = temp_f),
      na.rm = TRUE, adjust = 1/3,
      color = color.palette_signal[1], alpha = 0.7
    ) +
    geom_vline(
      data = dt_for.reg[
        is_in.sample_incl.control == TRUE,
        .N, by = .(date, month_in.factor, rate.period, temp_f)
      ][
        ,
        lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp_f",
        by = .(month_in.factor, rate.period)
      ],
      aes(xintercept = temp_f),
      linetype = "dotdash"
    ) +
    facet_grid(rate.period ~ month_in.factor) +
    scale_x_continuous(breaks = seq(10, 70, by = 10)) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Density",
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
  width = 50, height = 50, units = "cm"
)


# # 2. Plot regarding Temperature Distribution
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Distribution-of-Temperature_By-Rate-Period.png",
    sep = "/"
  ),
  plot_temperature_by.month,
  width = 80, height = 45, units = "cm"
)

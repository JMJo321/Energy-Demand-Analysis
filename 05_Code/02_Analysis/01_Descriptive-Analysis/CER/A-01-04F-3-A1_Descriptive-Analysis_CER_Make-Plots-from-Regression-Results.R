# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04F-3-A1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Plots from Regression Results with 30-minute
# # # interval data, Use "diff.in.temp_f" instead of "hdd_all"

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
# # 1. Path(s) from which Dataset(s)/Script(s) is(are) loaded
# # 1.1. For Regression Results
DIR_TO.LOAD_CER <- "04_CER"
FILE_TO.LOAD_CER_ESTIMATES <-
  "CER_Estimates_Response-to-Temperature_Use-Temperature-Difference.RData"
PATH_TO.LOAD_CER_ESTIMATES <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_ESTIMATES,
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
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
load(PATH_TO.LOAD_CER_ESTIMATES)


# ------------------------------------------------------------------------------
# Create DT(s), based on DTs loaded, to make Plots
# ------------------------------------------------------------------------------
# ------- Create a DT from Estimates obtained by using Subsamples: -------
# ------- By Season and Rate Period                                -------
# # 1. Create a DT
dt_estimates_by.season.and.rate.period <- rbind(
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_i,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_i.d,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i + d"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_iw.d,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i-BY-w + d"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_iw.dw,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_iw.dw.m,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_iw.dw.mp,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-p"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response_using.temp.diff_30min_by.season.and.rate.period_iw.dw.mpw,
    idcol = "season.and.rate.period"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w"] %>%
    .[!is.na(estimate)]
)


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add columns that show season and rate period, respectively
levels_season <- c("Warm", "Cold")
levels_rate.period <- c(
  "Night (23-7)", "Day: Pre-Peak (8-16)",
  "Peak (17-18)", "Day: Post-Peak (19-22)"
)
dt_estimates_by.season.and.rate.period[
  ,
  `:=` (
    season = (
      str_extract(season.and.rate.period, "(warm)|(cold)") %>%
        str_to_title(.) %>%
        factor(., levels = levels_season)
    ),
    rate.period = (
      str_extract(season.and.rate.period, "(night.+)|(day.+)|(peak.+)") %>%
        str_to_title(.) %>%
        factor(., levels = levels_rate.period)
    )
  )
]
# # 2.1.2. Add columns that show interval-related information
dt_estimates_by.season.and.rate.period[
  ,
  interval_30min := (
    str_extract(term, "[0-9]+?TRUE$") %>%
      str_replace(., "TRUE", "") %>%
      as.integer(.)
  )
]
dt_estimates_by.season.and.rate.period <- merge(
  x = dt_estimates_by.season.and.rate.period,
  y = data.table(
    interval_30min = seq(1, 48, by = 1),
    hour.of.day = rep(seq(0, 23, by = 1), each = 2),
    adjustment.for.plot = rep(c(0.25, 0.75), times = 24)
  ),
  by = "interval_30min",
  all.x = TRUE
)
dt_estimates_by.season.and.rate.period[
  ,
  hour.of.day_for.plot := hour.of.day + adjustment.for.plot
]
# # 2.1.3. Add a column that show whether a point estimate is significant or not
dt_estimates_by.season.and.rate.period[
  ,
  is_significant := !(conf.low <= 0 & 0 <= conf.high)
]

# # 2.2. Sort Observations
keys_season.and.rate.period <- c("season", "interval_30min", "model", "term")
setkeyv(dt_estimates_by.season.and.rate.period, keys_season.and.rate.period)


# ------- Make a DT that contains Simulation Results -------
# # 1. Create a DT that includes Estimates
# # 1.1. Make a temporary DT that will be used later
intervals <- seq(1, 48, by = 1)
hdds <- seq(0, 36, by = 2)
# ## Note:
# ## "36" is the maximum HDDs in cold season.
tmp_dt_template <- data.table(
  interval_30min = rep(intervals, each = length(hdds)),
  hdd = rep(hdds, times = length(intervals))
)

# # 1.2. Create DTs that contain Estimates
# # 1.2.1. DT that includes estimates for warm season
dt_simulation_by.season.and.rate.period_warm <- merge(
  x = tmp_dt_template,
  y = dt_estimates_by.season.and.rate.period[
    season == "Warm" &
      model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
      str_detect(term, "^is_treatment.and.post"),
    .(
      season, estimate, rate.period, interval_30min, hour.of.day,
      is_significant
    )
  ],
  by = "interval_30min",
  all.x = TRUE
) %>%
  merge(
    x = .,
    y = dt_estimates_by.season.and.rate.period[
      season == "Warm" &
        model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
        str_detect(term, "diff.in.temp_f:is_treatment.and.post"),
      .(estimate, interval_30min, is_significant)
    ],
    by = "interval_30min",
    all.x = TRUE
  ) %>%
  setnames(
    .,
    old = c(
      "estimate.x", "estimate.y", "is_significant.x", "is_significant.y"
    ),
    new = c(
      "reduction", "slope", "is_significant_reduction", "is_significant_slope"
    )
  )
# # 1.2.2. DT that includes estimates for cold season
dt_simulation_by.season.and.rate.period_cold <- merge(
  x = tmp_dt_template,
  y = dt_estimates_by.season.and.rate.period[
    season == "Cold" &
      model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
      str_detect(term, "^is_treatment.and.post"),
    .(
      season, estimate, rate.period, interval_30min, hour.of.day,
      is_significant
    )
  ],
  by = "interval_30min",
  all.x = TRUE
) %>%
  merge(
    x = .,
    y = dt_estimates_by.season.and.rate.period[
      season == "Cold" &
        model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
        str_detect(term, "diff.in.temp_f:is_treatment.and.post"),
      .(estimate, interval_30min, is_significant)
    ],
    by = "interval_30min",
    all.x = TRUE
  ) %>%
  setnames(
    .,
    old = c(
      "estimate.x", "estimate.y", "is_significant.x", "is_significant.y"
    ),
    new = c(
      "reduction", "slope", "is_significant_reduction", "is_significant_slope"
    )
  )
# # 1.2.3. Bind the two DTs
dt_simulation_by.season.and.rate.period <- rbind(
  dt_simulation_by.season.and.rate.period_warm[hdd <= 24],
  dt_simulation_by.season.and.rate.period_cold
)
# ## Note:
# ## Each season has different HDD ranges. In the case of the sample used in
# ## estimation, the maximum HDDs for warm season are 24.


# # 2. Modify the DT created above
# # 2.1. Add columns
dt_simulation_by.season.and.rate.period[
  interval_30min %% 2 == 0,
  half.hour := "First Half Hour"
]
dt_simulation_by.season.and.rate.period[
  is.na(half.hour),
  half.hour := "Second Half Hour"
]

dt_simulation_by.season.and.rate.period[
  ,
  `:=` (
    prediction = reduction + (hdd * slope),
    hour.of.day = factor(hour.of.day),
    interval_30min = factor(interval_30min),
    category = paste(
      as.character(is_significant_reduction),
      as.character(is_significant_slope),
      sep = "-"
    ) %>%
      factor(.)
  )
]


# ------------------------------------------------------------------------------
# Make Plots
# ------------------------------------------------------------------------------
# ------- Create object(s) that will be used to make plots -------
# # 1. Set Common Plot Options
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)

# # 2. Create Color Palette(s)
color.palette_signal <- unikn::usecol(pal_signal, n = 4)


# ------- Create Plots: with Estimates from Subsamples constructed based -------
# ------- on Season and Rate Period                                      -------
# # 1. For estimates showing reduction in electricity consumption
plot_by.season.and.rate.period_reduction <-
  ggplot(
    data = dt_estimates_by.season.and.rate.period[
      str_detect(term, "^is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_errorbar(
      aes(x = hour.of.day_for.plot, ymin = conf.low, ymax = conf.high),
      width = 0.15
    ) +
    geom_point(
      aes(x = hour.of.day_for.plot, y = estimate, shape = is_significant),
      fill = color.palette_signal[1]
    ) +
    facet_grid(model ~ season) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect: Reduction  (kWh per 30-Minute)",
      shape = "Significant?",
      caption = paste0(
        "Note: i - Household, d - Day of Week, w - 30-Minute Interval, ",
        "m - Month of Year, and p - Rate Period"
      )
    ) +
    plot.options


# # 2. For estimates showing the slope of HDDs
plot_by.season.and.rate.period_slope <-
  ggplot(
    data = dt_estimates_by.season.and.rate.period[
      str_detect(term, "diff.in.temp_f:is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_errorbar(
      aes(x = hour.of.day_for.plot, ymin = conf.low, ymax = conf.high),
      width = 0.15
    ) +
    geom_point(
      aes(x = hour.of.day_for.plot, y = estimate, shape = is_significant),
      fill = color.palette_signal[1]
    ) +
    facet_grid(model ~ season) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect: Change in Slope  (kWh per 30-Minute)",
      shape = "Significant?",
      caption = paste0(
        "Note: i - Household, d - Day of Week, w - 30-Minute Interval, ",
        "m - Month of Year, and p - Rate Period"
      )
    ) +
    plot.options


# ------- Create Plots: Simulation Results -------
plot_simulation_by.season.and.rate.period <-
  ggplot(data = dt_simulation_by.season.and.rate.period) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_point(
      aes(
        x = hdd, y = prediction,
        color = rate.period, shape = is_significant_reduction
      ),
      size = 2.5
    ) +
    geom_line(
      aes(
        x = hdd, y = prediction,
        color = rate.period, linetype = is_significant_slope
      ),
      lwd = 1
    ) +
    facet_grid(hour.of.day ~ season + half.hour) +
    scale_color_manual(values = color.palette_signal) +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c("dotted", "solid")) +
    labs(
      x = "HDDs",
      y = "Predicted Consumption  (kWh per 30-Minute)",
      color = "Rate Periods",
      shape = "Significant?: Reduction",
      linetype = "Significant?: Slope"
    ) +
    plot.options

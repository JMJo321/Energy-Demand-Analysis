# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-4
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Plots by using the DTs that include estimates
# # extracted from regression results with 30-minute interval data

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(latex2exp)
library(gridExtra)
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
FILE_TO.LOAD_CER_ESTIMATES <- "CER_Estimates_Average-Treatment-Effect.RData"
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
# ------- Create a DT from Estimates obtained by using Whole Sample -------
# # 1. Create a DT
dt_with.clustered.ses <-
  rbindlist(estimates_avg.effect_30min_with.clustered.ses, idcol = "model")
dt_without.clustered.ses <-
  rbindlist(estimates_avg.effect_30min_without.clustered.ses, idcol = "model")
dt_by.clustered.ses <- rbind(
  dt_with.clustered.ses[, category := "With Clustered SEs"],
  dt_without.clustered.ses[, category := "Without Clustered SEs"]
)


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add a column that includes model descriptions
dt_by.clustered.ses[
  str_detect(model, "(_i_)|(_i$)"),
  desc := "FEs: ID"
]
dt_by.clustered.ses[
  str_detect(model, "(_i.d_)|(_i.d$)"),
  desc := "FEs: ID + Day of Week"
]
dt_by.clustered.ses[
  str_detect(model, "(_iw.d_)|(_iw.d$)"),
  desc := "FEs: ID-BY-30-Minute-Interval + Day of Week"
]
dt_by.clustered.ses[
  str_detect(model, "(_iw.dw_)|(_iw.dw$)"),
  desc := "FEs: ID-BY-30-Minute-Interval + Day-of-Week-BY-30-Minute-Interval"
]
dt_by.clustered.ses[
  str_detect(model, "(_iw.dw.m_)|(_iw.dw.m$)"),
  desc := paste(
    "FEs: ID-BY-30-Minute-Interval",
    "Day-of-Week-BY-30-Minute-Interval",
    "Month-of-Year",
    sep = " + "
  )
]
# # 2.1.2. Add columns that show interval-related information
dt_by.clustered.ses[
  ,
  interval_30min := (
    str_extract(term, "[0-9]+?TRUE$") %>%
      str_replace(., "TRUE", "") %>%
      as.integer(.)
  )
]
dt_by.clustered.ses[
  ,
  `:=` (
    hour.of.day = (
      seq(0, 23, by = 1) %>% rep(., each = 2) %>% rep(., times = 10)
    ),
    adjustment.for.plot = (
      c(0.25, 0.75) %>% rep(., times = 24 * 10)
    )
  )
]
dt_by.clustered.ses[, hour.of.day_for.plot := hour.of.day + adjustment.for.plot]
# # 2.1.3. Add a column that show whether a point estimate is significant or not
dt_by.clustered.ses[, is_significant := !(conf.low <= 0 & 0 <= conf.high)]


# ------- Create a DT from Estimates obtained by using Subsample: -------
# ------- By Rate Period                                          -------
# # 1. Create a DT
dt_by.rate.period_night <-
  rbindlist(
    estimates_avg.effect_30min_by.rate.period_night, idcol = "model"
  ) %>%
    .[!is.na(estimate)]
dt_by.rate.period_day <-
  rbindlist(
    estimates_avg.effect_30min_by.rate.period_day, idcol = "model"
  ) %>%
    .[!is.na(estimate)]
dt_by.rate.period_peak <-
  rbindlist(
    estimates_avg.effect_30min_by.rate.period_peak, idcol = "model"
  ) %>%
    .[!is.na(estimate)]
dt_by.rate.period <- rbind(
  dt_by.rate.period_night[, rate.period := "Night"],
  dt_by.rate.period_day[, rate.period := "Day"],
  dt_by.rate.period_peak[, rate.period := "Peak"]
)


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add a column that shows model descriptions
dt_by.rate.period[
  str_detect(model, "(_i_)|(_i$)"),
  desc := "FEs: ID"
]
dt_by.rate.period[
  str_detect(model, "(_i.d_)|(_i.d$)"),
  desc := "FEs: ID + Day of Week"
]
dt_by.rate.period[
  str_detect(model, "(_iw.d_)|(_iw.d$)"),
  desc := "FEs: ID-BY-30-Minute-Interval + Day of Week"
]
dt_by.rate.period[
  str_detect(model, "(_iw.dw_)|(_iw.dw$)"),
  desc := "FEs: ID-BY-30-Minute-Interval + Day-of-Week-BY-30-Minute-Interval"
]
dt_by.rate.period[
  str_detect(model, "(_iw.dw.m_)|(_iw.dw.m$)"),
  desc := paste(
    "FEs: ID-BY-30-Minute-Interval",
    "Day-of-Week-BY-30-Minute-Interval",
    "Month-of-Year", sep = " + "
  )
]
# # 2.1.2. Add columns that show interval-related information
dt_by.rate.period[
  ,
  interval_30min := (
    str_extract(term, "[0-9]+?TRUE$") %>%
      str_replace(., "TRUE", "") %>%
      as.integer(.)
  )
]
keys_rate.period <- c("model", "interval_30min")
setkeyv(dt_by.rate.period, keys_rate.period)
# ## Note:
# ## This work is necessary to insert interval-related columns.
dt_by.rate.period[
  ,
  `:=` (
    hour.of.day = (
      seq(0, 23, by = 1) %>% rep(., each = 2) %>% rep(., times = 5)
    ),
    adjustment.for.plot = (
      c(0.25, 0.75) %>% rep(., times = 24 * 5)
    )
  )
]
dt_by.rate.period[, hour.of.day_for.plot := hour.of.day + adjustment.for.plot]
# # 2.1.3. Add a column that show whether a point estimate is significant or not
dt_by.rate.period[, is_significant := !(conf.low <= 0 & 0 <= conf.high)]

# # 2.2. Convert data type from character to factor
levels_rate.period <- c("Night", "Day", "Peak")
dt_by.rate.period[
  ,
  rate.period := factor(rate.period, levels = levels_rate.period)
]


# ------- Create a DT from Estimates obtained by using Subsample: -------
# ------- By Season and Rate Period                               -------
# # 1. Create a DT
dt_by.season.and.rate.period <-
  rbindlist(
    estimates_avg.effect_30min_by.season.and.rate.period,
    idcol = "model"
  ) %>% .[!is.na(estimate)]


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add columns that show season and rate period, respectively
levels_season <- c("Warm", "Cold")
dt_by.season.and.rate.period[
  ,
  `:=` (
    season = (
      str_extract(model, "(warm)|(cold)") %>%
        str_to_title(.) %>%
        factor(., levels = levels_season)
    ),
    rate.period = (
      str_extract(model, "(night)|(day)|(peak)") %>%
        str_to_title(.) %>%
        factor(., levels = levels_rate.period)
    )
  )
]
# # 2.1.2. Add columns that show interval-related information
dt_by.season.and.rate.period[
  ,
  interval_30min := (
    str_extract(term, "[0-9]+?TRUE$") %>%
      str_replace(., "TRUE", "") %>%
      as.integer(.)
  )
]
keys_season.and.rate.period <- c("season", "interval_30min")
setkeyv(dt_by.season.and.rate.period, keys_season.and.rate.period)
dt_by.season.and.rate.period[
  ,
  `:=` (
    hour.of.day = (
      seq(0, 23, by = 1) %>% rep(., each = 2) %>% rep(., times = 2)
    ),
    adjustment.for.plot = (
      c(0.25, 0.75) %>% rep(., times = 24 * 2)
    )
  )
]
dt_by.season.and.rate.period[
  ,
  hour.of.day_for.plot := hour.of.day + adjustment.for.plot
]
# # 2.1.3. Add a column that show whether a point estimate is significant or not
dt_by.season.and.rate.period[
  ,
  is_significant := !(conf.low <= 0 & 0 <= conf.high)
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
color.palette_signal <- unikn::usecol(pal_signal, n = 3)


# ------- Create Plots: with Estimates from Whole Sample -------
# # 1. For a Plot that includes All Models
plot_whole <-
  ggplot(data = dt_by.clustered.ses) +
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
    facet_wrap(~ desc + category, ncol = 2) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect  (kWh per 30-Minute)",
      shape = "Significant?"
    ) +
    plot.options


# # 2. For a Plot that includes Models with Complex FEs only
# # 2.1. For Comparison between models with clustered SEs and those without
# #      clustered SEs
plot_whole_complex.fes <-
  ggplot(
    data = dt_by.clustered.ses[
      str_detect(desc, "(^FEs: ID$)|(^FEs: ID )", negate = TRUE)
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
    facet_wrap(~ desc + category, ncol = 2) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect  (kWh per 30-Minute)",
      shape = "Significant?"
    ) +
    plot.options

# # 2.2. For comparision against estimates obtained by using subsample based on
# #      rate period
plot_whole_complex.fes_clustered.ses <-
  ggplot(
    data = dt_by.clustered.ses[
      str_detect(desc, "(^FEs: ID$)|(^FEs: ID )", negate = TRUE) &
        str_detect(category, "Without", negate = TRUE)
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
    facet_wrap(~ desc, ncol = 1) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_y_continuous(
      breaks = seq(-0.15, 0.05, by = 0.05), limits = c(-0.16, 0.05),
      labels = scales::comma
    ) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect  (kWh per 30-Minute)",
      subtitle = "Panel A: By using thw Whole Sample"
    ) +
    plot.options +
    guides(shape = "none")


# ------- Create Plots: with Estimates from Subsamples constructed based -------
# ------- on Rate Period                                                 -------
# # 1. For a Plot that includes all Models
plot_by.rate.period <-
  ggplot(data = dt_by.rate.period) +
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
    facet_wrap(. ~ desc, ncol = 1) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect  (kWh per 30-Minute)",
      shape = "Significant?"
    ) +
    plot.options


# # 2. For a Plot that includes Models with Complex FEs only
plot_by.rate.period_complex.fes <-
  ggplot(
    data = dt_by.rate.period[
      str_detect(desc, "(^FEs: ID$)|(^FEs: ID )", negate = TRUE)
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
    facet_wrap(. ~ desc, ncol = 1) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_y_continuous(
      breaks = seq(-0.20, 0.05, by = 0.05), limits = c(-0.20, 0.05),
      labels = scales::comma
    ) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "",
      shape = "Significant?",
      subtitle = "Panel B: By subsetting the Whole Sample based on Rate Period"
    ) +
    plot.options


# ------- Create Plots: with Estimates from Subsamples constructed based -------
# ------- on Season and Rate Period                                      -------
plot_by.season.and.rate.period <-
  ggplot(
    data = dt_by.season.and.rate.period
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
    facet_grid(season ~ .) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Hour of Day",
      y = "Treatment Effect  (kWh per 30-Minute)",
      shape = "Significant?"
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save Plots created above in PNG Format
# ------------------------------------------------------------------------------
# ------- Save Plots created above -------
# # 1. Plot(s) of Estimates obtained from Whole Sample
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Treatment-Effect_Whole-Sample.png",
    sep = "/"
  ),
  plot_whole,
  width = 60, height = 40, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Treatment-Effect_Whole-Sample_Models-with-Complex-FEs-only.png",
    sep = "/"
  ),
  plot_whole_complex.fes,
  width = 60, height = 35, units = "cm"
)

# # 2. Plot(s) of Estimates obtained from Subsample constructed based on Rate
# #    Period
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Treatment-Effect_Estimation-by-Rate-Period.png",
    sep = "/"
  ),
  plot_by.rate.period,
  width = 30, height = 35, units = "cm"
)

# # 3. Plot(s) of Estimates obtained from Subsample constructed based on Season
# #    and Rate Period
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Treatment-Effect_Estimation-by-Season-and-Rate-Period.png",
    sep = "/"
  ),
  plot_by.season.and.rate.period,
  width = 35, height = 25, units = "cm"
)

# # 4. Plot(s) for Comparison
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Treatment-Effect_Whole-Sample-vs-Subsample-by-Rate-Period.png",
    sep = "/"
  ),
  grid.arrange(
    plot_whole_complex.fes_clustered.ses,
    plot_by.rate.period_complex.fes,
    nrow = 1
  ),
  width = 60, height = 30, units = "cm"
)

# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04F-3
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Plots from Regression Results with 30-minute
# # interval data

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
  "CER_Estimates_Rate-Period-Level-Response-to-Temperature.RData"
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
dt_estimates_by.season <- rbind(
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_i,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_i.d,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i + d"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_iw.d,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i-BY-w + d"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_iw.dw,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_iw.dw.m,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_iw.dw.mp,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-p"] %>%
    .[!is.na(estimate)],
  rbindlist(
    estimates_temp.response.by.period_30min_by.season_iw.dw.mpw,
    idcol = "season"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w"] %>%
    .[!is.na(estimate)]
)


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add columns that show season and rate period, respectively
levels_season <- c("Warm", "Cold", "Both")
levels_rate.period <- list(
  night = "Night (23-7)",
  day_pre.peak = "Day: Pre-Peak (8-16)",
  peak = "Peak (17-18)",
  day_post.peak = "Day: Post-Peak (19-22)"
)
dt_estimates_by.season[
  ,
  `:=` (
    season = (
      season %>%
        str_to_title(.) %>%
        factor(., levels = levels_season)
    ),
    tmp_rate.period = (
      str_extract(term, "(night.+)|(day.+)|(peak.+)") %>%
        str_replace(., "TRUE$", "")
    )
  )
]
dt_estimates_by.season[
  ,
  rate.period := (
    lapply(tmp_rate.period, function (x) levels_rate.period[[x]]) %>%
      as.character(.) %>%
      factor(., levels = levels_rate.period)
  )
]
dt_estimates_by.season[, tmp_rate.period := NULL]
# # 2.1.2. Add a column that show whether a point estimate is significant or not
dt_estimates_by.season[
  ,
  is_significant := !(conf.low <= 0 & 0 <= conf.high)
]

# # 2.2. Sort Observations
keys_season <- c("season", "model", "term")
setkeyv(dt_estimates_by.season, keys_season)


# ------- Make a DT that contains Simulation Results -------
# # 1. Create a DT that includes Estimates
# # 1.1. Make a temporary DT that will be used later
rate.periods <- levels_rate.period %>% as.character(.)
hdds <- seq(0, 36, by = 2)
# ## Note:
# ## "36" is the maximum HDDs in cold season.
tmp_dt_template <- data.table(
  rate.period = rep(rate.periods, each = length(hdds)),
  hdd = rep(hdds, times = length(rate.periods))
)

# # 1.2. Create DTs that contain Estimates
# # 1.2.1. Create DTs
seasons <-
  dt_estimates_by.season[, .N, by = .(season)]$season %>% as.character(.)
for (season_ in seasons) {
  tmp_dt.name <- paste0("dt_simulation_by.season_", tolower(season_))
  assign(
    tmp_dt.name,
    (
      merge(
        x = tmp_dt_template,
        y = dt_estimates_by.season[
          season == season_ &
            model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
            str_detect(term, "^is_treatment.and.post"),
          .(season, estimate, rate.period, is_significant)
        ],
        by = "rate.period",
        all.x = TRUE
      ) %>%
        merge(
          x = .,
          y = dt_estimates_by.season[
            season == season_ &
              model == "FEs: i-BY-w + d-BY-w + m-BY-p-BY-w" &
              str_detect(term, "hdd_all:is_treatment.and.post"),
            .(estimate, rate.period, is_significant)
          ],
          by = "rate.period",
          all.x = TRUE
        ) %>%
        setnames(
          .,
          old = c(
            "estimate.x", "estimate.y", "is_significant.x", "is_significant.y"
          ),
          new = c(
            "reduction", "slope",
            "is_significant_reduction", "is_significant_slope"
          )
        )
    )
  )
}
# # 1.2.3. Bind the two DTs
dt_simulation_by.season <- rbind(
  dt_simulation_by.season_warm[hdd <= 24],
  dt_simulation_by.season_cold,
  dt_simulation_by.season_both
)
# ## Note:
# ## Each season has different HDD ranges. In the case of the sample used in
# ## estimation, the maximum HDDs for warm season are 24.


# # 2. Modify the DT created above
# # 2.1. Add columns
dt_simulation_by.season[
  ,
  `:=` (
    rate.period = factor(rate.period, levels = levels_rate.period),
    prediction = reduction + (hdd * slope)
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
plot_by.season_reduction <-
  ggplot(
    data = dt_estimates_by.season[
      str_detect(term, "^is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(x = rate.period, ymin = conf.low, ymax = conf.high),
      width = 0.07
    ) +
    geom_point(
      aes(x = rate.period, y = estimate, shape = is_significant),
      fill = color.palette_signal[1]
    ) +
    facet_grid(model ~ season) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Rate Period",
      y = "Estimated Response to Temperature: Reduction  (kWh per 30-Minute)",
      shape = "Significant?",
      caption = paste0(
        "Note: i - Household, d - Day of Week, w - 30-Minute Interval, ",
        "m - Month of Year, and p - Rate Period"
      )
    ) +
    plot.options


# # 2. For estimates showing the slope of HDDs
plot_by.season_slope <-
  ggplot(
    data = dt_estimates_by.season[
      str_detect(term, "hdd_all:is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(x = rate.period, ymin = conf.low, ymax = conf.high),
      width = 0.07
    ) +
    geom_point(
      aes(x = rate.period, y = estimate, shape = is_significant),
      fill = color.palette_signal[1]
    ) +
    facet_grid(model ~ season) +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Rate Period",
      y = "Estimated Response to Temperature: Change in Slope  (kWh per 30-Minute)",
      shape = "Significant?",
      caption = paste0(
        "Note: i - Household, d - Day of Week, w - 30-Minute Interval, ",
        "m - Month of Year, and p - Rate Period"
      )
    ) +
    plot.options


# ------- Create Plots: Simulation Results -------
plot_simulation_by.season <-
  ggplot(data = dt_simulation_by.season) +
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
    facet_wrap(. ~ season) +
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


# ------------------------------------------------------------------------------
# Save Plots created above in PNG Format
# ------------------------------------------------------------------------------
# ------- Save Plots created above -------
# # 1. Plots that show estimates
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Rate-Period-Level-Response-to-Temperature_Reduction-only.png",
    sep = "/"
  ),
  plot_by.season_reduction,
  width = 45, height = 45, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Rate-Period-Level-Response-to-Temperature_Slope-only.png",
    sep = "/"
  ),
  plot_by.season_slope,
  width = 45, height = 45, units = "cm"
)

# # 2. Plot(s) that illustrates simulation results
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Rate-Period-Level-Response-to-Temperature_Simulation.png",
    sep = "/"
  ),
  plot_simulation_by.season,
  width = 45, height = 35, units = "cm"
)

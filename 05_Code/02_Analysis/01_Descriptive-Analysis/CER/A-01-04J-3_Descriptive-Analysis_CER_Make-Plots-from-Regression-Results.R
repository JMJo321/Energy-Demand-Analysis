# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04J-3
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Plots from Regression Results with
# # Rate-Period-Level data

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
FILE_TO.LOAD_CER_ESTIMATES <- paste0(
  "CER_Estimates_Rate-Period-Level-Response-to-Temperature_",
  "By-Tariff-and-Stimulus.RData"
)
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
# ------- Create a DT from Estimates obtained by using Subsamples:       -------
# ------- By Season and Rate Period                                      -------
# # 1. Create a DT
dt_estimates_rate.period.response_by.tariff.and.stimulus <- rbind(
  rbindlist(
    estimates_temp.response.by.period_iw,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w"],
  rbindlist(
    estimates_temp.response.by.period_iw.dp,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-p"],
  rbindlist(
    estimates_temp.response.by.period_iw.dw,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w"],
  rbindlist(
    estimates_temp.response.by.period_iw.mp,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + m-BY-p"],
  rbindlist(
    estimates_temp.response.by.period_iw.mw,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + m-BY-w"],
  rbindlist(
    estimates_temp.response.by.period_iw.dp.mp,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-p + m-BY-p"],
  rbindlist(
    estimates_temp.response.by.period_iw.dw.mw,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-w"]
) %>%
  .[
    ,
    model := factor(
      model,
      levels = c(
        "FEs: i-BY-w",
        "FEs: i-BY-w + d-BY-p",
        "FEs: i-BY-w + d-BY-w",
        "FEs: i-BY-w + m-BY-p",
        "FEs: i-BY-w + m-BY-w",
        "FEs: i-BY-w + d-BY-p + m-BY-p",
        "FEs: i-BY-w + d-BY-w + m-BY-w"
      )
    )
  ]


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add columns that show season and rate period, respectively
levels_rate.period <- list(
  night = "Night (23-7)",
  day_pre.peak = "Day: Pre-Peak (8-16)",
  peak = "Peak (17-18)",
  day_post.peak = "Day: Post-Peak (19-22)"
)
levels_tariff <- c("A", "B", "C", "D")
levels_stimulus <- c(1:4) %>% as.character(.)
levels_stimulus.desc <- list(
  `1` = "Bi-Monthly Detailed Bill",
  `2` = "Monthly Detailed Bill",
  `3` = "Bi-Monthly Detailed Bill + IHD",
  `4` = "Bi-Monthly Detailed Bill + OLR"
)
dt_estimates_rate.period.response_by.tariff.and.stimulus[
  ,
  `:=` (
    tmp_rate.period =
      str_extract(tmp_desc, "(night)|(day_pre.peak)|(peak)|(day_post.peak)"),
    tariff =
      str_extract(tmp_desc, "_[a-z]_") %>%
        str_extract(., "[a-z]") %>%
        toupper(.) %>%
        factor(., levels = levels_tariff),
    stimulus = str_extract(tmp_desc, "[0-9]") %>%
      factor(., levels = levels_stimulus)
  )
]
dt_estimates_rate.period.response_by.tariff.and.stimulus[
  ,
  rate.period := (
    lapply(tmp_rate.period, function (x) levels_rate.period[[x]]) %>%
      as.character(.) %>%
      factor(., levels = levels_rate.period)
  )
]
dt_estimates_rate.period.response_by.tariff.and.stimulus[
  ,
  stimulus_desc := (
    lapply(stimulus, function (x) levels_stimulus.desc[[x]]) %>%
      as.character(.) %>%
      factor(., levels = levels_stimulus.desc)
  )
]
dt_estimates_rate.period.response_by.tariff.and.stimulus[
  ,
  `:=` (
    tmp_desc = NULL,
    tmp_rate.period = NULL
  )
]
# # 2.1.2. Add a column that show whether a point estimate is significant or not
dt_estimates_rate.period.response_by.tariff.and.stimulus[
  ,
  is_significant := !(conf.low <= 0 & 0 <= conf.high)
]


# ------- Make a DT that contains Simulation Results -------
# # 1. Create a DT that includes Estimates
# # 1.1. Make a temporary DT that will be used later
rate.periods <-
  dt_estimates_rate.period.response_by.tariff.and.stimulus[
    ,
    .N,
    by = .(rate.period)
  ]$rate.period %>%
    as.character(.)
tariffs <-
  dt_estimates_rate.period.response_by.tariff.and.stimulus[
    ,
    .N,
    by = .(tariff)
  ]$tariff %>%
    as.character(.)
stimuli <-
  dt_estimates_rate.period.response_by.tariff.and.stimulus[
    ,
    .N,
    by = .(stimulus)
  ]$stimulus %>%
    as.character(.)
hdds <- seq(0, 36, by = 2)
# ## Note:
# ## "36" is the maximum HDDs in cold season.
tmp_dt_template <-
  expand.grid(tariffs, stimuli, rate.periods, hdds) %>%
    setDT(.)

keys <- c("tariff", "stimulus", "rate.period", "hdd")
names(tmp_dt_template) <- keys
setkeyv(tmp_dt_template, keys)


# # 1.2. Create DTs that contain Estimates
# # 1.2.1. Create DTs
dt_simulation_by.tariff.and.stimulus <- merge(
  x = tmp_dt_template,
  y = dt_estimates_rate.period.response_by.tariff.and.stimulus[
    str_detect(term, "^is_treatment.and.post") &
      model == "FEs: i-BY-w + d-BY-w + m-BY-w",
    .(tariff, stimulus, stimulus_desc, rate.period, estimate, is_significant)
  ],
  by = c("tariff", "stimulus", "rate.period"),
  all.x = TRUE
) %>%
  merge(
    x = .,
    y = dt_estimates_rate.period.response_by.tariff.and.stimulus[
      str_detect(term, "^hdd_all:is_treatment.and.post") &
        model == "FEs: i-BY-w + d-BY-w + m-BY-w",
      .(tariff, stimulus, rate.period, estimate, is_significant)
    ],
    by = c("tariff", "stimulus", "rate.period"),
    all.x = TRUE
  ) %>%
  setnames(
    .,
    old = c(
      "estimate.x", "estimate.y",
      "is_significant.x", "is_significant.y"
    ),
    new = c(
      "reduction", "slope",
      "is_significant_reduction", "is_significant_slope"
    )
  )


# # 2. Modify the DT created above
# # 2.1. Add columns
dt_simulation_by.tariff.and.stimulus[
  ,
  `:=` (
    prediction = reduction + (hdd * slope)
  )
]
# # 2.2. Change Data Types
dt_simulation_by.tariff.and.stimulus[
  ,
  `:=` (
    rate.period = factor(rate.period, levels_rate.period),
    tariff = factor(tariff, levels_tariff),
    stimulus = factor(stimulus, levels_stimulus)
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
dodge <- position_dodge(width = 0.5)

# # 2. Create Color Palette(s)
color.palette_signal <- unikn::usecol(pal_signal, n = 4)


# ------- Create Plots: with Estimates from Subsamples constructed based -------
# ------- on Season and Rate Period                                      -------
# # 1. For estimates showing reduction in electricity consumption
plot_by.tariff.and.stimulus_reduction <-
  ggplot(
    data = dt_estimates_rate.period.response_by.tariff.and.stimulus[
      str_detect(term, "^is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(
        x = rate.period, ymin = conf.low, ymax = conf.high, color = tariff
      ),
      width = 0.1,
      position = dodge
    ) +
    geom_point(
      aes(
        x = rate.period, y = estimate, color = tariff, shape = is_significant
      ),
      fill = color.palette_signal[1],
      position = dodge
    ) +
    facet_grid(model ~ stimulus_desc) +
    scale_shape_manual(values = c(1, 21)) +
    scale_color_viridis_d() +
    labs(
      x = "Rate Periods",
      y = "Estimated Response to Temperature: Reduction  (kWh per 30-Minute)",
      shape = "Significant?",
      color = "Tariffs",
      caption = paste0(
        "Note: i - Household, w - 30-Minute Interval, d - Day of Week, ",
        "p - Rate Period, and m - Month of Year."
      )
    ) +
    plot.options


# # 2. For estimates showing the slope of HDDs
plot_by.tariff.and.stimulus_slope <-
  ggplot(
    data = dt_estimates_rate.period.response_by.tariff.and.stimulus[
      str_detect(term, "hdd_all:is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(
        x = rate.period, ymin = conf.low, ymax = conf.high, color = tariff
      ),
      width = 0.1,
      position = dodge
    ) +
    geom_point(
      aes(
        x = rate.period, y = estimate, color = tariff, shape = is_significant
      ),
      fill = color.palette_signal[1],
      position = dodge
    ) +
    facet_grid(model ~ stimulus_desc) +
    scale_color_viridis_d() +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "Rate Periods",
      y = "Estimated Response to Temperature: Change in Slope  (kWh per 30-Minute)",
      shape = "Significant?",
      color = "Tariffs",
      caption = paste0(
        "Note: i - Household, w - 30-Minute Interval, d - Day of Week, ",
        "p - Rate Period, and m - Month of Year."
      )
    ) +
    plot.options


# ------- Create Plots: Simulation Results -------
plot_simulation_by.tariff.and.stimulus <-
  ggplot(
    data = dt_simulation_by.tariff.and.stimulus
    # ## Note:
    # ## Each season has different HDD ranges. In the case of the sample used
    # ## in estimation, the maximum HDDs for warm season are 24.
  ) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_point(
      aes(
        x = hdd, y = prediction,
        color = tariff, shape = is_significant_reduction
      ),
      size = 2.5
    ) +
    geom_line(
      aes(
        x = hdd, y = prediction,
        color = tariff, linetype = is_significant_slope
      ),
      lwd = 1
    ) +
    facet_grid(rate.period ~ stimulus_desc, scales = "free_y") +
    scale_color_viridis_d() +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c("dotted", "solid")) +
    labs(
      x = "HDDs",
      y = "Simulated Average Treatment Effect  (kWh per 30-Minute Interval)",
      color = "Tariffs",
      shape = "Significant?: Reduction",
      linetype = "Significant?: Slope"
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2),
      linetype = guide_legend(order = 3)
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
    paste0(
      "CER_Rate-Period-Level-Response-to-Temperature_",
      "By-Tariff-and-Stimulus_Reduction-only.png"
    ),
    sep = "/"
  ),
  plot_by.tariff.and.stimulus_reduction,
  width = 60, height = 50, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste0(
      "CER_Rate-Period-Level-Response-to-Temperature_",
      "By-Tariff-and-Stimulus_Slope-only.png"
    ),
    sep = "/"
  ),
  plot_by.tariff.and.stimulus_slope,
  width = 60, height = 50, units = "cm"
)

# # 2. Plot(s) that illustrates simulation results
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste0(
      "CER_Rate-Period-Level-Response-to-Temperature_",
      "By-Tariff-and-Stimulus_Simulation.png"
    ),
    sep = "/"
  ),
  plot_simulation_by.tariff.and.stimulus,
  width = 55, height = 40, units = "cm"
)

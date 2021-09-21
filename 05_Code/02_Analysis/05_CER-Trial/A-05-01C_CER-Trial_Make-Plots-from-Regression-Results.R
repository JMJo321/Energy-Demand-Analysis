# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-01C
# #
# > Purpose of the script(s):
# # Make Plots from Regression Results with Rate-Period-Level data

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
  "CER_Estimates_Rate-Period-Level-Treatment-Effect_By-Season-and-Tariff.RData"
PATH_TO.LOAD_CER_ESTIMATES <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_ESTIMATES,
  sep = "/"
)


# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(PATH_NOTE, "07_CER-Trials", "02_Figures", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
load(PATH_TO.LOAD_CER_ESTIMATES)

# ------- Modify the DT loaded -------
dt_estimates_treatment.effect_by.season.and.tariff[
  ,
  ref.temp_f_in.factor := factor(ref.temp_f)
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
dodge <- position_dodge(width = 0.3)

# # 2. Create Color Palette(s)
color.palette_signal <- unikn::usecol(pal_signal, n = 4)


# ------- Create Plots:                                                  -------
# ------- with Estimates from Subsamples constructed basedon Season and  -------
# ------- Rate Period                                                    -------
# # 1. For estimates showing reduction in electricity consumption
plot_by.season.and.tariff_reduction <-
  ggplot(
    data = dt_estimates_treatment.effect_by.season.and.tariff[
      str_detect(term, "^is_treatment.and.post")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(
        x = rate.period, ymin = conf.low, ymax = conf.high, color = ref.temp_f_in.factor
      ),
      width = 0.1,
      position = dodge
    ) +
    geom_point(
      aes(
        x = rate.period, y = estimate, color = ref.temp_f_in.factor, shape = is_significant
      ),
      fill = color.palette_signal[1],
      position = dodge
    ) +
    facet_grid(model ~ tariff) +
    scale_shape_manual(values = c(1, 21)) +
    # scale_color_viridis_d() +
    labs(
      x = "\nRate Periods",
      y = "Estimated Response to Temperature: Reduction  (kWh per 30-Minute)",
      shape = "Significant?",
      color = TeX(r'(Ref. Temperature $(\degree F)$)'),
      caption = paste0(
        "Note: i - Household, w - 30-Minute Interval, d - Day of Week, ",
        "p - Rate Period, and m - Month of Year."
      )
    ) +
    plot.options


# # 2. For estimates showing the slope of HDDs
plot_by.season.and.tariff_slope <-
  ggplot(
    data = dt_estimates_treatment.effect_by.season.and.tariff[
      str_detect(term, "^hdd.+is_treatment.and.post$")
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(
        x = rate.period, ymin = conf.low, ymax = conf.high, color = ref.temp_f_in.factor
      ),
      width = 0.1,
      position = dodge
    ) +
    geom_point(
      aes(
        x = rate.period, y = estimate, color = ref.temp_f_in.factor, shape = is_significant
      ),
      fill = color.palette_signal[1],
      position = dodge
    ) +
    facet_grid(model ~ tariff) +
    # scale_color_viridis_d() +
    scale_shape_manual(values = c(1, 21)) +
    labs(
      x = "\nRate Periods",
      y = "Estimated Response to Temperature: Change in Slope  (kWh per 30-Minute)",
      shape = "Significant?",
      color = TeX(r'(Ref. Temperature $(\degree F)$)'),
      caption = paste0(
        "Note: i - Household, w - 30-Minute Interval, d - Day of Week, ",
        "p - Rate Period, and m - Month of Year."
      )
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save Plots created above in PNG Format
# ------------------------------------------------------------------------------
# ------- Save Plots created above -------
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste0(
      "CER_Rate-Period-Level-Treatment-Effect_By-Season-and-Tariff_",
      "Reduction-only",
      ".png"
    ),
    sep = "/"
  ),
  plot_by.season.and.tariff_reduction,
  width = 55, height = 60, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    paste0(
      "CER_Rate-Period-Level-Treatment-Effect_By-Season-and-Tariff_Slope-only",
      ".png"
    ),
    sep = "/"
  ),
  plot_by.season.and.tariff_slope,
  width = 55, height = 60, units = "cm"
)

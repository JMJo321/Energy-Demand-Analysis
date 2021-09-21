# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-02C
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
  paste0(
    "CER_Estimates_Rate-Period-Level-Treatment-Effect_By-Season-and-Tariff_",
    "With-Rate-Changes.RData"
  )
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
dt_estimates_treatment.effect_by.season.and.tariff_w.rate.change[
  str_detect(term, "^treatment.and.post.times.rate.change"),
  category := "Non-Temperature Effect"
]
dt_estimates_treatment.effect_by.season.and.tariff_w.rate.change[
  str_detect(term, "^hdd.times.treatment.and.post.times.rate.change"),
  category := "Temperature Effect"
]

dt_estimates_treatment.effect_by.season.and.tariff_w.rate.change[
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
# # 1. For estimates
plot_treatment.effect_w.rate.change <-
  ggplot(
    data = dt_estimates_treatment.effect_by.season.and.tariff_w.rate.change[
      !is.na(category)
    ]
  ) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbar(
      aes(
        x = rate.period, ymin = conf.low, ymax = conf.high
      ),
      width = 0.05,
      position = dodge
    ) +
    geom_point(
      aes(
        x = rate.period, y = estimate, shape = is_significant
      ),
      fill = color.palette_signal[1],
      position = dodge
    ) +
    facet_wrap(. ~ category, ncol = 1, scales = "free_y") +
    scale_shape_manual(values = c(1, 21)) +
    # scale_color_viridis_d() +
    labs(
      x = "\nRate Periods",
      y = "Estimated Treatment Effect  (kWh per 30-Minute)\n",
      shape = "Significant?"
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save Plots created above in PNG Format
# ------------------------------------------------------------------------------
# ------- Save Plots created above -------
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Rate-Period-Level-Treatment-Effect_With-Rate-Changes.png",
    sep = "/"
  ),
  plot_treatment.effect_w.rate.change,
  width = 35, height = 25, units = "cm"
)

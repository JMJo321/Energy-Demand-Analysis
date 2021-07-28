# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-3
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Plots from Regression Results with Hourly Data

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
FILE_TO.LOAD_CER_RESULTS <- "CER_Regression-Results_Rate-Period.RData"
PATH_TO.LOAD_CER_RESULTS <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_RESULTS,
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
load(PATH_TO.LOAD_CER_RESULTS)


# ------------------------------------------------------------------------------
# Create DT(s) from Regression Results
# ------------------------------------------------------------------------------
# ------- Create DT(s) from Regression Results with Hourly Data -------
# # 1. Create a DT to make plots
# # 1.1. Make a DT from regression results
results <- ls()[str_detect(ls(), "^result_")]
reg.exp_subset <- "(treatment.and.post)"
dt_simulation <- setDT(NULL)
for (result in results) {
  # ## Make objects that will be used to make an object name
  model_ <- str_extract(result, "(ols)|(fes)")
  season_ <- str_extract(result, "(cold)|(warm)")
  data_ <- str_extract(result, "(avg)|(real)")
  # ## Extract estimates
  dt_estimates <-
    summary(
      get(result), robust = TRUE
    )$coefficients %>%
      data.table(., keep.rownames = TRUE)
  names(dt_estimates) <-
    c("desc", "estimate", "se", "t.value", "prob_v.value")
  dt_estimates_t.and.p <- dt_estimates[
    str_detect(desc, reg.exp_subset)
  ][
    ,
    rate.period := str_extract(desc, "(Night:.+?\\))|(Day:.+?\\))|(Peak.+?\\))")
  ]
  dt_estimates_t.and.p[str_detect(desc, "I\\(hd_"), category := 2]
  dt_estimates_t.and.p[is.na(category) & str_detect(desc, "hd_"), category := 1]
  dt_estimates_t.and.p[is.na(category), category := 0]
  # ## Compute response
  dt_simulation_by.result <- setDT(NULL)
  for (period in rate.periods) {
    tmp_dt <- data.table(hd = seq(0, 60, by = 0.5))
    estimate_0 <-
      dt_estimates_t.and.p[rate.periods == period & category == 0]$estimate
    estimate_1 <-
      dt_estimates_t.and.p[rate.periods == period & category == 1]$estimate
    estimate_2 <-
      dt_estimates_t.and.p[rate.periods == period & category == 2]$estimate
    tmp_dt[
      ,
      `:=` (
        rate.period = period,
        response = estimate_0 + hd * estimate_1 + (hd^2) * estimate_2
      )
    ]
    dt_simulation_by.result <- rbind(dt_simulation_by.result, tmp_dt)
  }
  dt_simulation_by.result[
    ,
    `:=` (model = model_, season = season_, data = data_)
  ]
  dt_simulation <- rbind(dt_simulation, dt_simulation_by.result)
}

# # 1.2. Modify the DT created above
# # 1.2.1. Add columns
dt_simulation[str_detect(rate.period, "^Night"), group_rate.period := "Night"]
dt_simulation[str_detect(rate.period, "^Day"), group_rate.period := "Day"]
dt_simulation[str_detect(rate.period, "^Peak"), group_rate.period := "Peak"]
# # 1.2.2. Convert data types
levels_rate.period <- as.character(rate.periods)
levels_model <- c("ols", "fes")
levels_season <- c("cold", "warm")
levels_data <- c("avg", "real")
levels_group_rate.period <- c("Night", "Day", "Peak")
labels_model <- c("OLS", "FEs")
labels_season <- c("Warm (7-10)", "Cold (11-12)")
labels_data <- c("Average kWh", "kWh")
dt_simulation[
  ,
  `:=` (
    rate.period = factor(rate.period, levels = levels_rate.period),
    model = factor(model, levels = levels_model, labels = labels_model),
    season = factor(season, levels = levels_season, labels = labels_season),
    data = factor(data, levels = levels_data, labels = labels_data),
    group_rate.period = factor(
      group_rate.period, levels = levels_group_rate.period
    )
  )
]


# ------------------------------------------------------------------------------
# Make Plots
# ------------------------------------------------------------------------------
# ------- Set Common Plot Options -------
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Create Plot(s) for Descriptive Analysis -------
# # 1. Create a Plot that shows Simulation Results
plot_simulation <-
  ggplot(data = dt_simulation[model == "FEs" & data == "kWh"]) +
    geom_point(
      aes(x = hd, y = response, color = rate.period, shape = group_rate.period),
      size = 1.5
    ) +
    geom_line(
      aes(x = hd, y = response, color = rate.period, group = rate.period)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(season ~ .) +
    scale_x_continuous(breaks = seq(0, 60, by = 5)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Spectral") +
    labs(
      x = "HDs",
      y = "Response with respect to Treatment",
      color = "Rate Periods",
      shape = "Group of Rate Periods"
    ) +
    plot.options


# ------- Save plots created above -------
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Simulation_Response-to-Temperature_Hourly_Electricity.png",
    sep = "/"
  ),
  plot_simulation,
  width = 40, height = 25, units = "cm"
)

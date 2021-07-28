# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04C_A1
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Estimate the Treatment Impact on
# # Household Response to Temperature by using `hdd_all`

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
library(lfe)
library(stargazer)
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
# # 1.1. For Metering Data
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.RData"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY,
  sep = "/"
)

# # 1.2. For R Script including Regression Models
FILE_TO.LOAD_CER_MODELS <- "M-Energy-Demand-Analysis_Regression-Models_CER.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_CODE,
  FILE_TO.LOAD_CER_MODELS,
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
load(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Load Script(s) -------
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Create DTs for Tables, Plots, or Regressions
# ------------------------------------------------------------------------------
# ------- Create DT(s) for Plots -------
# # 1. Create a DT that includes Household-level Daily Average Consumption
# # 1.1. Add a column showing ranges of HDDs, which will be used to aggregate
# #      consumption
dt_for.reg[
  ,
  range_hdd := cut(hdd_all, breaks = seq(0, 48, by = 1), include.lowest = TRUE)
]

# # 1.2. Create a DT by aggregating daily consumption
dt_avg.kwh_daily <-
  dt_for.reg[   # To obtain each household's daily consumption
    is_in.sample_incl.control == TRUE,
    lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
    by = .(id, date, group, period, range_hdd)
  ][   # To compute daily average consumption
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = .(date, group, period, range_hdd)
  ]
# ## Note:
# ## Do NOT exclue `is_within.temperature.range == FALSE` because excluding
# ## observations meeting the condition distort average daily consumption.
# ## Excluding observations with `is_within.temperature.range == FALSE` will
# ## not cause any problem when I run regressions with a hourly-level sample.


# ------- Create DT(s) for Regressions -------
# # 1. Create a DT that includes Household-level Daily Average Consumption
# # 1.1. For DT including Observations of Control Group
dt_for.reg_daily_incl.control <- dt_for.reg[
  is_in.sample_incl.control == TRUE,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(
    date,
    id_in.factor,
    is_treated_r, is_treatment.period, treatment.and.post,
    mean.temp_all_f, hdd_all,
    day.of.week_in.factor, id.and.day.of.week_in.factor, month_in.factor
  )
]
# # 1.2. For DT excluding Observations of Control Group
dt_for.reg_daily_excl.control <- dt_for.reg[
  is_in.sample_excl.control == TRUE,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(
    date,
    id_in.factor,
    treatment.and.post,
    mean.temp_all_f, hdd_all,
    day.of.week_in.factor, id.and.day.of.week_in.factor, month_in.factor
  )
]


# ------------------------------------------------------------------------------
# Run Regressions
# ------------------------------------------------------------------------------
# ------- Run Regressions with OLS Models -------
# # 1. Run Regressions with Day-level Data
# # 1.1. With a sample including the control group
result_ols_daily_incl.control_linear <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_ols_daily_incl.control_linear
)
result_ols_daily_incl.control_quadratic <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_ols_daily_incl.control_quadratic
)

# # 1.2. With a sample excluding the control group
result_ols_daily_excl.control_linear <- felm(
  data = dt_for.reg_daily_excl.control,
  formula = model_ols_daily_excl.control_linear
)
result_ols_daily_excl.control_quadratic <- felm(
  data = dt_for.reg_daily_excl.control,
  formula = model_ols_daily_excl.control_quadratic
)


# ------- Run Regressions with FEs Models -------
# # 1. Run Regressions with Day-level Data
# # 1.1. With a sample including the control group
result_fes_daily_incl.control_linear <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_fes_daily_incl.control_linear
)
result_fes_daily_incl.control_linear_variation1 <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_fes_daily_incl.control_linear_variation1
)
result_fes_daily_incl.control_quadratic <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_fes_daily_incl.control_quadratic
)
result_fes_daily_incl.control_quadratic_variation1 <- felm(
  data = dt_for.reg_daily_incl.control,
  formula = model_fes_daily_incl.control_quadratic_variation1
)

# # 1.2. With a sample including the control group
result_fes_daily_excl.control_linear <- felm(
  data = dt_for.reg_daily_excl.control,
  formula = model_fes_daily_excl.control_linear
)
result_fes_daily_excl.control_quadratic <- felm(
  data = dt_for.reg_daily_excl.control,
  formula = model_fes_daily_excl.control_quadratic
)


# ------------------------------------------------------------------------------
# Create DTs from Regression Results
# ------------------------------------------------------------------------------
# ------- Create DTs from Regression Results with Daily Data -------
# # 1. Extract Estimates
# # 1.1. From results from FEs models
# # 1.1.1. From results from FEs models with the sample excluding control group
# # 1.1.1.1. Linear Model
dt_fes_daily_excl.control_linear <- summary(
  result_fes_daily_excl.control_linear, robust = TRUE
)$coefficients %>%
  data.table(., keep.rownames = TRUE)
names(dt_fes_daily_excl.control_linear) <-
  c("desc", "estimate", "se", "t.value", "prob_v.value")
# # 1.1.1.2. Quadratic Model
dt_fes_daily_excl.control_quadratic <- summary(
  result_fes_daily_excl.control_quadratic, robust = TRUE
)$coefficients %>%
  data.table(., keep.rownames = TRUE)
names(dt_fes_daily_excl.control_quadratic) <-
  c("desc", "estimate", "se", "t.value", "prob_v.value")
# # 1.1.2. From results from FEs models with the sample including control group
# # 1.1.2.1. Linear Model
dt_fes_daily_incl.control_linear <- summary(
  result_fes_daily_incl.control_linear, robust = TRUE
)$coefficients %>%
  data.table(., keep.rownames = TRUE)
names(dt_fes_daily_incl.control_linear) <-
  c("desc", "estimate", "se", "t.value", "prob_v.value")
# # 1.1.2.2. Quadratic Model
dt_fes_daily_incl.control_quadratic <- summary(
  result_fes_daily_incl.control_quadratic, robust = TRUE
)$coefficients %>%
  data.table(., keep.rownames = TRUE)
names(dt_fes_daily_incl.control_quadratic) <-
  c("desc", "estimate", "se", "t.value", "prob_v.value")


# # 2. Create DTs that include Simulation Results
# # 2.1. Simulation Results from FEs Models: Temperature Response
# # 2.1.1. From the sample excluding control group
# # 2.1.1.1. For Linear Model
dt_simulation_fes_daily_excl.control_linear <-
  data.table(hdd = seq(0, 50, by = 0.5)) %>%
    .[
      ,
      `:=` (
        model = "Linear",
        response = (
          dt_fes_daily_excl.control_linear[
            str_detect(desc, "^treatment.and.post")
          ]$estimate +
            dt_fes_daily_excl.control_linear[
              str_detect(desc, "^hdd_all:treatment.and.post")
            ]$estimate * hdd
        )
      )
    ]
# # 2.1.1.2. For Quadratic Model
dt_simulation_fes_daily_excl.control_quadratic <-
  data.table(hdd = seq(0, 50, by = 0.5)) %>%
    .[
      ,
      `:=` (
        model = "Quadratic",
        response = (
          dt_fes_daily_excl.control_quadratic[
            str_detect(desc, "^treatment.and.post")
          ]$estimate +
            dt_fes_daily_excl.control_quadratic[
              str_detect(desc, "^hdd_all:treatment.and.post")
            ]$estimate * hdd +
            dt_fes_daily_excl.control_quadratic[
              str_detect(desc, "^I.+treatment.and.postTRUE$")
            ]$estimate * hdd^2
        )
      )
    ]
# # 2.1.2. From the sample including control group
# # 2.1.2.1. For Linear Model
dt_simulation_fes_daily_incl.control_linear <-
  data.table(hdd = seq(0, 50, by = 0.5)) %>%
    .[
      ,
      `:=` (
        model = "Linear",
        response = (
          dt_fes_daily_incl.control_linear[
            str_detect(desc, "^treatment.and.post")
          ]$estimate +
            dt_fes_daily_incl.control_linear[
              str_detect(desc, "^hdd_all:treatment.and.post")
            ]$estimate * hdd
        )
      )
    ]
# # 2.1.2.2. For Quadratic Model
dt_simulation_fes_daily_incl.control_quadratic <-
  data.table(hdd = seq(0, 50, by = 0.5)) %>%
    .[
      ,
      `:=` (
        model = "Quadratic",
        response = (
          dt_fes_daily_incl.control_quadratic[
            str_detect(desc, "^treatment.and.post")
          ]$estimate +
            dt_fes_daily_incl.control_quadratic[
              str_detect(desc, "^hdd_all:treatment.and.post")
            ]$estimate * hdd +
            dt_fes_daily_incl.control_quadratic[
              str_detect(desc, "^I.+treatment.and.postTRUE$")
            ]$estimate * hdd^2
        )
      )
    ]
# # 2.1.3. Create DTs by combining DTs generated above
dt_simulation_fes_daily_excl.control <- rbind(
  dt_simulation_fes_daily_excl.control_linear,
  dt_simulation_fes_daily_excl.control_quadratic
)
dt_simulation_fes_daily_incl.control <- rbind(
  dt_simulation_fes_daily_incl.control_linear,
  dt_simulation_fes_daily_incl.control_quadratic
)
dt_simulation_fes_daily <- rbind(
  dt_simulation_fes_daily_excl.control[, category := "Excluding Control Group"],
  dt_simulation_fes_daily_incl.control[, category := "Including Control Group"]
)
# # 2.1.4. Modify the combined DT
# # 2.1.4.1. Convert data type from character to factor
levels <- c("Including Control Group", "Excluding Control Group")
dt_simulation_fes_daily[, category := factor(category, levels = levels)]


# ------------------------------------------------------------------------------
# Make Table(s) from Regression Results
# ------------------------------------------------------------------------------
# ------- Make Table(s) from Regression Results -------
# # 1. Create objects that will be used to make regression table(s)
list_results <- list(
  result_ols_daily_excl.control_linear,
  result_fes_daily_excl.control_linear,
  result_ols_daily_excl.control_quadratic,
  result_fes_daily_excl.control_quadratic,
  result_ols_daily_incl.control_linear,
  result_fes_daily_incl.control_linear,
  result_fes_daily_incl.control_linear_variation1,
  result_ols_daily_incl.control_quadratic,
  result_fes_daily_incl.control_quadratic,
  result_fes_daily_incl.control_quadratic_variation1
)
column.labels <- c(
  "Sample excluding Control Group", "Sample including Control Group"
)
column.separate <- c(4, 6)
covariate.labels <- c(
  "HDDs",
  "(HDDs)\\^2",
  "1[Treatment]",
  "1[Post]",
  "1[Treatment and Post]",
  "HDDs x 1[Treatment]",
  "(HDDs)\\^2 x 1[Treatment]",
  "HDDs x 1[Post]",
  "(HDDs)\\^2 x 1[Post]",
  "HDDs x 1[Treatment and Post]",
  "(HDDs)\\^2 x 1[Treatment and Post]",
  "(Constant)"
)
dep.var.labels <- "Daily Consumption (kWh per Day)"
add.lines <- list(
  c(
    "FEs: ID-by-Day of Week",
    "No", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes"
  ),
  c(
    "FEs: Month",
    "No", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes"
  )
)

# # 2. Print Table(s)
stargazer(
  list_results,
  type = "text",
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines
)


# ------------------------------------------------------------------------------
# Make Plots
# ------------------------------------------------------------------------------
# ------- Set Common Plot Options -------
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Create Plots for Descriptive Analysis -------
# # 1. Create a Plot that shows Simulation Results
plot_simulation_fes <-
  ggplot(data = dt_simulation_fes_daily) +
    geom_point(aes(x = hdd, y = response, color = model, shape = model)) +
    geom_line(aes(x = hdd, y = response, color = model, group = model)) +
    facet_grid(category ~ .) +
    scale_x_continuous(breaks = seq(0, 50, by = 5)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "HDDs",
      y = "Response with respect to Treatment",
      color = "Models",
      shape = "Models"
    ) +
    plot.options


# # 2. Create a Plot that shows Daily Average Consumption
plot_avg.kwh_daily <-
  ggplot(data = dt_avg.kwh_daily[, range_hdd := factor(range_hdd)]) +
    geom_jitter(
      aes(x = range_hdd, y = kwh, color = period),
      alpha = 0.3
    ) +
    geom_smooth(
      aes(x = as.numeric(range_hdd), y = kwh, color = period),
      method = "loess", formula = y ~ x,
      alpha = 0.3
    ) +
    facet_grid(group ~ .) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Ranges of HDDs",
      y = "Daily Average Consumption (kWh per Day)",
      color = "Periods"
    ) +
    plot.options +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# ------- Export Plots created above in PNG Format -------
# # 1. For Daily Average Consumption
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Simulation_Response-to-Temperature_Daily_Electricity_Using-HDD-All.png",
    sep = "/"
  ),
  plot_simulation_fes,
  width = 40, height = 30, units = "cm"
)

# # 2. For Simulation Results
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Daily-Average-Consumption_By-HDD_Electricity_Using-HDD-All.png",
    sep = "/"
  ),
  plot_avg.kwh_daily,
  width = 40, height = 30, units = "cm"
)

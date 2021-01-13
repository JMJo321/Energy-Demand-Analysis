# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-01A
# #
# > Purpose of the script(s)
# # : To make plots about SMUD Residential Rate Schedules.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(unikn)
library(gridExtra)
library(ggplot2)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
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
# # 1. Path(s) for Data file(s)
DIR_TO.LOAD_RRS <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RRS <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RRS <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RRS, FILE_TO.LOAD_RRS, sep = '/')

# # 2. Paths at which Plots will be saved
DIR_TO.SAVE_PLOTS <-
  paste(
    PATH_NOTE_DESCRIPTIVE.ANALYSIS,
    "03_SMUD-Residential-Rate-Schedules",
    sep = "/"
  )


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load Datasets required, and Modify them
# --------------------------------------------------
# ------- Load datasets required -------
rrs <-
  pq.to.dt(
    PATH_TO.LOAD_RRS,
    reg.ex_date = "(^date)|(^season_)",
    is_drop.index_cols = TRUE
  )
# ## Check primary keys of the DT loaded
stopifnot(rrs[, .N, by = .(date, rate_code_normalize)][N > 1, .N] == 0)


# ------- Modify the datasets -------
rrs_simplified <- rrs[rate_code_normalize %in% c("RSCH", "RSEH", "RSGH")]

cols_tier_kwh <-
  names(rrs_simplified)[
    str_detect(names(rrs_simplified), "^tier_.+_kwh$")
  ]
cols_tier_usd <-
  names(rrs_simplified)[
    str_detect(names(rrs_simplified), "^tier_.+_usd$")
  ]
cols_id.vars = c("date", "rate_code_normalize")
tmp_dt_kwh <- melt(
  rrs_simplified[, .SD, .SDcols = c(cols_id.vars, cols_tier_kwh)],
  id.vars = cols_id.vars,
  measure.vars = cols_tier_kwh,
  variable.name = "tier",
  value.name = "base.usage_in.kwh"
)
tmp_dt_kwh[, tier := str_extract(tier, "[0-9]")]
tmp_dt_usd <- melt(
  rrs_simplified[, .SD, .SDcols = c(cols_id.vars, cols_tier_usd)],
  id.vars = cols_id.vars,
  measure.vars = cols_tier_usd,
  variable.name = "tier",
  value.name = "charge_in.usd"
)
tmp_dt_usd[, tier := str_extract(tier, "[0-9]")]
rrs_simplified_variable <-
  tmp_dt_kwh[tmp_dt_usd, on = .(date, rate_code_normalize, tier)]
rrs_simplified_variable[, tier := factor(tier, levels = c("3", "2", "1"))]


# --------------------------------------------------
# Generate Plots: W.R.T. Unit Rates
# --------------------------------------------------
# ------- Set common plot options -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_linedraw(),
    scale_x_date(date_labels = "%Y", date_breaks = "1 year"),
    labs(
      x = ""
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Make plot(s): W.R.T. fixed charge -------
plot_fixed <-
  ggplot(data = rrs_simplified) +
    geom_line(
      aes(x = date, y = fixed_charge_in_usd), color = color.palette[1]
    ) +
    plot.options +
    scale_y_continuous(breaks = seq(4, 12, by = 1), limits = c(4.5, 12)) +
    labs(
      y = "US$ per Month"
    )


# ------- Make plot(s): W.R.T. variable charges -------
plot_variable <-
  ggplot(rrs_simplified_variable) +
    geom_line(aes(x = date, y = charge_in.usd, color = tier), alpha = 0.7) +
    plot.options +
    facet_grid(. ~ rate_code_normalize) +
    scale_y_continuous(
      limits = c(0.05, 0.2), breaks = seq(0.05, 0.2, by = 0.05)
    ) +
    scale_color_manual(values = color.palette) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(
      y = "US$ per kWh",
      color = "Tiers",
      subtitle = "Panel A: Tier Rates"
    )


# --------------------------------------------------
# Generate Plots: W.R.T. Monthly Allowance
# --------------------------------------------------
# ------- Make plot(s): W.R.T. monthly allowane -------
plot_qty <-
  ggplot(rrs_simplified_variable) +
    geom_line(aes(x = date, y = base.usage_in.kwh, color = tier), alpha = 0.7) +
    plot.options +
    facet_wrap(. ~ rate_code_normalize) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(600, 1450),
      breaks = seq(600, 1450, by = 200)
    ) +
    scale_color_manual(values = color.palette) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(
      y = "kWh per Month",
      color = "Tiers",
      subtitle = "Panel B: Base Usage Quantities"
    )


# --------------------------------------------------
# Save Plots
# --------------------------------------------------
# ------- Save plots in PNG format -------
# # 1. Plot of Fixed Charge
PLOT.NAME_FIXED <- "SMUD-Residential-Rate-Schedules_Fixed-Charge.png"
PATH_PLOT_FIXED <- paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_FIXED, sep = "/")
plot.save(PATH_PLOT_FIXED, plot_fixed, width = 20, height = 11, units = "cm")

# # 2. Plot of Variable Charge and Base Usage Qty
PLOT.NAME_VARIABLE <-
  "SMUD-Residential-Rate-Schedules_Variable-Charge-and-Base-Usage.png"
PATH_PLOT_VARIABLE <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_VARIABLE, sep = "/")
plot.save(
  PATH_PLOT_VARIABLE,
  grid.arrange(plot_variable, plot_qty, nrow = 2),
  width = 25, height = 20, units = "cm"
)

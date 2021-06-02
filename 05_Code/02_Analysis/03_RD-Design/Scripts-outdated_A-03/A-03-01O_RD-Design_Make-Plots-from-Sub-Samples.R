# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02O
# #
# > Purpose of the script(s)
# # : To generate plots from sub-samples, which are constructed based on rate
# #   codes.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
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
    paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep= "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep = "/")

# # 1.2. SMUD Residential Rate Schedules
DIR_TO.LOAD_RRS <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RRS <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RRS <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RRS, FILE_TO.LOAD_RRS, sep = "/")


# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, "02_RD-Design/02_Plots", sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Set limits of observations
# # 1.1. Limits related to consumption
NORMALIZED.MONTHLY.QTY_UPPER <- 500
MONTHLY.QTY_UPPER <- 7500
DAILY.QTY_UPPER <- 250

# # 1.2. Limits related year
YEAR_LOWER_1st <- 2004
YEAR_UPPER_1st <- 2007
YEAR_LOWER_2nd <- YEAR_UPPER_1st + 1
YEAR_UPPER_2nd <- 2011


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load Data required
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )

# # 2. Check primary keys of the DT
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Residential Rate Schedules Data -------
# # 1. Load SMUD Residential Rate Schedules data
dt_rrs <-
  pq.to.dt(
    PATH_TO.LOAD_RRS,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )

# # 2. Check primary keys of the DT
stopifnot(
  dt_rrs[
    , .N, by = .(date, rate_code_normalize)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Create DT(s) that will be used later
# --------------------------------------------------
# ------- Create DT(s) -------
# # 1. Generate a DT from SMUD Residential Rate Schedules
dt_rrs[, year := year(date)]
dt_base.qty <-
  dt_rrs[
    ,
    .N,
    by = .(rate_code_normalize, year, season, tier_1_qty_upto_in_kwh)
  ][, N := NULL]

names_old <- c("year", "season")
names_new <- c("billing.year_mid_period0", "season_after_period0")
setnames(dt_base.qty, names_old, names_new)


# --------------------------------------------------
# Generate ggplot objects
# --------------------------------------------------
# ------- Define Common Plot Options -------
plot.options_scatter <-
  list(
    geom_point(
        aes(x = kwh_total_period0, y = kwh_daily.avg),
        alpha = 0.1, size = 1
    ),
    theme_linedraw(),
    facet_grid(
      billing.year_mid_period0 + season_after_period0 ~ is_treated,
      scales = "free_x",
      space = "free_x"
    ),
    scale_x_continuous(
      labels = scales::comma,
      breaks = seq(0, MONTHLY.QTY_UPPER, by = 500)
    ),
    scale_y_continuous(labels = scales::comma, limits = c(0, DAILY.QTY_UPPER)),
    labs(
      x = "Monthly Consumption in Period 0 (kWh/Month)",
      y = "Daily Average Consumption in Period 1 (kWh/Day)"
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Create ggplot objects: for "RSGH" -------
# # 1. Set parameters
rate.codes <- c("RSGH")
consumption_upper.limit <- 4000

# # 2. Create ggplot objects
plot_scatter_g.rate_1st <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter

plot_scatter_g.rate_2nd <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter


# ------- Create ggplot objects: for "RSCH" -------
# # 1. Set parameters
rate.codes <- c("RSCH")

# # 2. Create ggplot objects
plot_scatter_c.rate_1st <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter

plot_scatter_c.rate_2nd <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter


# ------- Create ggplot objects: for "RSEH" -------
# # 1. Set parameters
rate.codes <- c("RSEH")

# # 2. Create ggplot objects
plot_scatter_e.rate_1st <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_1st:YEAR_UPPER_1st) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter

plot_scatter_e.rate_2nd <-
  ggplot(
    dt_for.reg[
      ( rate_code_normalize_period0 %in% rate.codes ) &
        ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) ) &
        ( season_before_period0 == season_after_period0 ) &
        kwh_total_period0 <= consumption_upper.limit
    ]
  ) +
    geom_vline(
      data = dt_base.qty[
        ( rate_code_normalize %in% rate.codes ) &
          ( billing.year_mid_period0 %in% c(YEAR_LOWER_2nd:YEAR_UPPER_2nd) )
      ],
      aes(xintercept = tier_1_qty_upto_in_kwh),
      linetype = "dashed", color = "grey70"
    ) +
    plot.options_scatter


# --------------------------------------------------
# Export Plots created above
# --------------------------------------------------
# ------- Export Plots in PNG format -------
# # 1. Plots for "RSGH"
# # 1.1. Observations the range between 2005 and 2007
PLOT.NAME_G.RATE_1ST <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSGH_2004-2007.png"
PATH_PLOT_G.RATE_1ST <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_G.RATE_1ST, sep = "/")
plot.save(
  PATH_PLOT_G.RATE_1ST,
  plot_scatter_g.rate_1st,
  width = 30, height = 35, units = "cm"
)
# # 1.2. Observations the range between 2008 and 2011
PLOT.NAME_G.RATE_2ND <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSGH_2008-2011.png"
PATH_PLOT_G.RATE_2ND <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_G.RATE_2ND, sep = "/")
plot.save(
  PATH_PLOT_G.RATE_2ND,
  plot_scatter_g.rate_2nd,
  width = 30, height = 35, units = "cm"
)

# # 2. Plots for "RSCH"
# # 2.1. Observations the range between 2005 and 2007
PLOT.NAME_C.RATE_1ST <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSCH_2004-2007.png"
PATH_PLOT_C.RATE_1ST <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_C.RATE_1ST, sep = "/")
plot.save(
  PATH_PLOT_C.RATE_1ST,
  plot_scatter_c.rate_1st,
  width = 30, height = 35, units = "cm"
)
# # 2.2. Observations the range between 2008 and 2011
PLOT.NAME_C.RATE_2ND <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSCH_2008-2011.png"
PATH_PLOT_C.RATE_2ND <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_C.RATE_2ND, sep = "/")
plot.save(
  PATH_PLOT_C.RATE_2ND,
  plot_scatter_c.rate_2nd,
  width = 30, height = 35, units = "cm"
)

# # 3. Plots for "RSEH"
# # 3.1. Observations the range between 2005 and 2007
PLOT.NAME_E.RATE_1ST <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSEH_2004-2007.png"
PATH_PLOT_E.RATE_1ST <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_E.RATE_1ST, sep = "/")
plot.save(
  PATH_PLOT_E.RATE_1ST,
  plot_scatter_e.rate_1st,
  width = 30, height = 35, units = "cm"
)
# # 3.2. Observations the range between 2008 and 2011
PLOT.NAME_E.RATE_2ND <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_RSEH_2008-2011.png"
PATH_PLOT_E.RATE_2ND <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_E.RATE_2ND, sep = "/")
plot.save(
  PATH_PLOT_E.RATE_2ND,
  plot_scatter_e.rate_2nd,
  width = 30, height = 35, units = "cm"
)


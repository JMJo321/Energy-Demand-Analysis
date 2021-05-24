# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-01A
# #
# > Purpose of the script(s)
# # : To generate histograms, with kernel density estimates, from SMUD
# #   Billing Data.

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
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_BILLING <- "SMUD/Billing-Data"
FILE_TO.LOAD_BILLING <- "SMUD_Billing-Data_Extended.parquet"
PATH_TO.LOAD_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_BILLING, FILE_TO.LOAD_BILLING, sep = "/"
  )
# # 1.2. SMUD Residential Rate Schedules
DIR_TO.LOAD_RRS <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RRS <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RRS <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RRS, FILE_TO.LOAD_RRS, sep = "/"
  )

# # 2. Paths at which Output will be saved
# # 2.1. Paths at which Plots will be saved
DIR_TO.SAVE_PLOTS <-
  paste(PATH_NOTE, "01_Validity-Test-of-RD-Design/02_Plots", sep = "/")


# ------- Define parameter(s) -------
RATE.CODS_SELECT <- c("RSCH",  "RSEH",  "RSGH")


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load SMUD Datasets, and Modify them
# --------------------------------------------------
# ------- Load SMUD Datasets -------
# # 1. Load SMUD billing data
billing <-
  pq.to.dt(
    PATH_TO.LOAD_BILLING,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )

# ## To check the primary keys of the billing dataset
stopifnot(
  billing[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# # 2. Load SMUD Residential Rate Schedules
rrs <- pq.to.dt(
  PATH_TO.LOAD_RRS,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
)

# ## To check the primary keys of the billing dataset
stopifnot(
  rrs[
    , .N, by = .(date, rate_code, season)
  ][
    N > 1, .N
  ] == 0
)



# ------- Modify the DTs for later use -------
# # 1. With respect to SMUD Billing Dataset
# # 1.1. Add column indicating each observation's billing year
# ## Note: The billing year is obtained from `period_from`.
billing[, year := year(period_from)]

# # 1.2. Convert data type of `rate_code` from character to factor
billing[, rate_code := factor(rate_code, levels = RATE.CODS_SELECT)]


# # 2. With respect to SMUD Residential Rate Schedules
# # 2.1. Add column indicating year
rrs[, year := year(date)]

# # 2.2. Add a column showing the q'ty of the first tier relative to that of
# #      the second tier
rrs[
  ,
  tier_2_qty_in.percent := tier_2_qty_upto_in_kwh / tier_1_qty_upto_in_kwh * 100
]


# --------------------------------------------------
# Generate Histograms
# --------------------------------------------------
# ------- Set plot options commonly used -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_linedraw(),
    xlim(c(0, 300)),
    labs(
      x = "Monthly Consumption relative to Baseline Allowance (%)",
      y = "Density"
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Generate histograms -------
# # 1. By Year
histogram_by.year <-
  ggplot(
    billing[is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT]
  ) +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_histogram(
        aes(x = kwh_total_in.percent_t1, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      aes(x = kwh_total_in.percent_t1),
      na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(year ~ ., ncol = 5) +
    plot.options


# # 2. By Rate Code
histogram_by.rate.code <-
  ggplot() +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_vline(
      data = rrs[
        rate_code_normalize %in% RATE.CODS_SELECT &
          !is.na(tier_2_qty_in.percent),
        .N,
        keyby = .(rate_code_normalize, season, year, tier_2_qty_in.percent)],
      aes(xintercept = tier_2_qty_in.percent),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_histogram(
        data = billing[
          is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
        ],
        aes(x = kwh_total_in.percent_t1, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      data = billing[
        is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
      ],
      aes(x = kwh_total_in.percent_t1),
      adjust = 1/2, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(. ~ rate_code_normalize, nrow = 1) +
    plot.options


# # 3. By Season
histogram_by.season <-
  ggplot() +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_vline(
      data = rrs[
        rate_code_normalize %in% RATE.CODS_SELECT &
          !is.na(tier_2_qty_in.percent),
        .N,
        keyby = .(rate_code_normalize, season, tier_2_qty_in.percent)],
      aes(xintercept = tier_2_qty_in.percent, color = rate_code_normalize),
      linetype = "dotdash", alpha = 0.7
    ) +
    geom_histogram(
        data = billing[
          is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
        ],
        aes(x = kwh_total_in.percent_t1, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      data = billing[
        is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
      ],
      aes(x = kwh_total_in.percent_t1),
      adjust = 1/2, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(. ~ season, nrow = 1) +
    plot.options +
    scale_color_manual(values = color.palette) +
    labs(color = "Rate Codes")


# # 4. By Season and Rate Code
histogram_by.season.and.rate.code <-
  ggplot() +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_vline(
      data = rrs[
        rate_code_normalize %in% RATE.CODS_SELECT &
          !is.na(tier_2_qty_in.percent),
        .N,
        keyby = .(rate_code_normalize, season, tier_2_qty_in.percent)],
      aes(xintercept = tier_2_qty_in.percent),
      linetype = "dotdash", alpha = 0.5
    ) +
    geom_histogram(
        data = billing[
          is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
        ],
        aes(x = kwh_total_in.percent_t1, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      data = billing[
        is_in.sample == TRUE & rate_code_normalize %in% RATE.CODS_SELECT
      ],
      aes(x = kwh_total_in.percent_t1),
      adjust = 1/2, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_grid(season ~ rate_code_normalize) +
    plot.options


# --------------------------------------------------
# Save Plots
# --------------------------------------------------
# ------- Save plots in PNG format -------
# # 1. Plot for "By Year"
PLOT.NAME_BY.YEAR <- "SMUD-Billing-Data_Histogram_By-Year.png"
PATH_PLOT_BY.YEAR <- paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.YEAR, sep = "/")
plot.save(
  PATH_PLOT_BY.YEAR, histogram_by.year,
  width = 40, height = 20, units = "cm"
)

# # 2. Plot for "By Rate Code"
PLOT.NAME_BY.RATE.CODE <- "SMUD-Billing-Data_Histogram_By-Rate-Code.png"
PATH_PLOT_BY.RATE.CODE <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.RATE.CODE, sep = "/")
plot.save(
  PATH_PLOT_BY.RATE.CODE, histogram_by.rate.code,
  width = 40, height = 15, units = "cm"
)

# # 3. Plot for "By Season"
PLOT.NAME_BY.SEASON <- "SMUD-Billing-Data_Histogram_By-Season.png"
PATH_PLOT_BY.SEASON <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.SEASON, sep = "/")
plot.save(
  PATH_PLOT_BY.SEASON, histogram_by.season,
  width = 40, height = 15, units = "cm"
)

# # 4. Plot for "By Season and Rate Code"
PLOT.NAME_BY.SEASON.AND.RATE.CODE <-
  "SMUD-Billing-Data_Histogram_By-Season-and-Rate-Code.png"
PATH_PLOT_BY.SEASON.AND.RATE.CODE <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.SEASON.AND.RATE.CODE, sep = "/")
plot.save(
  PATH_PLOT_BY.SEASON.AND.RATE.CODE, histogram_by.season.and.rate.code,
  width = 40, height = 25, units = "cm"
)

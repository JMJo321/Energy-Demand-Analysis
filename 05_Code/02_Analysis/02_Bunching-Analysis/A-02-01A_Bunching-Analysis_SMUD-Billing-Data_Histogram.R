# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-01B
# #
# > Purpose of the script(s)
# # : To generate histograms from SMUD Billing Data

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(huxtable)
library(unikn)
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
DIR_TO.LOAD_BILLING <- "SMUD/Billing-Data"
FILE_TO.LOAD_BILLING <- "SMUD_Billing-Data_Extended.parquet"
PATH_TO.LOAD_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_BILLING, FILE_TO.LOAD_BILLING, sep= "/"
  )

# # 2. Paths at which Output will be saved
# # 2.1. Paths at which Plots will be saved
DIR_TO.SAVE_PLOTS <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Plots", sep = "/")
# # 2.2. Paths at which Tables will be saved
DIR_TO.SAVE_TABLES <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Tables", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load SMUD Billing Data, and Modify it
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
billing <-
  pq.to.dt(
    PATH_TO.LOAD_BILLING,
    reg.ex_date= "(^date)|(_from$)|(_to$)",
    is_drop.index_cols= TRUE
  )

# # 2. Check primary keys of the DTs
stopifnot(
  billing[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Modify the DT for later use -------
# # 1. Add column indicating each observation's billing year
# ## Note: The billing year is obtained from `period_from`.
billing[, year := year(period_from)]

# # 2. Convert data type of `rate_code` from character to factor
cols_levels <-
  c(
    "RSC",  "RSE",  "RSG",  "RWC",  "RWE",  "RWG",
    "RSCH", "RSEH", "RSGH", "RWCH", "RWEH", "RWGH"
  )
billing[, rate_code := factor(rate_code, levels = cols_levels)]


# --------------------------------------------------
# Generate Histograms
# --------------------------------------------------
# ------- Set plot options commonly used -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_light(),
    xlim(c(0, 300)),
    labs(
      x = "Monthly Consumption relative to Baseline Allowance (%)",
      y = "Density",
      title = "SMUD Billing Data",
      caption = "Note: The width of each bin is 5%."
    )
  )


# ------- Generate histograms -------
# # 1. By Year
histogram_by.year <-
  ggplot(billing[is_in.sample == TRUE]) +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_histogram(
        aes(x = kwh_total_in.percent, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      aes(x = kwh_total_in.percent),
      na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(year ~ ., ncol = 5) +
    plot.options +
    labs(subtitle = "Histogram of Household Monthly Consumption: By Year")


# # 2. By Rate Code
histogram_by.rate.code <-
  ggplot(billing[is_in.sample == TRUE]) +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_histogram(
        aes(x = kwh_total_in.percent, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      aes(x = kwh_total_in.percent),
      adjust = 1/2, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(. ~ rate_code_normalize, nrow = 2) +
    plot.options +
    labs(
      subtitle = "Histogram of Household Monthly Consumption: By Rate Code",
      caption =
        paste0(
          "Note: The width of each bin is 5%. ",
          "And for kernel density, one-half of default bandwidth is used."
        )
    )

# # 2.1. Make a table to check the number of observations by rate code
table <- billing[!is.na(rate_code_normalize), .N, by = .(rate_code_normalize)]
names(table) <- c("Rate Codes", "Observations")
hux.table <- as_hux(table)
number_format(hux.table)[2:7, 2] <- fmt_pretty()
bottom_border(hux.table)[1,] <- 1
capture.output(
  print_latex(hux.table),
  file = paste(
    DIR_TO.SAVE_TABLES,
    "SMUD-Billing-Data_Histogram_Observations-by-Rate-Code.tex",
    sep = "/"
  )
)


# # 3. By selected Rate Code and Season
# ## Note: The selected rate codes seem to make bunches at the kink.

histogram_by.selected <-
  ggplot(
    billing[
      is_in.sample == TRUE &
        rate_code %in% c("RWC", "RWE", "RWG", "RWCH", "RWEH", "RWGH") &
        !is.na(season_before)
    ]
  ) +
    geom_vline(xintercept = 100, linetype = "dotdash", alpha = 0.5) +
    geom_histogram(
        aes(x = kwh_total_in.percent, y = ..density..),
        binwidth = 5, na.rm = TRUE,
        color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_density(
      aes(x = kwh_total_in.percent),
      adjust = 1/2, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_grid(season_before ~ rate_code_normalize) +
    plot.options +
    labs(
      subtitle =
        paste0(
          "Histogram of Household Monthly Consumption: ",
          "By Selected Rate Code and Season"
        ),
      caption =
        paste0(
          "Note: The width of each bin is 5%. ",
          "And for kernel density, one-half of default bandwidth is used."
        )
    )


# --------------------------------------------------
# Save Plots
# --------------------------------------------------
# ------- Save plots in PNG format -------
PLOT.NAME_BY.YEAR <- "SMUD-Billing-Data_Histogram_By-Year.png"
PATH_PLOT_BY.YEAR <- paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.YEAR, sep = "/")
plot.save(
  PATH_PLOT_BY.YEAR, histogram_by.year, width = 40, height = 20, units = "cm"
)

PLOT.NAME_BY.RATE.CODE <- "SMUD-Billing-Data_Histogram_By-Rate-Code.png"
PATH_PLOT_BY.RATE.CODE <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.RATE.CODE, sep = "/")
plot.save(
  PATH_PLOT_BY.RATE.CODE,
  histogram_by.rate.code,
  width = 40, height = 20, units = "cm"
)

PLOT.NAME_BY.SELECTED <-
  "SMUD-Billing-Data_Histogram_By-Selected-Rate-Code-and-Season.png"
PATH_PLOT_BY.SELECTED <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_BY.SELECTED, sep = "/")
plot.save(
  PATH_PLOT_BY.SELECTED,
  histogram_by.selected,
  width = 40, height = 25, units = "cm"
)

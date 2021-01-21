# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-02A
# #
# > Purpose of the script(s)
# # : To

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
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
DIR_TO.LOAD_GN <- "02_Greenergy-Program"
FILE_TO.LOAD_GN <- "DT_Greenergy-Program.RData"
PATH_TO.LOAD_GN <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN, sep= "/")
# # 1.2. SMUD Residential Rate Panel
DIR_TO.LOAD_RR <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RR <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RR <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RR, FILE_TO.LOAD_RR, sep = "/")

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_FIGURE <- paste(
 PATH_NOTE, "04_Greenergy-Program/02_Figures", sep = "/"
)


# ------- Define parameter(s) -------
# (NOT Applicable)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Datasets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Residential Rates Panel Data -------
dt_rr <- pq.to.dt(
  PATH_TO.LOAD_RR,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)
gc(reset = TRUE, full = TRUE)


# --------------------------------------------------
# Create DTs to make figures
# --------------------------------------------------
# ------- Create a DT: Greenergy Program Participation and Consumption -------
# # 1. Create a DT containing household-level consumption by rate code,
# # Greenergy Program particiation, and season
tmp_dt_qty <- dt_billing[
  !is.na(category_greenergy) &
    rate_code_normalize %in% c("RSCH", "RSEH", "RSGH") &
    billing.year_mid %in% c(2005:2011) &
    season_before == season_after,
  .(ids, rate_code_normalize, kwh_total, category_greenergy, season_before)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
  by = .(ids, rate_code_normalize, category_greenergy, season_before)
]

# # 2. Modify the DT
# # 2.1. Add a column showing aggregated rate codes
tmp_dt_qty[
  rate_code_normalize %in% c("RSCH", "RSEH"), rate.codes := "RSCH & RSEH"
]
tmp_dt_qty[rate_code_normalize == "RSGH", rate.codes := "RSGH"]


# ------- Create a DT: PV installation and consumption, only for non-participants -------
# # 1. Create a DT containing household-level consumption by rate code and
# # season
tmp_dt_pv <- dt_billing[
  category_greenergy == "Non-Greenergy" &
    rate_code_normalize %in% c("RSCH", "RSEH", "RSGH") &
    billing.year_mid %in% c(2005:2011) &
    season_before == season_after,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
  by = .(ids, is_pv.install, rate_code_normalize, season_before)
]

# # 2. Modify the DT
# # 2.1. Add a column showing aggregated rate codes
tmp_dt_pv[
  rate_code_normalize %in% c("RSCH", "RSEH"), rate.codes := "RSCH & RSEH"
]
tmp_dt_pv[is.na(rate.codes), rate.codes := "RSGH"]


# ------- Create a DT containing base usage quantities -------
# # 1. Create a DT
tmp_dt_rr <- melt(
  dt_rr[
    date %in% c(as.Date("2005-01-01"), as.Date("2005-08-01")) &
      rate_code_normalize %in% c("RSCH", "RSGH"),
    .(
      rate_code_normalize, season,
      tier_1_qty_upto_in_kwh, tier_2_qty_upto_in_kwh
    )
  ],
  id.vars = c("rate_code_normalize", "season"),
  measure.vars = c("tier_1_qty_upto_in_kwh", "tier_2_qty_upto_in_kwh"),
  variable.name = "tier", value.name = "kwh"
)

# # 2. Modify the DT
# # 2.1. Change string values
# # 2.1.1. For column "tier"
tmp_dt_rr[
  ,
  tier :=
    str_extract(tier, "^tier_[0-9]") %>% str_replace(., "_", " ") %>%
      str_to_title(.)
]
# # 2.1.2. For column "rate_code_normalize"
tmp_dt_rr[rate_code_normalize == "RSCH", rate_code_normalize := "RSCH & RSEH"]
# # 2.2. Change names of columns
names_old <- c("rate_code_normalize", "season")
names_new <- c("rate.codes", "season_before")
setnames(tmp_dt_rr, names_old, names_new)


# --------------------------------------------------
# Make ggplot objects
# --------------------------------------------------
# ------- Make a ggplot object: Greenergy Program participation and PV installation -------
plot_gn.and.pv <-
  ggplot(
    data = dt_billing[
      !is.na(category_greenergy),
      .N,
      by = .(ids, category_greenergy, is_pv.install)
    ]
  ) +
    geom_jitter(
      aes(x = category_greenergy, y = is_pv.install),
      size = 1, alpha = 0.1
    ) +
    theme_linedraw() +
    labs(x = "", y = "PV System Installation")


# ------- Make a ggplot object: Greenergy Program participation and consumption -------
plot_gn.and.qty <-
  ggplot(data = tmp_dt_qty, aes(x = kwh_total, y = category_greenergy)) +
    geom_boxplot(outlier.size = 1, outlier.alpha = 0.1, color = "grey60") +
    geom_vline(
      data = tmp_dt_rr,
      aes(xintercept = kwh, color = tier),
      linetype = "dashed"
    ) +
    facet_grid(season_before ~ rate.codes) +
    scale_x_continuous(limits = c(0, 2500), labels = scales::comma) +
    labs(x = "Household Mean Consumption (kWh per Month)", y = "", color = "") +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold")) +
    guides(color = "none")
# ## Note: From https://ggplot2.tidyverse.org/reference/geom_boxplot.html
# ## The upper whisker extends from the hinge to the largest value no further
# ## than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or
# ## distance between the first and third quartiles). The lower whisker extends
# ## from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data
# ## beyond the end of the whiskers are called "outlying" points and are
# ## plotted individually.


# ------- Make a ggplot object: PV installation and consumption -------
plot_pv.and.qty <-
  ggplot(data = tmp_dt_pv, aes(x = kwh_total, y = is_pv.install)) +
    geom_boxplot(outlier.size = 1, outlier.alpha = 0.1, color = "grey60") +
    geom_vline(
      data = tmp_dt_rr,
      aes(xintercept = kwh, color = tier),
      linetype = "dashed"
    ) +
    facet_grid(season_before ~ rate.codes) +
    scale_x_continuous(
      breaks = seq(0, 2800, by = 500), limits = c(0, 2800),
      labels = scales::comma
    ) +
    labs(
      x = "Household Mean Consumption (kWh per Month)",
      y = "PV System Installation"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold")) +
    guides(color = "none")


# --------------------------------------------------
# Save ggplot objects in PNG format
# --------------------------------------------------
# ------- Export ggplot objects in PNG format -------
# # 1. For the figure showing the relationship between Greenergy Program
# # participation and PV installation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Relationship-between-Greenergy-Program-Participation-and-PV-Installation.png",
    sep = "/"
  ),
  plot_gn.and.pv,
  width = 25, height = 15, units = "cm"
)

# # 2. For the figure showing the relationship between Greenergy Program
# # participation and consumption
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Relationship-between-Greenergy-Program-Participation-and-Consumption.png",
    sep = "/"
  ),
  plot_gn.and.qty,
  width = 30, height = 20, units = "cm"
)

# # 3. For the figure showing the relationship between PV installation and
# # consumption
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Relationship-between-PV-Installation-and-Consumption.png",
    sep = "/"
  ),
  plot_pv.and.qty,
  width = 30, height = 17, units = "cm"
)
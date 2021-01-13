# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02S
# #
# > Purpose of the script(s)
# # : To generate regression tables from samples, which are constructed based
# #   on rate codes.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stargazer)
library(stringr)
library(lfe)
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
# # 1.1. SMUD Billing Data for RD Design
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Table(s) will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_TABLES <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, DIR_TO.SAVE_RD, "01_Tables/Raw-Tables",
  sep = "/"
)


## ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005
# ## Note:
# ## There are no observation in summer in 2004.


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Create a vector of column names that will be kept
suffixes <- c("period0", "periodm1", "periodm2", "periodm3", "periodm4")
cols_base <- c(
  "ids", "ids_in.factor", "billing.ym_mid", "billing.ym_mid_in.factor",
  "kwh_daily.avg", "cdd_daily.avg_period", "hdd_daily.avg_period",
  "season_before", "season_after"
)
cols_period <- c(
  paste0("rate_code_normalize_", suffixes),
  paste0("is_treated_t1_", suffixes[-1]),
  paste0("is_treated_t2_", suffixes[-1]),
  paste0("kwh_total_in.percent_normalize_t1_", suffixes),
  paste0("kwh_total_in.percent_normalize_t2_", suffixes),
  paste0("kwh_daily.avg_", suffixes),
  paste0("billing.year_mid_", suffixes),
  paste0("season_before_", suffixes),
  paste0("season_after_", suffixes)
)
cols_to.keep <- c(cols_base, cols_period)

# # 2. Load data to run regressions
# # 2.1. Load data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )
# # 2.2. Drop unnecessary columns
dt_for.reg <- dt_for.reg[, .SD, .SDcols = cols_to.keep]
invisible(capture.output(gc(verbose = FALSE, full = TRUE)))
# # 2.3. Add indicator variables about whether there is any change in season
# #      between periods
for (suffix in suffixes) {
  tmp_col.name_to.get <- paste0("season_before_", suffix)
  tmp_col.name_indicator <- paste0("is_season.change_", suffix)
  dt_for.reg[
    season_before == get(tmp_col.name_to.get),
    (tmp_col.name_indicator) := FALSE
  ]
  dt_for.reg[
    season_before != get(tmp_col.name_to.get),
    (tmp_col.name_indicator) := TRUE
  ]
}


# --------------------------------------------------
# Define Regression Models
# --------------------------------------------------
# ------- Read a R script containing regression models -------
PATH_MODELS <- paste0("../../M-Energy-Demand-Analysis_Regression-Models_RD-Design.R")
source(PATH_MODELS)


# --------------------------------------------------
# Run Regressions and Export the Results
# --------------------------------------------------
# ------- Run Regressions and Export the Results in TeX files -------
list_rd_models <- list(
  `With BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear_clustering,
  `Interaction with BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear.w.inter_clustering,
  `With BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear_clustering_p2,
  `Interaction with BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear.w.inter_clustering_p2,
  `With BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear_clustering_p3,
  `Interaction with BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear.w.inter_clustering_p3,
  `With BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear_clustering_p4,
  `Interaction with BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear.w.inter_clustering_p4,
  `With BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear_clustering_p5,
  `Interaction with BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear.w.inter_clustering_p5
)
list_suffixes <- rep(suffixes, each = 2)
list_conditions <- paste0(
      "!is.na(kwh_daily.avg_", list_suffixes, ") & is.finite(kwh_daily.avg_",
      list_suffixes, ")"
    )
names(list_conditions) <- names(list_rd_models)

bws <- c(10, 30, 50, NA)
#list_bws <- as.list(c(10, 30, 50, NA))
list_bws <- as.list(bws)
names(list_bws) <- paste0(bws, "%")
names(list_bws)[length(list_bws)] <- "N/A"

run.regressions_by.model <-
  function(bw, rate.codes, year_lower, year_upper, season) {
    return(
      mapply(
        estimate_felm,
        model = list_rd_models,
        suffix = list_suffixes,
        MoreArgs = list(
          bw = bw,
          rate.codes = rate.codes, season = season,
          year_lower = year_lower, year_upper = year_upper
        ),
        SIMPLIFY = FALSE
      )
    )
  }


estimate_felm <-
  function(model, bw, rate.codes, year_lower, year_upper, season, suffix) {
    tmp_condition <- paste0(
      "!is.na(kwh_daily.avg_", suffix, ") & is.finite(kwh_daily.avg_",
      suffix, ")"
    )
    return(
      estimate.wBW_flexible_felm_for.subsample(
        dt = dt_for.reg[eval(parse(text = tmp_condition))],
        model = model,
        bw = bw,
        rate.codes = rate.codes, season = season,
        year_lower = year_lower, year_upper = year_upper,
        suffix = suffix
      )
    )
  }





export_in.latex <- function(
  bw, rate.codes, season, year_lower, year_upper, path
) {
  tmp_results <- run.regressions_by.model(
    bw = bw, rate.codes = rate.codes, season = season,
    year_lower = year_lower, year_upper = year_upper
  )
  n_results <- length(tmp_results)

  covariate.order <-
    c(1, 2, 13, 3, 4, 14, 5, 6, 15, 7, 8, 16, 9, 10, 17, 11, 12)
  covariate.labels <- c(
    "1[Treated]:0", "NC:0",
    "1[Treated]:0 $\\times$ NC:0",
    "1[Treated]:-1", "NC:-1",
    "1[Treated]:-1 $\\times$ NC:-1",
    "1[Treated]:-2", "NC:-2",
    "1[Treated]:-2 $\\times$ NC:-2",
    "1[Treated]:-3", "NC:-3",
    "1[Treated]:-3 $\\times$ NC:-3",
    "1[Treated]:-4", "NC:-4",
    "1[Treated]:-4 $\\times$ NC:-4",
    "Daily Average CDDs", "Daily Average HDDs"
  )
  list_add.lines <- list(
    c("Rate Code", rep(str_c(rate.codes, collapse = " \\& "), times = n_results)),
    c("Season", rep(season, times = n_results)),
    c("Bandwidth", rep(paste0(bw, "\\%"), times = n_results)),
    c("FEs: Account-Premise IDs", rep("No", times = n_results)),
    c("FEs: Billing Year-Month", rep("Yes", times = n_results))
  )
  capture.output(
    stargazer(
      tmp_results,
      type = "latex",
      title = "",
      covariate.labels = covariate.labels,
      dep.var.labels = "Daily Average Consumption in Period 1 (kWh/Day)",
      add.lines = list_add.lines,
      font.size = "footnotesize",
      omit.stat = c("ser", "f", "rsq"),
      order = covariate.order
    ),
    file = path
  )
}


export_in.latex(
  bw = 10, rate.codes = c("RSGH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Summer_BW-10.tex"
)
export_in.latex(
  bw = 30, rate.codes = c("RSGH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Summer_BW-30.tex"
)
export_in.latex(
  bw = 50, rate.codes = c("RSGH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Summer_BW-50.tex"
)
export_in.latex(
  bw = NA, rate.codes = c("RSGH"), season = "Summer",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Summer_BW-NA.tex"
)

export_in.latex(
  bw = 10, rate.codes = c("RSGH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Winter_BW-10.tex"
)
export_in.latex(
  bw = 30, rate.codes = c("RSGH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Winter_BW-30.tex"
)
export_in.latex(
  bw = 50, rate.codes = c("RSGH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Winter_BW-50.tex"
)
export_in.latex(
  bw = NA, rate.codes = c("RSGH"), season = "Winter",
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  path = "Regression-Results_RD-Design_RSGH_Winter_BW-NA.tex"
)


# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02K
# #
# > Purpose of the script(s)
# # :

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
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
# # 1.1. Regression Results
DIR_TO.LOAD_RD_RESULTS <- "01_RD-Design"
FILE_TO.LOAD_RD_RESULTS <-
  "DT_RD-Design_Regression-Results_Unrestricted-Samples.RData"
PATH_TO.LOAD_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD_RESULTS, FILE_TO.LOAD_RD_RESULTS,
  sep = "/"
)


# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_RD_RESULTS <- "01_RD-Design"
FILE_TO.SAVE_RD_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_Unrestricted-Samples.parquet"
PATH_TO.SAVE_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD_RESULTS, FILE_TO.SAVE_RD_RESULTS,
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Create a DT from Regression Results
# --------------------------------------------------
# ------- Load Regression Results -------
load(PATH_TO.LOAD_RD_RESULTS)


# ------- Modify the DT created -------
# # 1. Change a data field's values
list_var_independent <- list(
  `is_treatedTRUE` =
    "1[Treated]",
  `kwh_total_in.percent_normalize_period0` =
    "NC0",
  `is_treatedTRUE:kwh_total_in.percent_normalize_period0` =
    "NC0 * 1[Treated]",
  `I(kwh_total_in.percent_normalize_period0^2)` =
    "Square of NC0",
  `is_treatedTRUE:I(kwh_total_in.percent_normalize_period0^2)` =
    "Square of NC0 * 1[Treated]",
  `I(kwh_total_in.percent_normalize_period0^3)` =
    "Cubic of NC0",
  `is_treatedTRUE:I(kwh_total_in.percent_normalize_period0^3)` =
    "Cubic of NC0 * 1[Treated]",
  `cdd_daily.avg_period` =
    "Daily Average CDDs",
  `hdd_daily.avg_period` =
    "Daily Average HDDs",
  `(Intercept)` =
    "(Constant)"
)
dt_rd_results[
  ,
  var_independent :=
    sapply(var_independent, function(x) list_var_independent[[x]])
]


# # 2. Change data type: To Factor
# # 2.1. Make objects including factors' levels
order_functional.form <- c(
  "Linear", "Linear with Interaction",
  "Square", "Square with Interaction",
  "Cubic", "Cubic with Interaction"
)
order_model <- c(
  "Without FEs and Controls",
  "Without FEs",
  "With IDs FEs",
  "With BYM FEs",
  "With Both FEs",
  "Interaction without FEs and Controls",
  "Interaction without FEs",
  "Interaction with IDs FEs",
  "Interaction with BYM FEs",
  "Interaction with Both FEs"
)
order_bw <- paste0(c(seq(1, 20, by = 1), seq(30, 100, by = 10)), "%")
order_var_independent <- unlist(list_var_independent, use.names = FALSE)

# # 2.2. Convert data type
dt_rd_results[
  ,
  `:=` (
    functional.form = factor(
      functional.form, levels = order_functional.form, ordered = TRUE
    ),
    model = factor(model, levels = order_model, ordered = TRUE),
    bw_in.str = factor(bw_in.str, levels = order_bw, ordered = TRUE),
    var_independent = factor(
      var_independent, levels = order_var_independent
    )
  )
]


# # 3. Add columns
# # 3.1. Add a column indicating the maximum S.E. among the S.E.s
cols_se <- names(dt_rd_results)[str_detect(names(dt_rd_results), "^se_")]
dt_rd_results[, se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se]


# # 4. Drop unnecessary column(s)
dt_rd_results[, is_clustering := NULL]


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
  dt_rd_results,
  sink = PATH_TO.SAVE_RD_RESULTS,
  version = "1.0",
  compression = "gzip",
  use_dictionary = TRUE
)


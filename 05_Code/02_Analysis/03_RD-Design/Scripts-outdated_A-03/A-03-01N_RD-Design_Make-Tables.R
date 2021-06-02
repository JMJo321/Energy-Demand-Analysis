# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02N
# #
# > Purpose of the script(s)
# # : To generate regression tables.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(fixest)
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
# # 1.1. Regression Results
DIR_TO.LOAD_RD_RESULTS <-
  "/Volumes/withMark/Energy-Demand-Analysis/Regression-Results/RD-Design/For-Unrestricted-Sample"

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Table(s) will be saved
DIR_TO.SAVE_TABLES <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS,
  "02_RD-Design/01_Tables/Raw-Tables",
  sep = "/"
)


## ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Create and Export Regression Tables
# --------------------------------------------------
# ------- Creat Regression Tables, and Export it in TeX files -------
model <- "Square"
#bws <- c(2, 5, 10, 20, 40)
bws <- c(40)
for (bw in bws) {
  if (is.na(bw)) {
    tmp_bw <- "NA"
  } else {
    tmp_bw <- bw
  }

  tmp_rdata <- paste0("RD-Design_Results_", model, "_BW-", tmp_bw, ".RData")
  tmp_rdata_w.interaction <- paste0(
    "RD-Design_Results_", model, "-with-Interaction_BW-", tmp_bw, ".RData"
  )
  tmp_files_to.load <- c(tmp_rdata, tmp_rdata_w.interaction)
  lapply(
    paste(DIR_TO.LOAD_RD_RESULTS, tmp_files_to.load, sep = "/"), load,
    .GlobalEnv
  )

  tmp_path_to.save <-
    paste(
      DIR_TO.SAVE_TABLES,
      paste0("Regression-Results_RD-Design_", model, "_BW-", tmp_bw, ".tex"),
      sep = "/"
    )

  setFixest_dict(
    c(
      `(Intercept)` = "(Constant)",
      `is_treatedTRUE` = "1[Treated]",
      `kwh_daily.avg` = "Daily Average Consumption in Period 1 (kWh/Day)",
      `kwh_total_in.percent_normalize_period0` = "NC0",
      `I(kwh_total_in.percent_normalize_period0^2)` = "NC0$^2$",
      `I(kwh_total_in.percent_normalize_period0^3)` = "NC0$^3$",
      `cdd_daily.avg_period` = "Daily Avergage CDDs",
      `hdd_daily.avg_period` = "Daily Avergage HDDs",
      `factor(ids)` = "FEs: Account-Premise IDs",
      `factor(billing.ym_mid)` = "FEs: Billing Year-Month"
    )
  )
  setFixest_se(no_FE = "hetero", one_FE = "cluster", two_FE = "cluster")

  tmp_results <-
    paste0("list_results_", tolower(model), "_bw.", tolower(tmp_bw))
  tmp_results_w.interaction <-
    paste0("list_results_", tolower(model), ".w.inter_bw.", tolower(tmp_bw))
  if (is.na(bw)) {
    tmp_title <- paste0(
      "Regression Results: RD Design, ", model, " Models without Bandwidth"
    )
  } else {
    tmp_title <- paste0(
      "Regression Results: RD Design, ", model, " Models with ",
      bw,
      "\\% Bandwidth"
    )
  }

  capture.output(
    etable(
      mapply(summary, get(tmp_results)),
      get(tmp_results_w.interaction),
      tex = TRUE,
      drop = c("Constant"),
      order = c(
        "Treated",
        "NC0",
        "Daily Avergage CDDs",
        "Daily Avergage HDDs"
      ),
      title = tmp_title,
      label = paste0("Table:Regression-Results_RD_", model, "_BW-", tmp_bw),
      extraline = list(
        "{title:\\midrule; where:var}Bandwidth" = rep(
          paste0(bw, "\\%"), times = 10
        )
      ),
      fitstat = c("n", "r2", "ar2"),
      notes = "NC0 means Normalized Consumption in period 0 relative to Base Usage Qty (\\%). Standard errors are clustered by Account-Premise IDs or Billing Year-Month."
    ),
     file = tmp_path_to.save
  )

  rm(list = c(tmp_results, tmp_results_w.interaction))
  invisible(capture.output(gc(verbose = FALSE, full = TRUE)))
}


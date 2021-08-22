# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04G-2
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Create DTs by extracting estimates from regression
# # results.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(broom)
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
DIR_TO.LOAD_CER_RESULTS <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  sep = "/"
)

# # 2. Path(s) to which Dataset(s) is(are) saved
# # 2.1. For the DT including estimates extracted from regression results
DIR_TO.SAVE_CER_RESULTS <- DIR_TO.LOAD_CER_RESULTS
FILE_TO.SAVE_CER_ESTIMATES <-
  "CER_Estimates_Rate-Period-Level-Response-to-Temperature.RData"
PATH_TO.SAVE_CER_ESTIMATES <- paste(
  DIR_TO.SAVE_CER_RESULTS,
  FILE_TO.SAVE_CER_ESTIMATES,
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create, from Regression Results, DTs that include Estimates
# ------------------------------------------------------------------------------
# ------- Create DTs that include Estimates from Regression Results  -------
# # 1. Make a list of files to be loaded
files <- list.files(DIR_TO.LOAD_CER_RESULTS)
reg.results <-
  files[
    str_detect(
      files,
      "^CER_Regression-Results_Rate-Period-Level-Response-to-Temperature"
    )
  ]

# # 2. Create DTs
for (result in reg.results) {
  # ## Make a temporary object name to which the DT created will be assigned
  tmp_obj.name <-
    str_replace(
      result,
      "^CER_Regression-Results_Rate-Period-Level-Response-to-Temperature_",
      ""
    ) %>%
      str_replace(., "\\.RData", "") %>%
      str_replace_all(., "-", ".") %>%
      tolower(.) %>%
      paste0("estimates_temp.response.by.period_30min_", .)

  # ## Load regression result
  load(paste(DIR_TO.LOAD_CER_RESULTS, result, sep = "/"))

  # ## Create objects that will be use when making DTs
  obj_to.get <- ls()[str_detect(ls(), "^results_")]
  if (str_detect(obj_to.get, "without.clustered.ses")) {
    se.type <- "robust"
  } else {
    se.type <- "cluster"
  }

  # ## Create DTs
  assign(
    tmp_obj.name,
    lapply(
      get(obj_to.get),
      get_estimates.from.felm, level = 0.95, fe = FALSE, se.type = se.type
    )
  )

  # ## Remove the regression results that are already used to make DTs
  rm(list = obj_to.get)
  gc(reset = TRUE, full = TRUE)
}


# ------- Save DTs created in .RData Format  -------
estimates_to.save <- ls()[str_detect(ls(), "^estimates_")]
save(list = estimates_to.save, file = PATH_TO.SAVE_CER_ESTIMATES)

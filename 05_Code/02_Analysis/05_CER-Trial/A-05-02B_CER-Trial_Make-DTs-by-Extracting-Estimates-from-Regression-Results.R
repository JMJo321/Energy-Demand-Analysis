# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-02B
# #
# > Purpose of the script(s):
# # Create DTs by extracting estimates from regression results.

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
  paste0(
    "CER_Estimates_Rate-Period-Level-Treatment-Effect_By-Season-and-Tariff_",
    "With-Rate-Changes_by-Rate-Period.RData"
  )
PATH_TO.SAVE_CER_ESTIMATES <- paste(
  DIR_TO.SAVE_CER_RESULTS,
  FILE_TO.SAVE_CER_ESTIMATES,
  sep = "/"
)


# ------- Define parameter(s) -------
levels_model <- c(
  "FEs: i-BY-w",
  "FEs: i-BY-w + d-BY-p",
  "FEs: i-BY-w + d-BY-w",
  "FEs: i-BY-w + m-BY-p",
  "FEs: i-BY-w + m-BY-w",
  "FEs: i-BY-w + d-BY-p + m-BY-p",
  "FEs: i-BY-w + d-BY-w + m-BY-w"
)
levels_season <- c("Warm", "Cold", "Both")
levels_rate.period <- list(
  night = "Night (23-7)",
  day_pre.peak = "Day: Pre-Peak (8-16)",
  peak = "Peak (17-18)",
  day_post.peak = "Day: Post-Peak (19-22)"
)
levels_tariff <- c("A", "B", "C", "D")

# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create, from Regression Results, DTs that include Estimated Treatment Effect
# ------------------------------------------------------------------------------
# ------- Create DTs that include Estimates from Regression Results  -------
# # 1. Make a list of files to be loaded
files <- list.files(DIR_TO.LOAD_CER_RESULTS)
reg.results <-
  files[
    str_detect(
      files,
      paste0(
        "^CER_Regression-Results_Rate-Period-Level-Treatment-Effect_.+",
        "By-Season-and-Tariff_With-Rate-Changes"
      )
    )
  ]

# # 2. Create DTs
for (result in reg.results) {
  # ## Make a temporary object name to which the DT created will be assigned
  tmp_ref.temp <-
    str_extract(result, "[0-9]+") %>%
      str_replace_all(., "-", ".") %>%
      tolower(.)
  tmp_obj.name <-
    str_replace(
      result,
      paste0(
        "^CER_Regression-Results_Rate-Period-Level-Treatment-Effect_.+",
        "By-Season-and-Tariff_With-Rate-Changes"
      ),
      ""
    ) %>%
      str_replace(., "\\.RData", "") %>%
      str_replace_all(., "-", ".") %>%
      tolower(.) %>%
      paste0(
        "estimates_by.season.and.tariff_w.rate.change", .
      ) %>%
      paste0(., "_", tmp_ref.temp)

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


# ------------------------------------------------------------------------------
# Create a DT based on DTs created above, and then save them in .RData format
# ------------------------------------------------------------------------------
# -------  -------
# # 1. Create a DT by combining DTs created above
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period <- rbind(
  rbindlist(
    estimates_by.season.and.tariff_w.rate.change_by.rate.period_60,
    idcol = "tmp_desc"
  ) %>%
    .[, model := "FEs: i-BY-w + d-BY-w + m-BY-w"]
) %>%
  .[, model := factor(model, levels = levels_model)] %>%
  .[, ref.temp_f := 60]


# # 2. Modify the DT created above
# # 2.1. Add Columns
# # 2.1.1. Add a column that shows seasons
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
  ,
  season := (
    tmp_desc %>%
      str_extract(., "^[a-z]+") %>%
      str_to_title(.) %>%
      factor(., levels = levels_season)
  )
]
# # 2.1.2. Add a column that shows rate periods
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
  ,
  tmp_rate.period :=
    str_extract(tmp_desc, "(night)|(day_pre.peak)|(peak)|(day_post.peak)")
]
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
  ,
  rate.period := (
    lapply(tmp_rate.period, function (x) levels_rate.period[[x]]) %>%
      as.character(.) %>%
      factor(., levels = levels_rate.period)
  )
]
# # # 2.1.3. Add a column that show whether a point estimate is significant or not
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
  ,
  is_significant := !(conf.low <= 0 & 0 <= conf.high)
]

# # 2.2. Drop unnecessary columns
dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
  ,
  `:=` (
    tmp_desc = NULL,
    tmp_rate.period = NULL
  )
]


# ------- Save DTs created in .RData Format  -------
save(
  dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period,
  file = PATH_TO.SAVE_CER_ESTIMATES
)

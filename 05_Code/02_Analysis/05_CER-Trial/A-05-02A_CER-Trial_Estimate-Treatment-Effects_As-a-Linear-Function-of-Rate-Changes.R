# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-02A
# #
# > Purpose of the script(s):
# # Run regressions to estimate the treatment effect by rate period and tariff,
# # Use the model in which electricity consumption is a linear function of
# # rate changes.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(lfe)
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
# # 1.1. For the DT for regression analysis
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY,
  sep = "/"
)

# # 1.2. For R Script including Regression Models
FILE_TO.LOAD_CER_MODELS <- "M-Energy-Demand-Analysis_Regression-Models_CER.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_CODE,
  FILE_TO.LOAD_CER_MODELS,
  sep = "/"
)


# # 2. Path(s) to which Regression Results will be stored
DIR_TO.SAVE_CER <- paste(PATH_DATA_ANALYSIS, "04_CER", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To get a subsetting condition
get_condition.in.str <- function (case) {
  if (case[1] %in% c("warm", "cold")) {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0("season == '", (case[1] %>% str_to_title(.)), "'"),
      paste0(
        "as.character(rate.period_detail_level1) == '",
        list_rate.periods[[case[2]]],
        "'"
      ),
      sep = " & "
    )
  } else {
    condition_in.str <- paste(
      "is_in.sample_incl.control == TRUE",
      paste0(
        "as.character(rate.period_detail_level1) == '",
        list_rate.periods[[case[2]]],
        "'"
      ),
      sep = " & "
    )
  }
  return (condition_in.str)
}

# # 2. To run regressions by using `lapply` function
get_reg.result <- function(season.and.rate.period, formula) {
  reg.result <- felm(
    data = dt_for.reg[
      eval(parse(text = get_condition.in.str(season.and.rate.period)))
    ],
    formula = formula
  )
  return (reg.result)
}


# ------------------------------------------------------------------------------
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
dt_for.reg <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# ------- Load Script(s) -------
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Run regressions
# ------------------------------------------------------------------------------
# ------- Modify the DT that will be used to run regressions -------
# # 1. Add columns
# # 1.1. Add columns that shows the changes in rate
# # 1.1.1. At Period-Rate Period level
basis_rate <- dt_for.reg[
  group == "Control", .N, by = .(rate_cents.per.kwh)
]$rate_cents.per.kwh
dt_for.reg[
  ,
  rate.change_by.period.and.rate.period := rate_cents.per.kwh - basis_rate
]
# # 1.1.2. At Rate Period level
dt_rate.change <- dt_for.reg[
  period == "Treatment",
  .N,
  by = .(alloc_r_tariff, rate.period, rate.change_by.period.and.rate.period)
][
  , N := NULL
]
setnames(
  dt_rate.change,
  old = "rate.change_by.period.and.rate.period",
  new = "rate.change_by.rate.period"
)
dt_for.reg <- merge(
  x = dt_for.reg,
  y = dt_rate.change,
  by = c("alloc_r_tariff", "rate.period"),
  all.x = TRUE
)

# # 1.3. Add columns that are used to run regressions directly
dt_for.reg[, hdd.times.post := hdd_all_60f * is_treatment.period]
dt_for.reg[
  ,
  treatment.times.rate.change_by.rate.period :=
    is_treated_r * rate.change_by.rate.period
]
dt_for.reg[
  ,
  hdd.times.treatment.times.rate.change_by.rate.period :=
    hdd_all_60f * treatment.times.rate.change_by.rate.period
]
dt_for.reg[
  ,
  treatment.and.post.times.rate.change_by.rate.period :=
    is_treatment.and.post * rate.change_by.rate.period
]
dt_for.reg[
  ,
  hdd.times.treatment.and.post.times.rate.change_by.rate.period :=
    hdd_all_60f * treatment.and.post.times.rate.change_by.rate.period
]


# ------- Create objects that will be used when running regressions -------
# ## Note:
# ## Vectors `rate.periods_detail1_modified`, `tariffs`, and `tariffs_modified`
# ## are created in the script "M-Energy-Demand-Analysis_Regression-Models_CER".

# # 1. Create a list that includes the subsetting conditions for each
# #    regression
# # 1.1. Create objects that will be used later
# # 1.1.1. Object(s) regarding rate periods
rate.periods_detail1 <-
  dt_for.reg[
    , .N, keyby = .(rate.period_detail_level1)
  ]$rate.period_detail_level1 %>% as.character(.)
list_rate.periods <- as.list(rate.periods_detail1)
names(list_rate.periods) <- rate.periods_detail1_modified
# # 1.1.2. Object(s) regarding seasons
seasons <- "Both"
# ## Note:
# ## Do not separately estimate the treatment effect for "Warm" and "Cold"
# ## seasons.

# # 1.2. Create a list that includes conditions for subsetting the sample
# # 1.2.1. Create a DT that will be used to create a list
dt_cases <-
  expand.grid(seasons, rate.periods_detail1_modified) %>%
    setDT(.)
names(dt_cases) <- c("season", "rate.period")
# # 1.2.2. Create a list by using the DT created above
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  tmp_season <- dt_cases[row]$season %>% tolower(.)
  tmp_rate.period <- dt_cases[row]$rate.period %>% as.character(.)

  tmp_name <- paste(tmp_season, tmp_rate.period, sep = "_")
  tmp_list <- list(c(tmp_season, tmp_rate.period))
  names(tmp_list) <- tmp_name
  list_cases <- c(list_cases, tmp_list)
}


# # 2. Create a list that includes regression models
# # 2.1. Create a list that includes econometric models defined in the script
# #      "M-Energy-Demand-Analysis_Regression-Models_CER"
models_by.period.and.tariff_30min_w.rate.change <- list(
  model_by.period.and.tariff_30min_iw.dw.mw_by.rate.period =
    model_30min_iw.dw.mw_w.rate.change.by.rate.period
)


# ------- Run regressions -------
# ## Note:
# ## Run regression, save as .RData format, remove regression results, and
# ## make memory clean.

for (idx in 1:length(models_by.period.and.tariff_30min_w.rate.change)) {
  # ## Create temperature objects
  tmp_obj.name <-
    names(
      models_by.period.and.tariff_30min_w.rate.change
    )[idx] %>%
      str_replace(., "^model", "results") %>%
      str_replace(., "30min", "30min_60f")
  tmp_model <-
    names(
      models_by.period.and.tariff_30min_w.rate.change
    )[idx] %>%
      str_replace(., "(?>^.+_)", "") %>%
      str_replace_all(., "\\.", "-") %>%
      str_to_title(.)

  # ## Run regressions
  print("Running Regressions ...")
  assign(
    tmp_obj.name,
    lapply(
      list_cases,
      get_reg.result,
      formula =
        models_by.period.and.tariff_30min_w.rate.change[[idx]]
    )
  )

  # ## Save regression results in .RData format
  print("Saving Regression Results ...")
  save(
    list = tmp_obj.name,
    file = paste(
      DIR_TO.SAVE_CER,
      paste(
        "CER_Regression-Results_Rate-Period-Level-Treatment-Effect",
        "Based-on-60F",
        "By-Season-and-Tariff_With-Rate-Changes",
        paste0(tmp_model, ".RData"),
        sep = "_"
      ),
      sep = "/"
    )
  )

  # ## Remove regression results from memory
  rm(list = tmp_obj.name)
  gc(reset = TRUE, full = TRUE)

  # ## Print a message that shows the current work progress
  print(
    paste0(
      "Estimation is completed : Model ", idx, " out of ",
      length(models_by.period.and.tariff_30min_w.rate.change)
    )
  )
}

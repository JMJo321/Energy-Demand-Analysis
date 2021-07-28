# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02Q
# #
# > Purpose of the script(s)
# # :

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
DIR_TO.LOAD_RD_RESULTS <-
  paste(
    "/Volumes/Extreme-Pro/Energy-Demand-Analysis/Regression-Results",
    "RD-Design/For-Restricted-Samples/By-Rate-Code/From-FELM",
    sep = "/"
  )

# # 2. Paths at which Output will be saved
# # 2.1. Path at which DT created will be saved
DIR_TO.SAVE_RD <- "01_RD-Design"
# # 2.1.1. For Uncombined DT
FILE_TO.SAVE_RD_UNCOMBINED <- paste(
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Rate-Codes",
  "Uncombined.parquet",
  sep = "_"
)
PATH_TO.SAVE_RD_UNCOMBINED <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, FILE_TO.SAVE_RD_UNCOMBINED, sep = "/"
)
# # 2.1.2. For Combined DT
FILE_TO.SAVE_RD_COMBINED <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Rate-Codes.parquet"
PATH_TO.SAVE_RD_COMBINED <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, FILE_TO.SAVE_RD_COMBINED, sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
extract_reg.results_felm_by.rate.code <- function (dir, file) {
  # # 1. Print a Message showing Work Progress
  print(paste0("Ingesting: ", file))

  # # 2. Extract Information from filename given
  # # 2.1. Extract Rate Codes from file name
  tmp_rate.codes <-
    str_extract(file, "_RS.+?(?<=_)") %>%
      str_replace_all(., "_", "") %>%
      str_replace(., "-", ", ")
  # # 2.2. Extract Season from file name
  tmp_season <- str_extract(file, "(Summer)|(Winter)")
  # # 2.3. Extract Bandwidth from file name
  tmp_bw_in.str <-
    str_extract(file, "BW-[0-9]+(?=.)") %>% str_extract(., "[0-9]+")
  if (is.na(tmp_bw_in.str)) {
    tmp_bw_modified <- "N/A"
  } else {
    tmp_bw_modified <- paste0(tmp_bw_in.str, "%")
  }
  # # 2.4. Extract Info. about whether Standard Errors are clustered or not
  tmp_is_clustering <- str_detect(file, "No-Clustering", negate = TRUE)

  # # 3. Load Regression Results
  tmp_path_to.load <- paste(dir, file, sep = "/")
  load(tmp_path_to.load)

  # # 4. Extract Information from the Regression Results
  tmp_dt_extracted <- setDT(NULL)

  tmp_results <- ls()[str_detect(ls(), "list_results")]
  tmp_models <- names(get(tmp_results))
  for (model in tmp_models) {
    # ## Independent Variable
    tmp_dt_vars_independent <-
      get(tmp_results)[[model]]$coefficients %>% row.names(.) %>%
      as.data.table(.) %>% setnames(., ".", "var_independent")
    tmp_n_indepent.var <- tmp_dt_vars_independent[, .N]
    # ## Dependent Variable
    tmp_var_dependent <- get(tmp_results)[[model]]$lhs
    # ## Functional Form
    tmp_functional.form <- str_extract(model, "(Linear)|(Square)|(Cubic)")
    # ## Model
    tmp_model <- str_replace(model, ",.+$", "")
    # ## Method
    tmp_method <-
      get(tmp_results)[[model]] %>% .$call %>% .[[1]] %>% as.character(.)
    # ## Create a DT including base information
    tmp_dt_base <- cbind(
      data.table(
        rate.codes = rep(tmp_rate.codes, times = tmp_n_indepent.var),
        season = rep(tmp_season, times = tmp_n_indepent.var),
        functional.form = rep(tmp_functional.form, times = tmp_n_indepent.var),
        model = rep(tmp_model, times = tmp_n_indepent.var),
        method = rep(tmp_method, times = tmp_n_indepent.var),
        is_clustering = rep(tmp_is_clustering, times = tmp_n_indepent.var),
        var_dependent = rep(tmp_var_dependent, times = tmp_n_indepent.var)
      ),
      tmp_dt_vars_independent
    )
    # ## Add data fields about bandwidth
    tmp_dt_base[
      ,
      `:=` (
        bw_in.str = tmp_bw_modified,
        bw_in.percent =
          str_extract(tmp_bw_modified, "([0-9].+?(?=%))|([0-9](?=%))") %>%
            as.numeric(.)
      )
    ]
    # ## Estimated Coefficients
    tmp_dt_coef <-
      get(tmp_results)[[model]] %>% .$coefficients %>% as.data.table(.) %>%
        setnames(., tmp_var_dependent, "estimates")
    # ## Estimated Standard Errors
    if (tmp_is_clustering) {
      tmp_dt_se_standard <-
        get(tmp_results)[[model]] %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        get(tmp_results)[[model]] %>% summary(., robust = TRUE) %>%
          .$coefficients %>% .[, "Cluster s.e."] %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_hetero")
    } else {
      tmp_dt_se_standard <-
        get(tmp_results)[[model]] %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        get(tmp_results)[[model]] %>% summary(., robust = TRUE) %>%
          .$coefficients %>% .[, "Robust s.e"] %>% as.data.table(.) %>%
          setnames(., ".", "se_hetero")
    }

    # ## Bind the Information extracted
    tmp_dt_binded <- cbind(
      tmp_dt_base,
      tmp_dt_coef,
      tmp_dt_se_standard,
      tmp_dt_se_cluster,
      tmp_dt_se_hetero
    )

    tmp_dt_extracted <- rbind(tmp_dt_extracted, tmp_dt_binded)
  }
  # ## Return the information binded
  return(tmp_dt_extracted)

  rm(list = c(tmp_results))
  invisible(capture.output(gc(verbose = FALSE, full = TRUE)))
}


# --------------------------------------------------
# Create a DT from Regression Results
# --------------------------------------------------
# ------- Create a DT from Regression Results -------
# # 1. Extract Information from Regression Results
list_files_to.load <- list.files(path = DIR_TO.LOAD_RD_RESULTS) %>% as.list(.)
list_extracted <-
  lapply(
    X = list_files_to.load,
    FUN = extract_reg.results_felm_by.rate.code,
    dir = DIR_TO.LOAD_RD_RESULTS
  )

# # 2. Create a DT from the Extracted Information
dt_extracted <- rbindlist(list_extracted)


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
dt_extracted[
  ,
  var_independent :=
    sapply(var_independent, function(x) list_var_independent[[x]])
]


# # 2. Change data type: To Factor
# # 2.1. Make objects including factors' levels
order_rate.codes <- c("RSCH, RSEH", "RSGH")
order_season <- c("Summer", "Winter")
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
order_bw <- c(
  paste0(
    c(seq(1, 20, by = 1), seq(30, 50, by = 5), seq(60, 100, by = 10)), "%"
  ),
  "N/A"
)
order_var_independent <- unlist(list_var_independent, use.names = FALSE)

# # 2.2. Convert data type
dt_extracted[
  ,
  `:=` (
    rate.codes = factor(rate.codes, levels = order_rate.codes, ordered = TRUE),
    season = factor(season, levels = order_season, ordered = TRUE),
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


# ------- Create a DT by combining Clustering and Non-Clustering Data -------
# # 1. Generate Temporary DTs
tmp_dt_clustering <- dt_extracted[is_clustering == TRUE]
tmp_dt_no.clustering <- dt_extracted[is_clustering == FALSE]

# # 2. Combine the DTs
cols_on <- c(
  "rate.codes", "season", "functional.form", "model", "var_independent",
  "bw_in.str"
)
dt_rd_results_by.rate.code <-
  tmp_dt_clustering[
    tmp_dt_no.clustering[, .SD, .SDcols = c(cols_on, "se_hetero")],
    on = cols_on
  ]

# # 3. Modify the DT merged
dt_rd_results_by.rate.code[, `:=` (is_clustering = NULL, se_hetero = NULL)]
setnames(dt_rd_results_by.rate.code, "i.se_hetero", "se_hetero")


# ------- Add data files to the DTs -------
# # 3.1. Add a column indicating the maximum S.E. among the S.E.s
cols_se <-
  names(dt_rd_results_by.rate.code)[
    str_detect(names(dt_rd_results_by.rate.code), "^se_")
  ]
dt_extracted[
  , se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se
]
dt_rd_results_by.rate.code[
  , se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se
]


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
# # 1. For "dt_extracted"
arrow::write_parquet(
  dt_extracted,
  sink = PATH_TO.SAVE_RD_UNCOMBINED,
  version = "1.0",
  compression = "gzip",
  use_dictionary = TRUE
)

# # 2. For "dt_rd_results_by.rate.code"
arrow::write_parquet(
  dt_rd_results_by.rate.code,
  sink = PATH_TO.SAVE_RD_COMBINED,
  version = "1.0",
  compression = "gzip",
  use_dictionary = TRUE
)


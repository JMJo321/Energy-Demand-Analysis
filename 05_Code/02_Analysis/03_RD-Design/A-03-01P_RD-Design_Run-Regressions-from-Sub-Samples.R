# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02P
# #
# > Purpose of the script(s)
# # : Run regressions by using sub-samples, which are constructed based on
# #   rate codes and seasons.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(stargazer)
library(lfe)
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
# # 1.1. SMUD Billing Data for RD Design
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005
# ## Note:
# ## There are no observation in summer in 2004.


# ------- Define function(s) -------
# # 1. Run regressions, and then save the results
save.estimates <-
  function(
    bw, rate.codes, year_lower, year_upper, season,
    models_in.list, base.name_object, dir_to.save, base.name_file
  ) {
    # ## Generate the name of temporary object containing regression results
    if (is.na(bw)) {
      tmp_bw <- "na"
    } else {
      tmp_bw <- bw
    }
    tmp_obj.name <- paste(base.name_object, tmp_bw, sep = ".")
    # ## Run regressions
    assign(
      tmp_obj.name,
      lapply(
        models_in.list, estimate.wBW_felm_for.subsample,
        bw = bw,
        rate.codes = rate.codes,
        year_lower = year_lower, year_upper = year_upper,
        season = season
      )
    )
    # ## Save the regression results in .RData format
    tmp_path <- paste(
      dir_to.save,
      paste0(base.name_file, "-", toupper(tmp_bw), ".RData"),
      sep = "/"
    )
    save(list = c(tmp_obj.name), file = tmp_path)
    # ## Remove the regression results from the global environment
    rm(list = c(tmp_obj.name))
    invisible(capture.output(gc(verbose= FALSE, full= TRUE)))
}


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date= "(^date)|(_from$)|(_to$)",
    is_drop.index_cols= TRUE
  )

# # 2. Check primary keys of the DTs
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Define Regression Models
# --------------------------------------------------
# ------- Read a R script containing regression models -------
PATH_MODELS <- paste0("../../M-Energy-Demand-Analysis_Regression-Models_RD-Design.R")
source(PATH_MODELS)


# --------------------------------------------------
# Run Regressions
# --------------------------------------------------
# ------- Create objects that will be used later -------
# # 1. Make a list of bandwidths
list_bws <-
  as.list(c(seq(1, 20, by = 1), seq(25, 50, by = 5), seq(60, 100, by = 10)))

# # 2. Make a list of models
# # 2.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Linear` =
    felm_rd_bym.fes_linear_clustering,
  `Interaction with BYM FEs, Linear` =
    felm_rd_bym.fes_linear.w.inter_clustering,
  `With BYM FEs, Square` =
    felm_rd_bym.fes_square_clustering,
  `Interaction with BYM FEs, Square` =
    felm_rd_bym.fes_square.w.inter_clustering,
  `With BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic_clustering,
  `Interaction with BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic.w.inter_clustering
)

# # 2.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Linear` =
    felm_rd_bym.fes_linear_no.clustering,
  `Interaction with BYM FEs, Linear` =
    felm_rd_bym.fes_linear.w.inter_no.clustering,
  `With BYM FEs, Square` =
    felm_rd_bym.fes_square_no.clustering,
  `Interaction with BYM FEs, Square` =
    felm_rd_bym.fes_square.w.inter_no.clustering,
  `With BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic_no.clustering,
  `Interaction with BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic.w.inter_no.clustering
)


# ------- Run Regressions: for "RSGH" -------
# # 1. For observations in summer seasons
# # 1.1. For models with clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_g.code_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSGH_Summer_BW"
)
# ## Note:
# ## It takes about 2 hours and 40 minutes.

save.estimates(
  bw = NA,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_g.code_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSGH_Summer_BW"
)

# # 1.2. For models without clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_g.code_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSGH_Summer_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_g.code_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSGH_Summer_BW"
)


# # 2. For observations in winter seasons
# # 2.1. For models with clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_g.code_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSGH_Winter_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_g.code_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSGH_Winter_BW"
)

# # 2.2. For models without clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_g.code_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSGH_Winter_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_g.code_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSGH_Winter_BW"
)


# ------- Run Regressions: for "RSCH" and "RSEH" -------
# # 1. For observations in summer seasons
# # 1.1. For models with clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_ce.codes_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSCH-RSEH_Summer_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_ce.codes_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSCH-RSEH_Summer_BW"
)

# # 1.2. For models without clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_ce.codes_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSCH-RSEH_Summer_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Summer",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_ce.codes_summer_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSCH-RSEH_Summer_BW"
)


# # 2. For observations in winter seasons
# # 2.1. For models with clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_ce.codes_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSCH-RSEH_Winter_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_clustering,
  base.name_object = "list_results_felm_clustering_ce.codes_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_Clustering_RSCH-RSEH_Winter_BW"
)

# # 2.2. For models without clustering
lapply(
  list_bws, save.estimates,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_ce.codes_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSCH-RSEH_Winter_BW"
)
save.estimates(
  bw = NA,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = "Winter",
  models_in.list = list_rd_models_for.subsample_no.clustering,
  base.name_object = "list_results_felm_no.clustering_ce.codes_winter_bw",
  dir_to.save = DIR_TO.SAVE_RESULTS,
  base.name_file = "RD-Design_Results_FELM_No-Clustering_RSCH-RSEH_Winter_BW"
)

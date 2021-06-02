# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02J
# #
# > Purpose of the script(s)
# # : Run regressions for various models with a unrestricted sample.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(stargazer)
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
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")
PATH_TO.SAVE_RESULTS <- paste(
  DIR_TO.SAVE_RESULTS,
  "DT_For-Regression_RD-Design_Regression-Results_Un-Restricted-Sample.parquet",
  sep = "/"
)


# ------- Define parameter(s) -------
# # (NOT Applicable)


# ------- Define function(s) -------
# # 1. Run Regressions by Bandwidth
get_reg.results_by.bw <- function(bw) {
  tmp_results_in.list <-
    mapply(
      FUN = summarize_reg.results_felm_for.unrestricted.sample,
      model_no.clustering = list_rd_models_no.clustering,
      model_clustering = list_rd_models_clustering,
      model.name = names(list_rd_models_no.clustering),
      bw = bw,
      SIMPLIFY = FALSE
    )
  tmp_dt <- rbindlist(tmp_results_in.list)

  return(tmp_dt)
}

# # 2. Summarize Regression Results given Models and Bandwidth
summarize_reg.results_felm_for.unrestricted.sample <-
  function(model_no.clustering, model_clustering, model.name, bw) {
    print(paste0("Estimating: ", model.name, " for BW: ", bw))

    tmp_functional.form <- str_extract(model.name, "(Linear)|(Square)|(Cubic)")
    tmp_model <- str_replace(model.name, ",.+$", "")

    tmp_results_no.clustering <-
      estimate.wBW_felm(model = model_no.clustering, bw = bw)
    tmp_results_clustering <-
      estimate.wBW_felm(model = model_clustering, bw = bw)

    tmp_dt_no.clustering <-
      extract_reg.results_felm_for.unrestricted.sample(
        felm.obj = tmp_results_no.clustering,
        is_clustering = FALSE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        bw = bw
      )
    tmp_dt_clustering <-
      extract_reg.results_felm_for.unrestricted.sample(
        felm.obj = tmp_results_clustering,
        is_clustering = TRUE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        bw = bw
      )

    cols_on <- c("functional.form", "model", "bw_in.str", "var_independent")
    tmp_dt <-
      tmp_dt_no.clustering[
        tmp_dt_clustering[, .SD, .SDcols = c(cols_on, "se_cluster")],
        on = cols_on
      ]
    tmp_dt[, se_cluster := i.se_cluster]
    tmp_dt[, i.se_cluster := NULL]

    return(tmp_dt)
  }

# # 3. Extract Regression Results from FELM Object obtained from
# #    a Unrestricted Sample
extract_reg.results_felm_for.unrestricted.sample <-
  function(felm.obj, is_clustering, functional.form, model, bw) {
    # ## Independent Variable
    tmp_dt_vars_independent <-
      felm.obj$coefficients %>% row.names(.) %>% as.data.table(.) %>%
        setnames(., ".", "var_independent")
    tmp_n_indepent.var <- tmp_dt_vars_independent[, .N]
    # ## Dependent Variable
    tmp_var_dependent <- felm.obj$lhs
    # ## Method
    tmp_method <-
      felm.obj$call %>% .[[1]] %>% as.character(.)
    # ## Create a DT including base information
    tmp_dt_base <- cbind(
      data.table(
        functional.form = rep(functional.form, times = tmp_n_indepent.var),
        model = rep(model, times = tmp_n_indepent.var),
        method = rep(tmp_method, times = tmp_n_indepent.var),
        is_clustering = rep(is_clustering, times = tmp_n_indepent.var),
        var_dependent = rep(tmp_var_dependent, times = tmp_n_indepent.var)
      ),
      tmp_dt_vars_independent
    )
    # ## Add data fields about bandwidth
    tmp_dt_base[
      ,
      `:=` (
        bw_in.str = paste0(bw, "%"),
        bw_in.percent = bw
      )
    ]
    # ## Estimated Coefficients
    tmp_dt_coef <-
      felm.obj$coefficients %>% as.data.table(.) %>%
        setnames(., tmp_var_dependent, "estimates")
    # ## Estimated Standard Errors
    if (is_clustering) {
      tmp_dt_se_standard <-
        felm.obj %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        felm.obj %>% summary(., robust = TRUE) %>%
          .$coefficients %>% .[, "Cluster s.e."] %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_hetero")
    } else {
      tmp_dt_se_standard <-
        felm.obj %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        felm.obj %>% summary(., robust = TRUE) %>%
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

    return(tmp_dt_binded)
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
# ------- Generate Objects that will be used later -------
# # 1. Generate Lists of Models
# # 1.1. For Models without Clustering
list_rd_models_no.clustering <- list(
  `Without FEs and Controls, Linear` =
    felm_rd_wo.fes_linear_no.controls_no.clustering,
  `Without FEs, Linear` =
    felm_rd_wo.fes_linear_no.clustering,
  `With IDs FEs, Linear` =
    felm_rd_ids.fes_linear_no.clustering,
  `With BYM FEs, Linear` =
    felm_rd_bym.fes_linear_no.clustering,
  `With Both FEs, Linear` =
    felm_rd_both.fes_linear_no.clustering,
  `Interaction without FEs and Controls, Linear` =
    felm_rd_wo.fes_linear.w.inter_no.controls_no.clustering,
  `Interaction without FEs, Linear` =
    felm_rd_wo.fes_linear.w.inter_no.clustering,
  `Interaction with IDs FEs, Linear` =
    felm_rd_ids.fes_linear.w.inter_no.clustering,
  `Interaction with BYM FEs, Linear` =
    felm_rd_bym.fes_linear.w.inter_no.clustering,
  `Interaction with Both FEs, Linear` =
    felm_rd_both.fes_linear.w.inter_no.clustering,
  `Without FEs and Controls, Square` =
    felm_rd_wo.fes_square_no.controls_no.clustering,
  `Without FEs, Square` =
    felm_rd_wo.fes_square_no.clustering,
  `With IDs FEs, Square` =
    felm_rd_ids.fes_square_no.clustering,
  `With BYM FEs, Square` =
    felm_rd_bym.fes_square_no.clustering,
  `With Both FEs, Square` =
    felm_rd_both.fes_square_no.clustering,
  `Interaction without FEs and Controls, Square` =
    felm_rd_wo.fes_square.w.inter_no.controls_no.clustering,
  `Interaction without FEs, Square` =
    felm_rd_wo.fes_square.w.inter_no.clustering,
  `Interaction with IDs FEs, Square` =
    felm_rd_ids.fes_square.w.inter_no.clustering,
  `Interaction with BYM FEs, Square` =
    felm_rd_bym.fes_square.w.inter_no.clustering,
  `Interaction with Both FEs, Square` =
    felm_rd_both.fes_square.w.inter_no.clustering,
  `Without FEs and Controls, Cubic` =
    felm_rd_wo.fes_cubic_no.controls_no.clustering,
  `Without FEs, Cubic` =
    felm_rd_wo.fes_cubic_no.clustering,
  `With IDs FEs, Cubic` =
    felm_rd_ids.fes_cubic_no.clustering,
  `With BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic_no.clustering,
  `With Both FEs, Cubic` =
    felm_rd_both.fes_cubic_no.clustering,
  `Interaction without FEs and Controls, Cubic` =
    felm_rd_wo.fes_cubic.w.inter_no.controls_no.clustering,
  `Interaction without FEs, Cubic` =
    felm_rd_wo.fes_cubic.w.inter_no.clustering,
  `Interaction with IDs FEs, Cubic` =
    felm_rd_ids.fes_cubic.w.inter_no.clustering,
  `Interaction with BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic.w.inter_no.clustering,
  `Interaction with Both FEs, Cubic` =
    felm_rd_both.fes_cubic.w.inter_no.clustering
)

# # 1.2. For Models with Clustering
list_rd_models_clustering <- list(
  `Without FEs and Controls, Linear` =
    felm_rd_wo.fes_linear_no.controls_clustering,
  `Without FEs, Linear` =
    felm_rd_wo.fes_linear_clustering,
  `With IDs FEs, Linear` =
    felm_rd_ids.fes_linear_clustering,
  `With BYM FEs, Linear` =
    felm_rd_bym.fes_linear_clustering,
  `With Both FEs, Linear` =
    felm_rd_both.fes_linear_clustering,
  `Interaction without FEs and Controls, Linear` =
    felm_rd_wo.fes_linear.w.inter_no.controls_clustering,
  `Interaction without FEs, Linear` =
    felm_rd_wo.fes_linear.w.inter_clustering,
  `Interaction with IDs FEs, Linear` =
    felm_rd_ids.fes_linear.w.inter_clustering,
  `Interaction with BYM FEs, Linear` =
    felm_rd_bym.fes_linear.w.inter_clustering,
  `Interaction with Both FEs, Linear` =
    felm_rd_both.fes_linear.w.inter_clustering,
  `Without FEs and Controls, Square` =
    felm_rd_wo.fes_square_no.controls_clustering,
  `Without FEs, Square` =
    felm_rd_wo.fes_square_clustering,
  `With IDs FEs, Square` =
    felm_rd_ids.fes_square_clustering,
  `With BYM FEs, Square` =
    felm_rd_bym.fes_square_clustering,
  `With Both FEs, Square` =
    felm_rd_both.fes_square_clustering,
  `Interaction without FEs and Controls, Square` =
    felm_rd_wo.fes_square.w.inter_no.controls_clustering,
  `Interaction without FEs, Square` =
    felm_rd_wo.fes_square.w.inter_clustering,
  `Interaction with IDs FEs, Square` =
    felm_rd_ids.fes_square.w.inter_clustering,
  `Interaction with BYM FEs, Square` =
    felm_rd_bym.fes_square.w.inter_clustering,
  `Interaction with Both FEs, Square` =
    felm_rd_both.fes_square.w.inter_clustering,
  `Without FEs and Controls, Cubic` =
    felm_rd_wo.fes_cubic_no.controls_clustering,
  `Without FEs, Cubic` =
    felm_rd_wo.fes_cubic_clustering,
  `With IDs FEs, Cubic` =
    felm_rd_ids.fes_cubic_clustering,
  `With BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic_clustering,
  `With Both FEs, Cubic` =
    felm_rd_both.fes_cubic_clustering,
  `Interaction without FEs and Controls, Cubic` =
    felm_rd_wo.fes_cubic.w.inter_no.controls_clustering,
  `Interaction without FEs, Cubic` =
    felm_rd_wo.fes_cubic.w.inter_clustering,
  `Interaction with IDs FEs, Cubic` =
    felm_rd_ids.fes_cubic.w.inter_clustering,
  `Interaction with BYM FEs, Cubic` =
    felm_rd_bym.fes_cubic.w.inter_clustering,
  `Interaction with Both FEs, Cubic` =
    felm_rd_both.fes_cubic.w.inter_clustering
)

# # 2. Make a list of Bandwidths
list_bws <- as.list(
  c(seq(1, 20, by = 1), seq(30, 50, by = 5), seq(60, 100, by = 10))
)


# ------- Run Regressions -------
# # 1. Create a List by Running Regressions
list_rd_results_by.bw <- lapply(X = list_bws, FUN = get_reg.results_by.bw)

# # 2. Create a DT from the list obtained by Running Regressions
dt_rd_results <- rbindlist(list_rd_results_by.bw)


# --------------------------------------------------
# Save the DT created
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
  dt_rd_results,
  sink = PATH_TO.SAVE_RESULTS,
  version = "1.0",
  compression = "gzip",
  use_dictionary = TRUE
)

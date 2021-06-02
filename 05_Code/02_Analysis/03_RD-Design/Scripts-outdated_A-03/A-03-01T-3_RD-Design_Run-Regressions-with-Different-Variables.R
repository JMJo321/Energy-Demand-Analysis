# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02X-2
# #
# > Purpose of the script(s)
# # : To

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(lfe)
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
# # 1.1. Regression Results
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <-
  "DT_For-Regression_RD-Design_Reduced.RData"
PATH_TO.LOAD_RD <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD,
  sep = "/"
)

# # # 2. Paths at which Output will be saved
# DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
# DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")
# FILE_TO.SAVE_RESULTS <-
#   "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month_Tier-2.parquet"
# PATH_TO.SAVE_RESULTS <-
#   paste(DIR_TO.SAVE_RESULTS, FILE_TO.SAVE_RESULTS, sep = "/")


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005


# ------- Define function(s) -------
# # 1. Run Regressions
# # 1.1. By Month of Forward Response
run.regressions <- function(
  rate.codes, year_lower, year_upper, suffix_tier,
  season = NULL, month_response = NULL
) {
  return(
    lapply(
      list_bws, get_reg.results_by.bw,
      rate.codes = rate.codes,
      season = season, month_response = month_response,
      year_lower = year_lower, year_upper = year_upper,
      suffix_tier = suffix_tier
    ) %>% rbindlist(.)
  )
}

# # 1.2. By Bandwidth
get_reg.results_by.bw <- function(
  bw, rate.codes, year_lower, year_upper, suffix_tier,
  season = NULL, month_response = NULL
) {
    tmp_results_in.list <-
      mapply(
        FUN = summarize_reg.results_felm_for.subsample,
        model_no.clustering = list_rd_models_for.subsample_no.clustering,
        model_clustering = list_rd_models_for.subsample_clustering,
        model.name = names(list_rd_models_for.subsample_no.clustering),
        suffix = list_suffixes,
        # ## Note: Those four lists should be defined separately.
        MoreArgs = list(
          bw = bw, rate.codes = rate.codes,
          season = season, month_response = month_response,
          year_lower = year_lower, year_upper = year_upper,
          suffix_tier = suffix_tier
        ),
        SIMPLIFY = FALSE
      )
    tmp_dt <- rbindlist(tmp_results_in.list)

    return(tmp_dt)
}


# # 2. Summarize Regression Results
summarize_reg.results_felm_for.subsample <-
  function(
    model_no.clustering, model_clustering, model.name, bw, rate.codes,
    season = NULL, month_response = NULL,
    year_lower, year_upper, suffix_tier, suffix_period
  ) {
    # ## Print a message
    if (is.null(season)) {tmp_str <- month_response} else {tmp_str <- season}
    print(
      paste0(
        "Estimating: ", model.name, " for BW-", bw, ", ", tmp_str,
        " and ", str_to_title(suffix_period)
      )
    )

    # ## Create temporary objects that will be used later
    tmp_functional.form <- str_extract(model.name, "(Linear)|(Square)|(Cubic)")
    tmp_model <- str_replace(model.name, ",.+$", "")
    tmp_rate.codes <- str_c(rate.codes, collapse = ", ")
    tmp_term_btw.periods <-
      str_extract(model.name, "P[0-9]$") %>% str_extract("[0-9]$") %>%
        as.numeric(.)
    tmp_condition <- paste0(
        "!is.na(kwh_daily.avg_", suffix_period, ") & is.finite(kwh_daily.avg_",
        suffix_period, ")"
      )

    # ## Change independent variables in models
    tmp_exp_treat.var_from <- paste(
      "is_treated_t1", suffix_period, sep = "_"
    )
    tmp_exp_treat.var_to <- paste(
      "is_treated", suffix_tier, suffix_period, sep = "_"
    ) %>% str2lang(.) %>% as.list(.)
    names(tmp_exp_treat.var_to) <- tmp_exp_treat.var_from
    tmp_exp_running.var_from <- paste(
      "kwh_total_in.percent_normalize_t1", suffix_period, sep = "_"
    )
    tmp_exp_running.var_to <- paste(
      "kwh_total_in.percent_normalize", suffix_tier, suffix_period, sep = "_"
    ) %>% str2lang(.) %>% as.list(.)
    names(tmp_exp_running.var_to) <- tmp_exp_running.var_from

    TMP_model_for.reg_no.clustering <-
      do.call("substitute", list(model_no.clustering, tmp_exp_treat.var_to))
    tmp_model_for.reg_no.clustering <-
      do.call(
        "substitute",
        list(TMP_model_for.reg_no.clustering, tmp_exp_running.var_to)
      ) %>% as.formula(.)

    TMP_model_for.reg_clustering <-
      do.call("substitute", list(model_clustering, tmp_exp_treat.var_to))
    tmp_model_for.reg_clustering <-
      do.call(
        "substitute",
        list(TMP_model_for.reg_clustering, tmp_exp_running.var_to)
      ) %>% as.formula(.)

    # ##
    tmp_dt_no.clustering <-
      extract_reg.results_felm_for.subsample(
        felm.obj = estimate.wBW_terms_felm_for.subsample(
          dt = dt_for.reg[eval(parse(text = tmp_condition))],
          model = tmp_model_for.reg_no.clustering,
          bw = bw, rate.codes = rate.codes,
          season = season, month_response = month_response,
          year_lower = year_lower, year_upper = year_upper,
          suffix_tier = suffix_tier, suffix_period = suffix_period
        ),
        is_clustering = FALSE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        rate.code = tmp_rate.codes,
        season = season,
        month_response = month_response,
        term_btw.periods = tmp_term_btw.periods,
        bw = bw
      )
    tmp_dt_clustering <-
      extract_reg.results_felm_for.subsample(
        felm.obj = estimate.wBW_terms_felm_for.subsample(
          dt = dt_for.reg[eval(parse(text = tmp_condition))],
          model = tmp_model_for.reg_clustering,
          bw = bw, rate.codes = rate.codes,
          season = season, month_response = month_response,
          year_lower = year_lower, year_upper = year_upper,
          suffix_tier = suffix_tier, suffix_period = suffix_period
        ),
        is_clustering = TRUE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        rate.code = tmp_rate.codes,
        season = season,
        month_response = month_response,
        term_btw.periods = tmp_term_btw.periods,
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
    tmp_dt[, is_clustering := NULL]

    return(tmp_dt)
  }


# # 3. Extract Regression Results from FELM Object obtained from
# #    a Unrestricted Sample
extract_reg.results_felm_for.subsample <-
  function(
    felm.obj, is_clustering, functional.form, model, rate.code,
    season = NULL, month_response = NULL, term_btw.periods, bw
  ) {
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
    if (is.na(season) | is.null(season)) {
      tmp_season <- NA
    } else {
      tmp_season <- season
    }
    if (is.null(month_response)) {
      tmp_month_response <- NA
    } else {
      tmp_month_response <- month_response
    }

    tmp_dt_base <- cbind(
      data.table(
        functional.form = rep(functional.form, times = tmp_n_indepent.var),
        model = rep(model, times = tmp_n_indepent.var),
        rate.code = rep(rate.code, times = tmp_n_indepent.var),
        season = rep(tmp_season, times = tmp_n_indepent.var),
        month_response = rep(tmp_month_response, times = tmp_n_indepent.var),
        term_btw.periods = rep(term_btw.periods, times = tmp_n_indepent.var),
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
# X
# --------------------------------------------------
# ------- X -------
load(PATH_TO.LOAD_RD)


# --------------------------------------------------
# Define Regression Models
# --------------------------------------------------
# ------- Read a R script containing regression models -------
PATH_MODELS <- paste(
  PATH_PROJ, "05_Code",
  "M-Energy-Demand-Analysis_Regression-Models_RD-Design.R",
  sep = "/"
)
source(PATH_MODELS)


# --------------------------------------------------
# Run Regressions: Linear Models
# --------------------------------------------------
# ------- Create objects that will be used later -------
# # 1. Make a list of bandwidths
# list_bws <- as.list(c(seq(1, 10, by = 1), seq(20, 50, by = 10)))
list_bws <- as.list(c(3, 5, 7))

# # 2. Make a list of suffixes
suffixes_forward <-
  c("period0", "periodm1", "periodm2", "periodm3", "periodm4")
suffixes_backward <-
  c("periodp1", "periodp2", "periodp3", "periodp4", "periodp5")


# # ------- Run Regressions: Forward Responses -------
# # # 1. Make a list of models
# # # 1.1. For models with clustering
# list_rd_models_for.subsample_clustering <- list(
#   `With BYM FEs, Linear, P1` =
#     felm_rd_bym.fes_linear_clustering_forward_p1,
#   `Interaction with BYM FEs, Linear, P1` =
#     felm_rd_bym.fes_linear.w.inter_clustering_forward_p1,
#   `With BYM FEs, Linear, P2` =
#     felm_rd_bym.fes_linear_clustering_forward_p2,
#   `Interaction with BYM FEs, Linear, P2` =
#     felm_rd_bym.fes_linear.w.inter_clustering_forward_p2,
#   `With BYM FEs, Linear, P3` =
#     felm_rd_bym.fes_linear_clustering_forward_p3,
#   `Interaction with BYM FEs, Linear, P3` =
#     felm_rd_bym.fes_linear.w.inter_clustering_forward_p3,
#   `With BYM FEs, Linear, P4` =
#     felm_rd_bym.fes_linear_clustering_forward_p4,
#   `Interaction with BYM FEs, Linear, P4` =
#     felm_rd_bym.fes_linear.w.inter_clustering_forward_p4,
#   `With BYM FEs, Linear, P5` =
#     felm_rd_bym.fes_linear_clustering_forward_p5,
#   `Interaction with BYM FEs, Linear, P5` =
#     felm_rd_bym.fes_linear.w.inter_clustering_forward_p5
# )
# # # 1.2. For models without clustering
# list_rd_models_for.subsample_no.clustering <- list(
#   `With BYM FEs, Linear, P1` =
#     felm_rd_bym.fes_linear_no.clustering_forward_p1,
#   `Interaction with BYM FEs, Linear, P1` =
#     felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p1,
#   `With BYM FEs, Linear, P2` =
#     felm_rd_bym.fes_linear_no.clustering_forward_p2,
#   `Interaction with BYM FEs, Linear, P2` =
#     felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p2,
#   `With BYM FEs, Linear, P3` =
#     felm_rd_bym.fes_linear_no.clustering_forward_p3,
#   `Interaction with BYM FEs, Linear, P3` =
#     felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p3,
#   `With BYM FEs, Linear, P4` =
#     felm_rd_bym.fes_linear_no.clustering_forward_p4,
#   `Interaction with BYM FEs, Linear, P4` =
#     felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p4,
#   `With BYM FEs, Linear, P5` =
#     felm_rd_bym.fes_linear_no.clustering_forward_p5,
#   `Interaction with BYM FEs, Linear, P5` =
#     felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p5
# )
#
# # 2. Run Regressions
list_suffixes <- rep(suffixes_forward, each = 2)
list_seasons <- list("Summer", "Winter", NA)
# # # 2.1. For RSGH
# dt_forward_linear_g.code <- lapply(
#   list_seasons, run.regressions,
#   rate.codes = c("RSGH"), month_response = NULL,
#   year_lower = YEAR_LOWER, year_upper = YEAR_UPPER, suffix_tier = "t2"
# ) %>% rbindlist(.)
# # # 2.2. For RSCH & RSEH
# dt_forward_linear_ce.codes <- lapply(
#   list_seasons, run.regressions,
#   rate.codes = c("RSCH", "RSEH"), month_response = NULL,
#   year_lower = YEAR_LOWER, year_upper = YEAR_UPPER, suffix_tier = "t2"
# ) %>% rbindlist(.)


# ------- Run Regressions: Backward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear_clustering_backward_p1,
  `Interaction with BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p1,
  `With BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear_clustering_backward_p2,
  `Interaction with BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p2,
  `With BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear_clustering_backward_p3,
  `Interaction with BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p3,
  `With BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear_clustering_backward_p4,
  `Interaction with BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p4,
  `With BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear_clustering_backward_p5,
  `Interaction with BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear_no.clustering_backward_p1,
  `Interaction with BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p1,
  `With BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear_no.clustering_backward_p2,
  `Interaction with BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p2,
  `With BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear_no.clustering_backward_p3,
  `Interaction with BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p3,
  `With BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear_no.clustering_backward_p4,
  `Interaction with BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p4,
  `With BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear_no.clustering_backward_p5,
  `Interaction with BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p5
)

# # 2. Run Regressions
list_suffixes <- rep(suffixes_backward, each = 2)
# # 2.1. For RSGH
dt_backward_linear_g.code <- lapply(
  list_seasons, run.regressions,
  rate.codes = c("RSGH"), month_response = NULL,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER, suffix_tier = "t1"
) %>% rbindlist(.)
# # # 2.2. For RSCH & RSEH
# dt_backward_linear_ce.codes <- lapply(
#   list_seasons, run.regressions,
#   rate.codes = c("RSCH", "RSEH"), month_response = NULL,
#   year_lower = YEAR_LOWER, year_upper = YEAR_UPPER, suffix_tier = "t2"
# ) %>% rbindlist(.)


# --------------------------------------------------
# Create a DT from Regression Results
# --------------------------------------------------
# ------- Make a Combined DT from Results -------
# # 1. Make a DT by combining DTs including regression results
dt_rd_results_by.term_t2 <- rbind(
  dt_forward_linear_g.code[, category := "Forward"],
  # dt_forward_linear_ce.codes[, category := "Forward"],
  dt_backward_linear_g.code[, category := "Backward"]
  # ,
  # dt_backward_linear_ce.codes[, category := "Backward"],
  # dt_forward_square_g.code[, category := "Forward"],
  # dt_forward_square_ce.codes[, category := "Forward"],
  # dt_backward_square_g.code[, category := "Backward"],
  # dt_backward_square_ce.codes[, category := "Backward"]
)


# ------- Modify the Combined DT from Results -------
# # 1. Change a data field's values
list_var_independent <- list(
  `is_treated_t2_period0TRUE` =
    "1[Treated]:0",
  `kwh_total_in.percent_normalize_t2_period0` =
    "NC:0",
  `is_treated_t2_period0TRUE:kwh_total_in.percent_normalize_t2_period0` =
    "NC:0 * 1[Treated]:0",
  `is_treated_t2_periodm1TRUE` =
    "1[Treated]:-1",
  `kwh_total_in.percent_normalize_t2_periodm1` =
    "NC:-1",
  `is_treated_t2_periodm1TRUE:kwh_total_in.percent_normalize_t2_periodm1` =
    "NC:-1 * 1[Treated]:-1",
  `is_treated_t2_periodm2TRUE` =
    "1[Treated]:-2",
  `kwh_total_in.percent_normalize_t2_periodm2` =
    "NC:-2",
  `is_treated_t2_periodm2TRUE:kwh_total_in.percent_normalize_t2_periodm2` =
    "NC:-2 * 1[Treated]:-2",
  `is_treated_t2_periodm3TRUE` =
    "1[Treated]:-3",
  `kwh_total_in.percent_normalize_t2_periodm3` =
    "NC:-3",
  `is_treated_t2_periodm3TRUE:kwh_total_in.percent_normalize_t2_periodm3` =
    "NC:-3 * 1[Treated]:-3",
  `is_treated_t2_periodm4TRUE` =
    "1[Treated]:-4",
  `kwh_total_in.percent_normalize_t2_periodm4` =
    "NC:-4",
  `is_treated_t2_periodm4TRUE:kwh_total_in.percent_normalize_t2_periodm4` =
    "NC:-4 * 1[Treated]:-4",
  `is_treated_t2_periodp1TRUE` =
    "1[Treated]:+1",
  `kwh_total_in.percent_normalize_t2_periodp1` =
    "NC:+1",
  `I(kwh_total_in.percent_normalize_t2_periodp1^2)` =
    "Square of NC:+1",
  `is_treated_t2_periodp1TRUE:kwh_total_in.percent_normalize_t2_periodp1` =
    "NC:+1 * 1[Treated]:+1",
   `is_treated_t2_periodp1TRUE:I(kwh_total_in.percent_normalize_t2_periodp1^2)` =
     "Square of NC:+1 * 1[Treated]:+1",
  `is_treated_t2_periodp2TRUE` =
    "1[Treated]:+2",
  `kwh_total_in.percent_normalize_t2_periodp2` =
    "NC:+2",
  `I(kwh_total_in.percent_normalize_t2_periodp2^2)` =
    "Square of NC:+2",
  `is_treated_t2_periodp2TRUE:kwh_total_in.percent_normalize_t2_periodp2` =
    "NC:+2 * 1[Treated]:+2",
  `is_treated_t2_periodp2TRUE:I(kwh_total_in.percent_normalize_t2_periodp2^2)` =
     "Square of NC:+2 * 1[Treated]:+2",
  `is_treated_t2_periodp3TRUE` =
    "1[Treated]:+3",
  `kwh_total_in.percent_normalize_t2_periodp3` =
    "NC:+3",
  `I(kwh_total_in.percent_normalize_t2_periodp3^2)` =
    "Square of NC:+3",
  `is_treated_t2_periodp3TRUE:kwh_total_in.percent_normalize_t2_periodp3` =
    "NC:+3 * 1[Treated]:+3",
  `is_treated_t2_periodp3TRUE:I(kwh_total_in.percent_normalize_t2_periodp3^2)` =
     "Square of NC:+3 * 1[Treated]:+3",
  `is_treated_t2_periodp4TRUE` =
    "1[Treated]:+4",
  `kwh_total_in.percent_normalize_t2_periodp4` =
    "NC:+4",
  `I(kwh_total_in.percent_normalize_t2_periodp4^2)` =
    "Square of NC:+4",
  `is_treated_t2_periodp4TRUE:kwh_total_in.percent_normalize_t2_periodp4` =
    "NC:+4 * 1[Treated]:+4",
  `is_treated_t2_periodp4TRUE:I(kwh_total_in.percent_normalize_t2_periodp4^2)` =
     "Square of NC:+4 * 1[Treated]:+4",
  `is_treated_t2_periodp5TRUE` =
    "1[Treated]:+5",
  `kwh_total_in.percent_normalize_t2_periodp5` =
    "NC:+5",
  `I(kwh_total_in.percent_normalize_t2_periodp5^2)` =
    "Square of NC:+5",
  `is_treated_t2_periodp5TRUE:kwh_total_in.percent_normalize_t2_periodp5` =
    "NC:+5 * 1[Treated]:+5",
  `is_treated_t2_periodp5TRUE:I(kwh_total_in.percent_normalize_t2_periodp5^2)` =
     "Square of NC:+5 * 1[Treated]:+5",
  `cdd_daily.avg_period` =
    "Daily Average CDDs",
  `hdd_daily.avg_period` =
    "Daily Average HDDs",
  `(Intercept)` =
    "(Constant)"
)
dt_rd_results_by.term_t2[
  ,
  var_independent :=
    sapply(var_independent, function(x) list_var_independent[[x]])
]

# # 2. Change data type: To Factor
# # 2.1. Make objects including factors' levels
order_functional.form <- c("Linear", "Square", "Cubic")
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
order_bw <- paste0(list_bws, "%")
order_var_independent <- unlist(list_var_independent, use.names = FALSE)
# # 2.2. Convert data type
dt_rd_results_by.term_t2[
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
cols_se <-
  names(dt_rd_results_by.term_t2)[str_detect(names(dt_rd_results_by.term_t2), "^se_")]
dt_rd_results_by.term_t2[
  , se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se
]

# # 4. Change the order of columns
setcolorder(
  dt_rd_results_by.term_t2,
  c(1:4, length(names(dt_rd_results_by.term_t2)) - 1)
)


# --------------------------------------------------
# Save the DT modified
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
  dt_rd_results_by.term_t2,
  sink = PATH_TO.SAVE_RESULTS,
  version = "1.0",
  compression = "snappy",
  use_dictionary = TRUE
)

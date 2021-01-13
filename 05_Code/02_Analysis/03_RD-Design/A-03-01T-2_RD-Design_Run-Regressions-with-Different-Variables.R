# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02T-2
# #
# > Purpose of the script(s)
# # : To generate a DT by running regressions with different dependent/running
# #   variables.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(stargazer)
library(lfe)
library(zoo)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
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
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design_Reduced.RData"
PATH_TO.LOAD_RD <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep = "/"
)

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")
FILE_TO.SAVE_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month.parquet"
PATH_TO.SAVE_RESULTS <-
  paste(DIR_TO.SAVE_RESULTS, FILE_TO.SAVE_RESULTS, sep = "/")


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005
# ## Note:
# ## There are no observation in summer in 2004.


# ------- Define function(s) -------
# # 1. Run Regressions
# # 1.1. By Bandwidth
get_reg.results_by.bw <- function(
  bw, rate.codes, season, year_lower, year_upper
                                  #, month_response
) {
    tmp_results_in.list <-
      mapply(
        FUN = summarize_reg.results_felm_for.subsample,
        model_no.clustering = list_rd_models_for.subsample_no.clustering,
        model_clustering = list_rd_models_for.subsample_clustering,
        model.name = names(list_rd_models_for.subsample_no.clustering),
        suffix = list_suffixes,
        # ## Note: Those four lists will be defined later.
        MoreArgs = list(
          bw = bw, rate.codes = rate.codes, season = season,
          year_lower = year_lower, year_upper = year_upper
          #              ,
          #month_response = month_response
        ),
        SIMPLIFY = FALSE
      )
    tmp_dt <- rbindlist(tmp_results_in.list)

    return(tmp_dt)
}
# # 1.2. By Month of Forward Response
run.regressions_by.month_response <- function(
  rate.codes, season, year_lower, year_upper
  #rate.codes, season, month_response, year_lower, year_upper
) {
  return(
    lapply(
      list_bws, get_reg.results_by.bw,
      rate.codes = rate.codes, season = season,
      #month_response = month_response,
      year_lower = year_lower, year_upper = year_upper
    ) %>% rbindlist(.)
  )
}

# # 2. Summarize Regression Results given Models and Bandwidth
summarize_reg.results_felm_for.subsample <-
  function(
    model_no.clustering, model_clustering, model.name, bw, rate.codes, season,
    year_lower, year_upper, suffix
    #year_lower, year_upper, month_response, suffix
  ) {
    print(
      paste0(
        "Estimating: ", model.name, " for BW-", bw, ", ", season,
        " and ", str_to_title(suffix)
      )
    )

    tmp_functional.form <- str_extract(model.name, "(Linear)|(Square)|(Cubic)")
    tmp_model <- str_replace(model.name, ",.+$", "")
    tmp_rate.codes <- str_c(rate.codes, collapse = ", ")
    #tmp_month_response <- month_response
    tmp_term_btw.periods <-
      str_extract(model.name, "P[0-9]$") %>% str_extract("[0-9]$") %>%
        as.numeric(.)
    if (is.na(season)) {tmp_season <- NA} else {tmp_season <- season}
    tmp_condition <- paste0(
        "!is.na(kwh_daily.avg_", suffix, ") & is.finite(kwh_daily.avg_",
        suffix, ")"
      )

    tmp_results_no.clustering <-
      estimate.wBW_terms_felm_for.seasonal.subsample(
        dt = dt_for.reg[eval(parse(text = tmp_condition))],
        model = model_no.clustering, bw = bw, rate.codes = rate.codes,
        year_lower = year_lower, year_upper = year_upper,
        season = season, suffix = suffix
      )
    tmp_results_clustering <-
      estimate.wBW_terms_felm_for.seasonal.subsample(
        dt = dt_for.reg[eval(parse(text = tmp_condition))],
        model = model_clustering, bw = bw, rate.codes = rate.codes,
        year_lower = year_lower, year_upper = year_upper,
        season = season, suffix = suffix
      )

    tmp_dt_no.clustering <-
      extract_reg.results_felm_for.subsample(
        felm.obj = tmp_results_no.clustering,
        is_clustering = FALSE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        rate.code = tmp_rate.codes,
        #month_response = tmp_month_response,
        season = tmp_season,
        term_btw.periods = tmp_term_btw.periods,
        bw = bw
      )
    tmp_dt_clustering <-
      extract_reg.results_felm_for.subsample(
        felm.obj = tmp_results_clustering,
        is_clustering = TRUE,
        functional.form = tmp_functional.form,
        model = tmp_model,
        rate.code = tmp_rate.codes,
        #month_response = tmp_month_response,
        season = tmp_season,
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
    felm.obj, is_clustering, functional.form, model, rate.code, season,
    term_btw.periods, bw
    #month_response, term_btw.periods, bw
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
    tmp_dt_base <- cbind(
      data.table(
        functional.form = rep(functional.form, times = tmp_n_indepent.var),
        model = rep(model, times = tmp_n_indepent.var),
        rate.code = rep(rate.code, times = tmp_n_indepent.var),
        season = rep(season, times = tmp_n_indepent.var),
        #month_response = rep(
        #  month_response, times = tmp_n_indepent.var
        #),
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
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
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
list_bws <- as.list(c(seq(10, 50, by = 10)))

# # 2. Make a list of month
#list_month_forward.response <- as.list(c(1:12))
#list_month_backward.response <- as.list(c(1:12))

#list_month_forward.response <- as.list(c(1:1))
#list_month_backward.response <- as.list(c(1:1))

suffixes_forward <-
  c("period0", "periodm1", "periodm2", "periodm3", "periodm4")
suffixes_backward <-
  c("periodp1", "periodp2", "periodp3", "periodp4", "periodp5")


# ------- Run Regressions: Forward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear_clustering_forward_p1,
  `Interaction with BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p1,
  `With BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear_clustering_forward_p2,
  `Interaction with BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p2,
  `With BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear_clustering_forward_p3,
  `Interaction with BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p3,
  `With BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear_clustering_forward_p4,
  `Interaction with BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p4,
  `With BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear_clustering_forward_p5,
  `Interaction with BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear_no.clustering_forward_p1,
  `Interaction with BYM FEs, Linear, P1` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p1,
  `With BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear_no.clustering_forward_p2,
  `Interaction with BYM FEs, Linear, P2` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p2,
  `With BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear_no.clustering_forward_p3,
  `Interaction with BYM FEs, Linear, P3` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p3,
  `With BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear_no.clustering_forward_p4,
  `Interaction with BYM FEs, Linear, P4` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p4,
  `With BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear_no.clustering_forward_p5,
  `Interaction with BYM FEs, Linear, P5` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p5
)

# # 2. Run Regressions
list_suffixes <- rep(suffixes_forward, each = 2)
list_seasons <- list("Summer", "Winter", NA)
dt_forward_linear_g.code <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)
dt_forward_linear_ce.codes <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)


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
dt_backward_linear_g.code <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)
dt_backward_linear_ce.codes <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)


# --------------------------------------------------
# Run Regressions: Square Models
# --------------------------------------------------
# ------- Run Regressions: Forward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Square, P1` =
    felm_rd_bym.fes_square_clustering_forward_p1,
  `Interaction with BYM FEs, Square, P1` =
    felm_rd_bym.fes_square.w.inter_clustering_forward_p1,
  `With BYM FEs, Square, P2` =
    felm_rd_bym.fes_square_clustering_forward_p2,
  `Interaction with BYM FEs, Square, P2` =
    felm_rd_bym.fes_square.w.inter_clustering_forward_p2,
  `With BYM FEs, Square, P3` =
    felm_rd_bym.fes_square_clustering_forward_p3,
  `Interaction with BYM FEs, Square, P3` =
    felm_rd_bym.fes_square.w.inter_clustering_forward_p3,
  `With BYM FEs, Square, P4` =
    felm_rd_bym.fes_square_clustering_forward_p4,
  `Interaction with BYM FEs, Square, P4` =
    felm_rd_bym.fes_square.w.inter_clustering_forward_p4,
  `With BYM FEs, Square, P5` =
    felm_rd_bym.fes_square_clustering_forward_p5,
  `Interaction with BYM FEs, Square, P5` =
    felm_rd_bym.fes_square.w.inter_clustering_forward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Square, P1` =
    felm_rd_bym.fes_square_no.clustering_forward_p1,
  `Interaction with BYM FEs, Square, P1` =
    felm_rd_bym.fes_square.w.inter_no.clustering_forward_p1,
  `With BYM FEs, Square, P2` =
    felm_rd_bym.fes_square_no.clustering_forward_p2,
  `Interaction with BYM FEs, Square, P2` =
    felm_rd_bym.fes_square.w.inter_no.clustering_forward_p2,
  `With BYM FEs, Square, P3` =
    felm_rd_bym.fes_square_no.clustering_forward_p3,
  `Interaction with BYM FEs, Square, P3` =
    felm_rd_bym.fes_square.w.inter_no.clustering_forward_p3,
  `With BYM FEs, Square, P4` =
    felm_rd_bym.fes_square_no.clustering_forward_p4,
  `Interaction with BYM FEs, Square, P4` =
    felm_rd_bym.fes_square.w.inter_no.clustering_forward_p4,
  `With BYM FEs, Square, P5` =
    felm_rd_bym.fes_square_no.clustering_forward_p5,
  `Interaction with BYM FEs, Square, P5` =
    felm_rd_bym.fes_square.w.inter_no.clustering_forward_p5
)

# # 2. Run Regressions
list_suffixes <- rep(suffixes_forward, each = 2)
dt_forward_square_g.code <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)
dt_forward_square_ce.codes <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)


# ------- Run Regressions: Backward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Square, P1` =
    felm_rd_bym.fes_square_clustering_backward_p1,
  `Interaction with BYM FEs, Square, P1` =
    felm_rd_bym.fes_square.w.inter_clustering_backward_p1,
  `With BYM FEs, Square, P2` =
    felm_rd_bym.fes_square_clustering_backward_p2,
  `Interaction with BYM FEs, Square, P2` =
    felm_rd_bym.fes_square.w.inter_clustering_backward_p2,
  `With BYM FEs, Square, P3` =
    felm_rd_bym.fes_square_clustering_backward_p3,
  `Interaction with BYM FEs, Square, P3` =
    felm_rd_bym.fes_square.w.inter_clustering_backward_p3,
  `With BYM FEs, Square, P4` =
    felm_rd_bym.fes_square_clustering_backward_p4,
  `Interaction with BYM FEs, Square, P4` =
    felm_rd_bym.fes_square.w.inter_clustering_backward_p4,
  `With BYM FEs, Square, P5` =
    felm_rd_bym.fes_square_clustering_backward_p5,
  `Interaction with BYM FEs, Square, P5` =
    felm_rd_bym.fes_square.w.inter_clustering_backward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Square, P1` =
    felm_rd_bym.fes_square_no.clustering_backward_p1,
  `Interaction with BYM FEs, Square, P1` =
    felm_rd_bym.fes_square.w.inter_no.clustering_backward_p1,
  `With BYM FEs, Square, P2` =
    felm_rd_bym.fes_square_no.clustering_backward_p2,
  `Interaction with BYM FEs, Square, P2` =
    felm_rd_bym.fes_square.w.inter_no.clustering_backward_p2,
  `With BYM FEs, Square, P3` =
    felm_rd_bym.fes_square_no.clustering_backward_p3,
  `Interaction with BYM FEs, Square, P3` =
    felm_rd_bym.fes_square.w.inter_no.clustering_backward_p3,
  `With BYM FEs, Square, P4` =
    felm_rd_bym.fes_square_no.clustering_backward_p4,
  `Interaction with BYM FEs, Square, P4` =
    felm_rd_bym.fes_square.w.inter_no.clustering_backward_p4,
  `With BYM FEs, Square, P5` =
    felm_rd_bym.fes_square_no.clustering_backward_p5,
  `Interaction with BYM FEs, Square, P5` =
    felm_rd_bym.fes_square.w.inter_no.clustering_backward_p5
)

# # 2. Run Regressions
list_suffixes <- rep(suffixes_backward, each = 2)
dt_backward_square_g.code <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSGH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)
dt_backward_square_ce.codes <- lapply(
  list_seasons, run.regressions_by.month_response,
  rate.codes = c("RSCH", "RSEH"),
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
) %>% rbindlist(.)


# --------------------------------------------------
# Create a DT from Regression Results
# --------------------------------------------------
load("/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/Backward_Linear_RSGH_Tier-1.RData")
load("/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/04_Data/03_For-Analysis/01_RD-Design/DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month.RData")
# ------- Make a Combined DT from Results -------
# # 1. Make a DT by combining DTs including regression results
dt_rd_results_by.term_not.by.month <- rbind(
 # dt_forward_linear_g.code[, category := "Forward"],
 # dt_forward_linear_ce.codes[, category := "Forward"],
 dt_backward_linear_g.code[, category := "Backward"]
  # ,
 # dt_backward_linear_ce.codes[, category := "Backward"],
 # dt_forward_square_g.code[, category := "Forward"],
 # dt_forward_square_ce.codes[, category := "Forward"],
 # dt_backward_square_g.code[, category := "Backward"],
 # dt_backward_square_ce.codes[, category := "Backward"]
)

# # # 2. Save the combined DT in .RData format
# save(
#  dt_rd_results_by.term_not.by.month,
#  file = paste(
#    DIR_TO.SAVE_RESULTS,
#    "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month.RData",
#    sep = "/"
#  )
# )


# ------- Modify the Combined DT from Results -------
# # 1. Change a data field's values
list_var_independent <- list(
 `is_treated_period0TRUE` =
   "1[Treated]:0",
 `kwh_total_in.percent_normalize_period0` =
   "NC:0",
 `is_treated_period0TRUE:kwh_total_in.percent_normalize_period0` =
   "NC:0 * 1[Treated]:0",
 `is_treated_periodm1TRUE` =
   "1[Treated]:-1",
 `kwh_total_in.percent_normalize_periodm1` =
   "NC:-1",
 `is_treated_periodm1TRUE:kwh_total_in.percent_normalize_periodm1` =
   "NC:-1 * 1[Treated]:-1",
 `is_treated_periodm2TRUE` =
   "1[Treated]:-2",
 `kwh_total_in.percent_normalize_periodm2` =
   "NC:-2",
 `is_treated_periodm2TRUE:kwh_total_in.percent_normalize_periodm2` =
   "NC:-2 * 1[Treated]:-2",
 `is_treated_periodm3TRUE` =
   "1[Treated]:-3",
 `kwh_total_in.percent_normalize_periodm3` =
   "NC:-3",
 `is_treated_periodm3TRUE:kwh_total_in.percent_normalize_periodm3` =
   "NC:-3 * 1[Treated]:-3",
 `is_treated_periodm4TRUE` =
   "1[Treated]:-4",
 `kwh_total_in.percent_normalize_periodm4` =
   "NC:-4",
 `is_treated_periodm4TRUE:kwh_total_in.percent_normalize_periodm4` =
   "NC:-4 * 1[Treated]:-4",
 `is_treated_periodp1TRUE` =
   "1[Treated]:+1",
 `kwh_total_in.percent_normalize_periodp1` =
   "NC:+1",
 `I(kwh_total_in.percent_normalize_periodp1^2)` =
   "Square of NC:+1",
 `is_treated_periodp1TRUE:kwh_total_in.percent_normalize_periodp1` =
   "NC:+1 * 1[Treated]:+1",
  `is_treated_periodp1TRUE:I(kwh_total_in.percent_normalize_periodp1^2)` =
    "Square of NC:+1 * 1[Treated]:+1",
 `is_treated_periodp2TRUE` =
   "1[Treated]:+2",
 `kwh_total_in.percent_normalize_periodp2` =
   "NC:+2",
 `I(kwh_total_in.percent_normalize_periodp2^2)` =
   "Square of NC:+2",
 `is_treated_periodp2TRUE:kwh_total_in.percent_normalize_periodp2` =
   "NC:+2 * 1[Treated]:+2",
 `is_treated_periodp2TRUE:I(kwh_total_in.percent_normalize_periodp2^2)` =
    "Square of NC:+2 * 1[Treated]:+2",
 `is_treated_periodp3TRUE` =
   "1[Treated]:+3",
 `kwh_total_in.percent_normalize_periodp3` =
   "NC:+3",
 `I(kwh_total_in.percent_normalize_periodp3^2)` =
   "Square of NC:+3",
 `is_treated_periodp3TRUE:kwh_total_in.percent_normalize_periodp3` =
   "NC:+3 * 1[Treated]:+3",
 `is_treated_periodp3TRUE:I(kwh_total_in.percent_normalize_periodp3^2)` =
    "Square of NC:+3 * 1[Treated]:+3",
 `is_treated_periodp4TRUE` =
   "1[Treated]:+4",
 `kwh_total_in.percent_normalize_periodp4` =
   "NC:+4",
 `I(kwh_total_in.percent_normalize_periodp4^2)` =
   "Square of NC:+4",
 `is_treated_periodp4TRUE:kwh_total_in.percent_normalize_periodp4` =
   "NC:+4 * 1[Treated]:+4",
 `is_treated_periodp4TRUE:I(kwh_total_in.percent_normalize_periodp4^2)` =
    "Square of NC:+4 * 1[Treated]:+4",
 `is_treated_periodp5TRUE` =
   "1[Treated]:+5",
 `kwh_total_in.percent_normalize_periodp5` =
   "NC:+5",
 `I(kwh_total_in.percent_normalize_periodp5^2)` =
   "Square of NC:+5",
 `is_treated_periodp5TRUE:kwh_total_in.percent_normalize_periodp5` =
   "NC:+5 * 1[Treated]:+5",
 `is_treated_periodp5TRUE:I(kwh_total_in.percent_normalize_periodp5^2)` =
    "Square of NC:+5 * 1[Treated]:+5",
 `cdd_daily.avg_period` =
   "Daily Average CDDs",
 `hdd_daily.avg_period` =
   "Daily Average HDDs",
 `(Intercept)` =
   "(Constant)"
)
dt_rd_results_by.term_not.by.month[
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
dt_rd_results_by.term_not.by.month[
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
 names(dt_rd_results_by.term_not.by.month)[
   str_detect(names(dt_rd_results_by.term_not.by.month), "^se_")
 ]
dt_rd_results_by.term_not.by.month[
 , se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se
]


# # 4. Change the order of columns
setcolorder(
 dt_rd_results_by.term_not.by.month,
 c(1:4, length(names(dt_rd_results_by.term_not.by.month)) - 1)
)


# --------------------------------------------------
# Save the DT modified
# --------------------------------------------------
# ------- Save the DT created in Parquet format -------
arrow::write_parquet(
 dt_rd_results_by.term_not.by.month_t1,
 sink = PATH_TO.SAVE_RESULTS,
 version = "1.0",
 compression = "gzip",
 use_dictionary = TRUE
)

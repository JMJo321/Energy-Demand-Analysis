# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02Y-2
# #
# > Purpose of the script(s)
# # : To run regressions in order to estimate the treatment effect which is
# #   related to the second threshold, with quadratic term.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
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
# # 1.1. Regression Results
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <-
  "DT_For-Regression_RD-Design_Reduced.RData"
PATH_TO.LOAD_RD <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD,
  sep = "/"
)

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_RD <- DIR_TO.LOAD_RD
DIR_TO.SAVE_RESULTS <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE_RD, sep = "/")
FILE_TO.SAVE_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month_Tier-2.parquet"
PATH_TO.SAVE_RESULTS <-
  paste(DIR_TO.SAVE_RESULTS, FILE_TO.SAVE_RESULTS, sep = "/")


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load a Dataset
# --------------------------------------------------
# ------- Load a Dataset -------
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
list_bws <- as.list(c(seq(1, 10, by = 1), seq(20, 50, by = 10)))

# # 2. Make a list of seasons
list_seasons <- list("Summer", "Winter", NA)


# ------- Run Regressions: Forward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_quadratic_clustering_forward_p1,
  `Interaction with BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p1,
  `With BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_quadratic_clustering_forward_p2,
  `Interaction with BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p2,
  `With BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_quadratic_clustering_forward_p3,
  `Interaction with BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p3,
  `With BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_quadratic_clustering_forward_p4,
  `Interaction with BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p4,
  `With BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_quadratic_clustering_forward_p5,
  `Interaction with BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_linear.w.inter_clustering_forward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_quadratic_no.clustering_forward_p1,
  `Interaction with BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p1,
  `With BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_quadratic_no.clustering_forward_p2,
  `Interaction with BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p2,
  `With BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_quadratic_no.clustering_forward_p3,
  `Interaction with BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p3,
  `With BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_quadratic_no.clustering_forward_p4,
  `Interaction with BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p4,
  `With BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_quadratic_no.clustering_forward_p5,
  `Interaction with BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p5
)

# # 2. Run Regressions
# # 2.1. For RSGH
# # 2.1.1. For Non-clustered models
combinations_no.clustering <-
  expand.grid(
    list_rd_models_for.subsample_no.clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_no.clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_no.clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_no.clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_forward_g.code_no.clustering <- setDT(NULL)
for (idx in 1:length(combinations_no.clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_no.clustering$model[[idx]]
  tmp_model.description <- combinations_no.clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_no.clustering$suffix[[idx]]
  tmp_bw <- combinations_no.clustering$bw[[idx]]
  tmp_season <- combinations_no.clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_forward_g.code_no.clustering <- rbind(
    dt_results_forward_g.code_no.clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSGH"),
      is_clustering = FALSE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.2. For Clustered models
combinations_clustering <-
  expand.grid(
    list_rd_models_for.subsample_clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_forward_g.code_clustering <- setDT(NULL)
for (idx in 1:length(combinations_clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_clustering$model[[idx]]
  tmp_model.description <- combinations_clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_clustering$suffix[[idx]]
  tmp_bw <- combinations_clustering$bw[[idx]]
  tmp_season <- combinations_clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_forward_g.code_clustering <- rbind(
    dt_results_forward_g.code_clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSGH"),
      is_clustering = TRUE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.3. Join the DTs
cols_on <- c("functional.form", "model", "bw_in.str", "var_independent")
dt_results_forward_g.code <-
  dt_results_forward_g.code_clustering[
    dt_results_forward_g.code_no.clustering[
      , .SD, .SDcols = c(cols_on, "se_hetero")
    ],
    on = cols_on
  ]
dt_results_forward_g.code[, se_hetero := i.se_hetero]
dt_results_forward_g.code[, i.se_hetero := NULL]

# # 2.2. For RSCH and RSEH
# # 2.2.1. For Non-clustered models
combinations_no.clustering <-
  expand.grid(
    list_rd_models_for.subsample_no.clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_no.clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_no.clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_no.clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_forward_ce.codes_no.clustering <- setDT(NULL)
for (idx in 1:length(combinations_no.clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_no.clustering$model[[idx]]
  tmp_model.description <- combinations_no.clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_no.clustering$suffix[[idx]]
  tmp_bw <- combinations_no.clustering$bw[[idx]]
  tmp_season <- combinations_no.clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_forward_ce.codes_no.clustering <- rbind(
    dt_results_forward_ce.codes_no.clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSCH", "RSEH"),
      is_clustering = FALSE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.2. For Clustered models
combinations_clustering <-
  expand.grid(
    list_rd_models_for.subsample_clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_forward_ce.codes_clustering <- setDT(NULL)
for (idx in 1:length(combinations_clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_clustering$model[[idx]]
  tmp_model.description <- combinations_clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_clustering$suffix[[idx]]
  tmp_bw <- combinations_clustering$bw[[idx]]
  tmp_season <- combinations_clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_forward_ce.codes_clustering <- rbind(
    dt_results_forward_ce.codes_clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSCH", "RSEH"),
      is_clustering = TRUE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}

save(dt_results_forward_g.code, dt_results_forward_ce.codes, file = "Forward_Quadratic_Tier-2.RData")

# # 2.1.3. Join the DTs
cols_on <- c("functional.form", "model", "bw_in.str", "var_independent")
dt_results_forward_ce.codes <-
  dt_results_forward_ce.codes_clustering[
    dt_results_forward_ce.codes_no.clustering[
      , .SD, .SDcols = c(cols_on, "se_hetero")
    ],
    on = cols_on
  ]
dt_results_forward_ce.codes[, se_hetero := i.se_hetero]
dt_results_forward_ce.codes[, i.se_hetero := NULL]


# ------- Run Regressions: Backward Responses -------
# # 1. Make a list of models
# # 1.1. For models with clustering
list_rd_models_for.subsample_clustering <- list(
  `With BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_quadratic_clustering_backward_p1,
  `Interaction with BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p1,
  `With BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_quadratic_clustering_backward_p2,
  `Interaction with BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p2,
  `With BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_quadratic_clustering_backward_p3,
  `Interaction with BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p3,
  `With BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_quadratic_clustering_backward_p4,
  `Interaction with BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p4,
  `With BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_quadratic_clustering_backward_p5,
  `Interaction with BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_linear.w.inter_clustering_backward_p5
)
# # 1.2. For models without clustering
list_rd_models_for.subsample_no.clustering <- list(
  `With BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_quadratic_no.clustering_backward_p1,
  `Interaction with BYM FEs, Quadratic, P1` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p1,
  `With BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_quadratic_no.clustering_backward_p2,
  `Interaction with BYM FEs, Quadratic, P2` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p2,
  `With BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_quadratic_no.clustering_backward_p3,
  `Interaction with BYM FEs, Quadratic, P3` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p3,
  `With BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_quadratic_no.clustering_backward_p4,
  `Interaction with BYM FEs, Quadratic, P4` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p4,
  `With BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_quadratic_no.clustering_backward_p5,
  `Interaction with BYM FEs, Quadratic, P5` =
    felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p5
)

# # 2. Run Regressions
# # 2.1. For RSGH
# # 2.1.1. For Non-clustered models
combinations_no.clustering <-
  expand.grid(
    list_rd_models_for.subsample_no.clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_no.clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_no.clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_no.clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_backward_g.code_no.clustering <- setDT(NULL)
for (idx in 1:length(combinations_no.clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_no.clustering$model[[idx]]
  tmp_model.description <- combinations_no.clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_no.clustering$suffix[[idx]]
  tmp_bw <- combinations_no.clustering$bw[[idx]]
  tmp_season <- combinations_no.clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_backward_g.code_no.clustering <- rbind(
    dt_results_backward_g.code_no.clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSGH"),
      is_clustering = FALSE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.2. For Clustered models
combinations_clustering <-
  expand.grid(
    list_rd_models_for.subsample_clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_backward_g.code_clustering <- setDT(NULL)
for (idx in 1:length(combinations_clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_clustering$model[[idx]]
  tmp_model.description <- combinations_clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_clustering$suffix[[idx]]
  tmp_bw <- combinations_clustering$bw[[idx]]
  tmp_season <- combinations_clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_backward_g.code_clustering <- rbind(
    dt_results_backward_g.code_clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSGH"),
      is_clustering = TRUE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.3. Join the DTs
cols_on <- c("functional.form", "model", "bw_in.str", "var_independent")
dt_results_backward_g.code <-
  dt_results_backward_g.code_clustering[
    dt_results_backward_g.code_no.clustering[
      , .SD, .SDcols = c(cols_on, "se_hetero")
    ],
    on = cols_on
  ]
dt_results_backward_g.code[, se_hetero := i.se_hetero]
dt_results_backward_g.code[, i.se_hetero := NULL]

# # 2.2. For RSCH and RSEH
# # 2.2.1. For Non-clustered models
combinations_no.clustering <-
  expand.grid(
    list_rd_models_for.subsample_no.clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_no.clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_no.clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_no.clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_backward_ce.codes_no.clustering <- setDT(NULL)
for (idx in 1:length(combinations_no.clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_no.clustering$model[[idx]]
  tmp_model.description <- combinations_no.clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_no.clustering$suffix[[idx]]
  tmp_bw <- combinations_no.clustering$bw[[idx]]
  tmp_season <- combinations_no.clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_backward_ce.codes_no.clustering <- rbind(
    dt_results_backward_ce.codes_no.clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSCH", "RSEH"),
      is_clustering = FALSE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}
# # 2.1.2. For Clustered models
combinations_clustering <-
  expand.grid(
    list_rd_models_for.subsample_clustering, list_bws, list_seasons,
    stringsAsFactors = FALSE
  )
names(combinations_clustering) <- c("model", "bw", "season")
tmp_list_suffixes <- combinations_clustering$model %>%
  as.character(.) %>% str_extract(., "is_treated_.+?\\s") %>%
  str_replace(., " ", "") %>% str_extract(., "period.+$") %>% as.list(.)
combinations_clustering["suffix"] <- list(tmp_list_suffixes)

dt_results_backward_ce.codes_clustering <- setDT(NULL)
for (idx in 1:length(combinations_clustering$bw)) {
  # ## Generate temporary objects
  tmp_model <- combinations_clustering$model[[idx]]
  tmp_model.description <- combinations_clustering$model[idx] %>% names(.)
  tmp_suffix <- combinations_clustering$suffix[[idx]]
  tmp_bw <- combinations_clustering$bw[[idx]]
  tmp_season <- combinations_clustering$season[[idx]]
  # ## Print a message
  print(
    paste0(
      "Estimating: ", idx, " out of ", length(combinations_no.clustering$bw),
      ", ", tmp_model.description, ", BW ", tmp_bw, "%, ", tmp_season
    )
  )
  # ## Extract information from a FELM object
  dt_results_backward_ce.codes_clustering <- rbind(
    dt_results_backward_ce.codes_clustering,
    extract_reg.results_felm_for.subsample(
      model = tmp_model, suffix_period = tmp_suffix,
      bw = tmp_bw, season = tmp_season,
      model.description = tmp_model.description,
      rate.codes = c("RSCH", "RSEH"),
      is_clustering = TRUE, month_response = NULL,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix_tier = "t2"
    )
  )
  # ## Conduct garbage collection
  gc(reset = TRUE, full = TRUE)
}

save(dt_results_backward_g.code, dt_results_backward_ce.codes, file = "Backward_Quadratic_Tier-2.RData")

# # 2.1.3. Join the DTs
cols_on <- c("functional.form", "model", "bw_in.str", "var_independent")
dt_results_backward_ce.codes <-
  dt_results_backward_ce.codes_clustering[
    dt_results_backward_ce.codes_no.clustering[
      , .SD, .SDcols = c(cols_on, "se_hetero")
    ],
    on = cols_on
  ]
dt_results_backward_ce.codes[, se_hetero := i.se_hetero]
dt_results_backward_ce.codes[, i.se_hetero := NULL]


# # --------------------------------------------------
# # Create a DT from Regression Results
# # --------------------------------------------------
# # ------- Make a Combined DT from Results -------
# # # 1. Make a DT by combining DTs including regression results
# dt_rd_results_by.term_t2 <- rbind(
#   dt_forward_quadratic_g.code[, category := "Forward"],
#   # dt_forward_quadratic_ce.codes[, category := "Forward"],
#   dt_backward_quadratic_g.code[, category := "Backward"]
#   # ,
#   # dt_backward_quadratic_ce.codes[, category := "Backward"],
#   # dt_forward_square_g.code[, category := "Forward"],
#   # dt_forward_square_ce.codes[, category := "Forward"],
#   # dt_backward_square_g.code[, category := "Backward"],
#   # dt_backward_square_ce.codes[, category := "Backward"]
# )
#
#
# # ------- Modify the Combined DT from Results -------
# # # 1. Change a data field's values
# list_var_independent <- list(
#   `is_treated_t2_period0TRUE` =
#     "1[Treated]:0",
#   `kwh_total_in.percent_normalize_t2_period0` =
#     "NC:0",
#   `is_treated_t2_period0TRUE:kwh_total_in.percent_normalize_t2_period0` =
#     "NC:0 * 1[Treated]:0",
#   `is_treated_t2_periodm1TRUE` =
#     "1[Treated]:-1",
#   `kwh_total_in.percent_normalize_t2_periodm1` =
#     "NC:-1",
#   `is_treated_t2_periodm1TRUE:kwh_total_in.percent_normalize_t2_periodm1` =
#     "NC:-1 * 1[Treated]:-1",
#   `is_treated_t2_periodm2TRUE` =
#     "1[Treated]:-2",
#   `kwh_total_in.percent_normalize_t2_periodm2` =
#     "NC:-2",
#   `is_treated_t2_periodm2TRUE:kwh_total_in.percent_normalize_t2_periodm2` =
#     "NC:-2 * 1[Treated]:-2",
#   `is_treated_t2_periodm3TRUE` =
#     "1[Treated]:-3",
#   `kwh_total_in.percent_normalize_t2_periodm3` =
#     "NC:-3",
#   `is_treated_t2_periodm3TRUE:kwh_total_in.percent_normalize_t2_periodm3` =
#     "NC:-3 * 1[Treated]:-3",
#   `is_treated_t2_periodm4TRUE` =
#     "1[Treated]:-4",
#   `kwh_total_in.percent_normalize_t2_periodm4` =
#     "NC:-4",
#   `is_treated_t2_periodm4TRUE:kwh_total_in.percent_normalize_t2_periodm4` =
#     "NC:-4 * 1[Treated]:-4",
#   `is_treated_t2_periodp1TRUE` =
#     "1[Treated]:+1",
#   `kwh_total_in.percent_normalize_t2_periodp1` =
#     "NC:+1",
#   `I(kwh_total_in.percent_normalize_t2_periodp1^2)` =
#     "Square of NC:+1",
#   `is_treated_t2_periodp1TRUE:kwh_total_in.percent_normalize_t2_periodp1` =
#     "NC:+1 * 1[Treated]:+1",
#    `is_treated_t2_periodp1TRUE:I(kwh_total_in.percent_normalize_t2_periodp1^2)` =
#      "Square of NC:+1 * 1[Treated]:+1",
#   `is_treated_t2_periodp2TRUE` =
#     "1[Treated]:+2",
#   `kwh_total_in.percent_normalize_t2_periodp2` =
#     "NC:+2",
#   `I(kwh_total_in.percent_normalize_t2_periodp2^2)` =
#     "Square of NC:+2",
#   `is_treated_t2_periodp2TRUE:kwh_total_in.percent_normalize_t2_periodp2` =
#     "NC:+2 * 1[Treated]:+2",
#   `is_treated_t2_periodp2TRUE:I(kwh_total_in.percent_normalize_t2_periodp2^2)` =
#      "Square of NC:+2 * 1[Treated]:+2",
#   `is_treated_t2_periodp3TRUE` =
#     "1[Treated]:+3",
#   `kwh_total_in.percent_normalize_t2_periodp3` =
#     "NC:+3",
#   `I(kwh_total_in.percent_normalize_t2_periodp3^2)` =
#     "Square of NC:+3",
#   `is_treated_t2_periodp3TRUE:kwh_total_in.percent_normalize_t2_periodp3` =
#     "NC:+3 * 1[Treated]:+3",
#   `is_treated_t2_periodp3TRUE:I(kwh_total_in.percent_normalize_t2_periodp3^2)` =
#      "Square of NC:+3 * 1[Treated]:+3",
#   `is_treated_t2_periodp4TRUE` =
#     "1[Treated]:+4",
#   `kwh_total_in.percent_normalize_t2_periodp4` =
#     "NC:+4",
#   `I(kwh_total_in.percent_normalize_t2_periodp4^2)` =
#     "Square of NC:+4",
#   `is_treated_t2_periodp4TRUE:kwh_total_in.percent_normalize_t2_periodp4` =
#     "NC:+4 * 1[Treated]:+4",
#   `is_treated_t2_periodp4TRUE:I(kwh_total_in.percent_normalize_t2_periodp4^2)` =
#      "Square of NC:+4 * 1[Treated]:+4",
#   `is_treated_t2_periodp5TRUE` =
#     "1[Treated]:+5",
#   `kwh_total_in.percent_normalize_t2_periodp5` =
#     "NC:+5",
#   `I(kwh_total_in.percent_normalize_t2_periodp5^2)` =
#     "Square of NC:+5",
#   `is_treated_t2_periodp5TRUE:kwh_total_in.percent_normalize_t2_periodp5` =
#     "NC:+5 * 1[Treated]:+5",
#   `is_treated_t2_periodp5TRUE:I(kwh_total_in.percent_normalize_t2_periodp5^2)` =
#      "Square of NC:+5 * 1[Treated]:+5",
#   `cdd_daily.avg_period` =
#     "Daily Average CDDs",
#   `hdd_daily.avg_period` =
#     "Daily Average HDDs",
#   `(Intercept)` =
#     "(Constant)"
# )
# dt_rd_results_by.term_t2[
#   ,
#   var_independent :=
#     sapply(var_independent, function(x) list_var_independent[[x]])
# ]
#
# # # 2. Change data type: To Factor
# # # 2.1. Make objects including factors' levels
# order_functional.form <- c("Linear", "Square", "Cubic")
# order_model <- c(
#   "Without FEs and Controls",
#   "Without FEs",
#   "With IDs FEs",
#   "With BYM FEs",
#   "With Both FEs",
#   "Interaction without FEs and Controls",
#   "Interaction without FEs",
#   "Interaction with IDs FEs",
#   "Interaction with BYM FEs",
#   "Interaction with Both FEs"
# )
# order_bw <- paste0(list_bws, "%")
# order_var_independent <- unlist(list_var_independent, use.names = FALSE)
# # # 2.2. Convert data type
# dt_rd_results_by.term_t2[
#   ,
#   `:=` (
#     functional.form = factor(
#       functional.form, levels = order_functional.form, ordered = TRUE
#     ),
#     model = factor(model, levels = order_model, ordered = TRUE),
#     bw_in.str = factor(bw_in.str, levels = order_bw, ordered = TRUE),
#     var_independent = factor(
#       var_independent, levels = order_var_independent
#     )
#   )
# ]
#
# # # 3. Add columns
# # # 3.1. Add a column indicating the maximum S.E. among the S.E.s
# cols_se <-
#   names(dt_rd_results_by.term_t2)[str_detect(names(dt_rd_results_by.term_t2), "^se_")]
# dt_rd_results_by.term_t2[
#   , se_max := apply(.SD, 1, max, na.rm = TRUE), .SDcols = cols_se
# ]
#
# # # 4. Change the order of columns
# setcolorder(
#   dt_rd_results_by.term_t2,
#   c(1:4, length(names(dt_rd_results_by.term_t2)) - 1)
# )
#
#
# # --------------------------------------------------
# # Save the DT modified
# # --------------------------------------------------
# # ------- Save the DT created in Parquet format -------
# arrow::write_parquet(
#   dt_rd_results_by.term_t2,
#   sink = PATH_TO.SAVE_RESULTS,
#   version = "1.0",
#   compression = "snappy",
#   use_dictionary = TRUE
# )

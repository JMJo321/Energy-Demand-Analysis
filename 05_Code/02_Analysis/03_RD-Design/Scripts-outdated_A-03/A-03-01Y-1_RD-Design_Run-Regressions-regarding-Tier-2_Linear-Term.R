# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02Y-1
# #
# > Purpose of the script(s)
# # : To run regressions in order to estimate the treatment effect which is
# #   related to the second threshold, With linear term.

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
  "DT_RD-Design_Regression-Results_Linear-and-Tier-2.RData"
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


# --------------------------------------------------
# Save DTs containing regression results
# --------------------------------------------------
# ------- Save DTs -------
# # 1. Rename DTs
dt_results_forward_linear_g.code <- copy(dt_results_forward_g.code)
dt_results_forward_linear_ce.codes <- copy(dt_results_forward_ce.codes)
dt_results_backward_linear_g.code <- copy(dt_results_backward_g.code)
dt_results_backward_linear_ce.codes <- copy(dt_results_backward_ce.codes)

# # 2. Save DTs in .RData format
save(
  dt_results_forward_linear_g.code, dt_results_forward_linear_ce.codes,
  dt_results_backward_linear_g.code, dt_results_backward_linear_ce.codes,
  file = PATH_TO.SAVE_RESULTS
)

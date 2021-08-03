# Description
# : Define Regression Models for CER Data

# ------------------------------------------------------------------------------
# To load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)


# ------------------------------------------------------------------------------
# To set working directory
# ------------------------------------------------------------------------------
# (In each script)


# ------------------------------------------------------------------------------
# To define parameter(s), and/or function(s)
# ------------------------------------------------------------------------------
# # 1. To get formulas for `felm`
get_felm.formula <-
  function (
    dep.var,
    indep.var_covariates, indep.var_fes, indep.var_ivs, indep.var_clustered.ses
  ) {
    indep.var <- paste(
      indep.var_covariates,
      indep.var_fes,
      indep.var_ivs,
      indep.var_clustered.ses,
      sep = " | "
    )
    formula_in.str <- paste(dep.var, indep.var, sep = "~ ")
    return(formula(formula_in.str))
}


# ------------------------------------------------------------------------------
# Define Regression Models: To estimate Temp. Response with Daily Data
# ------------------------------------------------------------------------------
# ------- Models without FEs -------
# # 1. Models for a Sample excluding Observations of Control Group
model_ols_daily_excl.control_linear <- formula(
  kwh ~
    hdd_all + is_treatment.and.post + hdd_all:is_treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)
model_ols_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) + is_treatment.and.post +
      hdd_all:is_treatment.and.post + I(hdd_all^2):is_treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)

# # 2. Models for a Sample including Observations of Control Group
model_ols_daily_incl.control_linear <- formula(
  kwh ~
    hdd_all + is_treated_r + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_ols_daily_incl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) +
      is_treated_r + hdd_all:is_treated_r + I(hdd_all^2):is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
        I(hdd_all^2):is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post +
        I(hdd_all^2):is_treatment.and.post |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# ------- Models with FEs -------
# # 1. Models for a Sample excluding Observations of Control Group
model_fes_daily_excl.control_linear <- formula(
  kwh ~
    hdd_all + is_treatment.and.post + hdd_all:is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) + is_treatment.and.post +
      hdd_all:is_treatment.and.post + I(hdd_all^2):is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)

# # 2. Models for a Sample including Observations of Control Group
model_fes_daily_incl.control_linear <- formula(
  kwh ~
    hdd_all + is_treated_r + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_linear_variation1 <- formula(
  kwh ~
    hdd_all + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) +
      is_treated_r + hdd_all:is_treated_r + I(hdd_all^2):is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
        I(hdd_all^2):is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post +
        I(hdd_all^2):is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_quadratic_variation1 <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) +
      hdd_all:is_treated_r + I(hdd_all^2):is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
        I(hdd_all^2):is_treatment.period +
      is_treatment.and.post + hdd_all:is_treatment.and.post +
        I(hdd_all^2):is_treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# ------------------------------------------------------------------------------
# Define Regression Models that exploit Hourly Data
# ------------------------------------------------------------------------------
# ------- Models for Estimating Average Treatment Effect -------
# # 1. Object(s) that will be used later to estimate the Average Treatment
# #    Effect
dep.var_avg.effect <- "kwh"
indep.var_covariates_avg.effect <- str_c(
  paste0("is_treatment.and.post_30min_", seq(1, 48, by = 1)),
  collapse = " + "
)
indep.var_ivs_avg.effect <- "0"
indep.var_clustered.ses_avg.effect <- "id_in.factor + day_in.factor"


# # 2. Define Models with Clustering Standard Errors
model_avg.effect_30min_i <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_i.d <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.d <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.dw <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.dw.m <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = "0"
)


# # 3. Define Models with Clustering Standard Errors
model_avg.effect_30min_i_clustered.ses <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = indep.var_clustered.ses_avg.effect
)
model_avg.effect_30min_i.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = indep.var_clustered.ses_avg.effect
)
model_avg.effect_30min_iw.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = indep.var_clustered.ses_avg.effect
)
model_avg.effect_30min_iw.dw_clustered.ses <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = indep.var_clustered.ses_avg.effect
)
model_avg.effect_30min_iw.dw.m_clustered.ses <- get_felm.formula(
  dep.var = dep.var_avg.effect,
  indep.var_covariates = indep.var_covariates_avg.effect,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_avg.effect,
  indep.var_clustered.ses = indep.var_clustered.ses_avg.effect
)


# ------- Models for Estimating Household Response to Changes in Temp. -------
# ## Note:
# ## To define econometric models, the variable `hdd_all` is utilized, instead
# ## of `hdd_extremes` and `hdd_soil`.

# # 1. Object(s) that will be used later to estimate Household Response to
# #    Changes in Temperature
dep.var_temp.response <- "kwh"
indep.var_covariates_temp.response <- paste(
  "hdd",
  str_c(
    paste0("is_treatment_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_treatment_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  str_c(
    paste0("is_post_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_post_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  str_c(
    paste0("is_treatment.and.post_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_treatment.and.post_30min_", seq(1, 48, by = 1)),
    collapse = " + "
  ),
  sep = " + "
)
indep.var_ivs_temp.response <- "0"
indep.var_clustered.ses_temp.response <- "id_in.factor + day_in.factor"


# # 2. FEs Models
model_temp.response_30min_i_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_i.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_iw.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_iw.dw_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_iw.dw.m_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_iw.dw.mp_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.rate.period_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)
model_temp.response_30min_iw.dw.mpw_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response,
  indep.var_covariates = indep.var_covariates_temp.response,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.rate.period.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)

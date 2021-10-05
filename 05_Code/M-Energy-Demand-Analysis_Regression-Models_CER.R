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


# ------- Models for Estimating Household Response to Changes in Temp.   -------
# ------- by using 30-Minute Interval Data                               -------
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
    "month.and.rate.period.level1_in.factor",
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
    "month.and.rate.period.level1.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response
)


# ------- Models for Estimating Rate-Period-Level Household Response to  -------
# ------- Changes in Temp. by using 30-Minute-interval Data              -------
# # 1. Object(s) that will be used later to estimate Household Response to
# #    Changes in Temperature
dep.var_temp.response.by.period <- "kwh"
rate.periods_detail1_modified <-
  c("night", "day_pre.peak", "peak", "day_post.peak")
indep.var_covariates_temp.response.by.period <- paste(
  "hdd",
  str_c(
    paste0("is_treatment_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_treatment_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("is_post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("is_treatment.and.post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0(
      "hdd:is_treatment.and.post_rate.period_", rate.periods_detail1_modified
    ),
    collapse = " + "
  ),
  sep = " + "
)
indep.var_ivs_temp.response.by.period <- "0"
indep.var_clustered.ses_temp.response.by.period <-
  "id_in.factor + day_in.factor"


# # 2. FEs Models
model_temp.response.by.period_30min_i_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_i.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_iw.d_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_iw.dw_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_iw.dw.m_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_iw.dw.mp_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.rate.period.level1_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)
model_temp.response.by.period_30min_iw.dw.mpw_clustered.ses <- get_felm.formula(
  dep.var = dep.var_temp.response.by.period,
  indep.var_covariates = indep.var_covariates_temp.response.by.period,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.rate.period.level1.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs_temp.response.by.period,
  indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
)


# ------- Models for Estimating Rate-Period-Level Household Response to  -------
# ------- Changes in Temp. by using Rate-Period-Level Consumption        -------
# # 1. Object(s) that will be used later to estimate Household Response to
# #    Changes in Temperature
dep.var_temp.response.by.period <- "kwh_per.hour"
rate.periods_detail1_modified <-
  c("night", "day_pre.peak", "peak", "day_post.peak")
indep.var_covariates_temp.response.by.period <- paste(
  "hdd",
  str_c(
    paste0("is_treatment_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_treatment_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("is_post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("hdd:is_post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0("is_treatment.and.post_rate.period_", rate.periods_detail1_modified),
    collapse = " + "
  ),
  str_c(
    paste0(
      "hdd:is_treatment.and.post_rate.period_", rate.periods_detail1_modified
    ),
    collapse = " + "
  ),
  sep = " + "
)
indep.var_ivs_temp.response.by.period <- "0"
indep.var_clustered.ses_temp.response.by.period <-
  "id_in.factor + day_in.factor"


# # 2. FEs Models
model_temp.response.by.period_rate.period_i_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_i.d_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id_in.factor",
      "day.of.week_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_id.d_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.day.of.week_in.factor",
      "day.of.week_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_ip.d_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.rate.period.level1_in.factor",
      "day.of.week_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_idp.d_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.day.of.week.and.rate.period.level1_in.factor",
      "day.of.week_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_idp.dp_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.day.of.week.and.rate.period.level1_in.factor",
      "day.of.week.and.rate.period.level1_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_idp.dp.m_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.day.of.week.and.rate.period.level1_in.factor",
      "day.of.week.and.rate.period.level1_in.factor",
      "month_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )
model_temp.response.by.period_rate.period_idp.dp.mp_clustered.ses <-
  get_felm.formula(
    dep.var = dep.var_temp.response.by.period,
    indep.var_covariates = indep.var_covariates_temp.response.by.period,
    indep.var_fes = paste(
      "id.and.day.of.week.and.rate.period.level1_in.factor",
      "day.of.week.and.rate.period.level1_in.factor",
      "month.and.rate.period.level1_in.factor",
      sep = " + "
    ),
    indep.var_ivs = indep.var_ivs_temp.response.by.period,
    indep.var_clustered.ses = indep.var_clustered.ses_temp.response.by.period
  )


# ------- Models to Estimate Rate-Period-Level Household Response to     -------
# ------- Changes in Temp. for Each Tariff by using 30-Minute-interval   -------
# ------- Data                                                           -------
# # 1. Object(s) that will be used later to estimate Household Response to
# #    Changes in Temperature
dep.var_treatment.effect_by.period.and.tariff <- "kwh"
tariffs <- c("A", "B", "C", "D")
tariffs_modified <- tolower(tariffs)
indep.var_covariates_treatment.effect_by.period.and.tariff <- paste(
  "is_treated_r",
  "hdd_by_is_treated_r",
  "is_treatment.period",
  "hdd_by_is_treatment.period",
  "is_treatment.and.post",
  "hdd_by_is_treatment.and.post",
  sep = " + "
)
indep.var_ivs_treatment.effect_by.period.and.tariff <- "0"
indep.var_clustered.ses_treatment.effect_by.period.and.tariff <-
  "id_in.factor + day_in.factor"


# # 2. FEs Models
model_treatment.effect_by.period.and.tariff_30min_iw_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.dp_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.rate.period.level1_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.dw_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.mp_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "month.and.rate.period.level1_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.mw_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.dp.mp_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.rate.period.level1_in.factor",
      "month.and.rate.period.level1_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )
model_treatment.effect_by.period.and.tariff_30min_iw.dw.mw_clustered.ses <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_by.period.and.tariff,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_by.period.and.tariff,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_by.period.and.tariff,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_by.period.and.tariff
  )


# ------- Models for Estimating Household Response to Changes in Temp.   -------
# ------- by using 30-Minute Interval Data, as a function of Rate        -------
# ## Note:
# ## To define econometric models, the variable `hdd_all` is utilized, instead
# ## of `hdd_extremes` and `hdd_soil`.

# # 1. Object(s) that will be used later to estimate Household Response to
# #    Changes in Temperature
dep.var_treatment.effect_w.rate.change <- "kwh"
indep.var_covariates_treatment.effect_w.rate.change.by.rate.period <-
  paste(
    "treatment.times.rate.change_by.rate.period",
    "hdd.times.treatment.times.rate.change_by.rate.period",
    "is_treatment.period",
    "hdd.times.post",
    "treatment.and.post.times.rate.change_by.rate.period",
    "hdd.times.treatment.and.post.times.rate.change_by.rate.period",
    sep = " + "
  )
indep.var_covariates_treatment.effect_w.rate.change.by.period.and.rate.period <-
  paste(
    "treatment.times.rate.change_by.period.and.rate.period",
    "hdd.times.treatment.times.rate.change_by.period.and.rate.period",
    "is_treatment.period",
    "hdd.times.post",
    "treatment.and.post.times.rate.change_by.period.and.rate.period",
    "hdd.times.treatment.and.post.times.rate.change_by.period.and.rate.period",
    sep = " + "
  )
# ## Note:
# ## Several independent variables are created when running regressions.
indep.var_ivs_treatment.effect_w.rate.change <- "0"
indep.var_clustered.ses_treatment.effect_w.rate.change <-
  "id_in.factor + day_in.factor"


# # 2. FEs Models
model_30min_iw.dw.mw_w.rate.change.by.rate.period <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_w.rate.change,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_w.rate.change.by.rate.period,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_w.rate.change,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_w.rate.change
  )

model_30min_iw.dw.mw_w.rate.change.by.period.and.rate.period <-
  get_felm.formula(
    dep.var =
      dep.var_treatment.effect_w.rate.change,
    indep.var_covariates =
      indep.var_covariates_treatment.effect_w.rate.change.by.period.and.rate.period,
    indep.var_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.var_ivs =
      indep.var_ivs_treatment.effect_w.rate.change,
    indep.var_clustered.ses =
      indep.var_clustered.ses_treatment.effect_w.rate.change
  )

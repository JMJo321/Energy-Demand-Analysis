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
# To define parameter(s)
# ------------------------------------------------------------------------------
# (In each script)


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
# Define Regression Models: To estimate Temp. Response with Hourly Data
# ------------------------------------------------------------------------------
# ------- Models for Estimating Average Treatment Effect by Rate Period -------
# # 1. Create Function(s) and Object(s) that will be used later to estimate
# #    the Average Treatment Effect
# # 1.1. Create function(s)
# # 1.1.1. To get formulas for `felm`
get_felm.formula_avg.effect <-
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

# # 1.2. Create Object(s)
dep.var <- "kwh"
indep.var_covariates <- str_c(
  paste0("is_treatment.and.post_30min_", seq(1, 48, by = 1)),
  collapse = " + "
)
indep.var_ivs <- "0"
indep.var_clustered.ses <- "id_in.factor + day_in.factor"


# # 2. Define Models with Clustering Standard Errors
model_avg.effect_30min_i <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_i.d <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.d <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.dw <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = "0"
)
model_avg.effect_30min_iw.dw.m <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = "0"
)


# # 3. Define Models with Clustering Standard Errors
model_avg.effect_30min_i_clustered.ses <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = indep.var_clustered.ses
)
model_avg.effect_30min_i.d_clustered.ses <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = indep.var_clustered.ses
)
model_avg.effect_30min_iw.d_clustered.ses <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = indep.var_clustered.ses
)
model_avg.effect_30min_iw.dw_clustered.ses <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = indep.var_clustered.ses
)
model_avg.effect_30min_iw.dw.m_clustered.ses <- get_felm.formula_avg.effect(
  dep.var = dep.var,
  indep.var_covariates = indep.var_covariates,
  indep.var_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.var_ivs = indep.var_ivs,
  indep.var_clustered.ses = indep.var_clustered.ses
)


# ------- Models without FEs -------
# # 1. For models using `hdd_all`
model_ols_30min_all <- formula(
  kwh ~
    hdd_all +
    is_treated_r + hdd_all:is_treated_r +
    is_treatment.period + hdd_all:is_treatment.period +
    is_treatment.and.post + hdd_all:is_treatment.and.post |
    0 |
    0 |
    id_in.factor + day_in.factor
)

# # 2. For models using `hdd_extremes`
model_ols_hourly_extremes <- formula(
  kwh ~
    hdd_extremes +
    is_treated_r + hdd_extremes:is_treated_r +
    is_treatment.period + hdd_extremes:is_treatment.period +
    is_treatment.and.post + hdd_extremes:is_treatment.and.post |
    0 |
    0 |
    id_in.factor + day_in.factor
)

# # 3. For models using `hdd_soil`
model_ols_hourly_soil <- formula(
  kwh ~
    hdd_soil +
    is_treated_r + hdd_soil:is_treated_r +
    is_treatment.period + hdd_soil:is_treatment.period +
    is_treatment.and.post + hdd_soil:is_treatment.and.post |
    0 |
    0 |
    id_in.factor + day_in.factor
)


# ------- Models with FEs -------
# # 1. For models using `hdd_all`
model_fes_hourly_all_id <- formula(
  kwh ~
    hdd_all +
    is_treated_r + hdd_all:is_treated_r +
    is_treatment.period + hdd_all:is_treatment.period +
    is_treatment.and.post + hdd_all:is_treatment.and.post |
    id_in.factor |
    0 |
    id_in.factor + day_in.factor
)
model_fes_hourly_all_id.and.day.of.week <- formula(
  kwh ~
    hdd_all +
    is_treated_r + hdd_all:is_treated_r +
    is_treatment.period + hdd_all:is_treatment.period +
    is_treatment.and.post + hdd_all:is_treatment.and.post |
    id.and.day.of.week_in.factor |
    0 |
    id_in.factor + day_in.factor
)

# # 2. For models using `hdd_extremes`
model_fes_hourly_extremes_id <- formula(
  kwh ~
    hdd_extremes +
    is_treated_r + hdd_extremes:is_treated_r +
    is_treatment.period + hdd_extremes:is_treatment.period +
    is_treatment.and.post + hdd_extremes:is_treatment.and.post |
    id_in.factor |
    0 |
    id_in.factor + day_in.factor
)
model_fes_hourly_extremes_id.and.day.of.week <- formula(
  kwh ~
    hdd_extremes +
    is_treated_r + hdd_extremes:is_treated_r +
    is_treatment.period + hdd_extremes:is_treatment.period +
    is_treatment.and.post + hdd_extremes:is_treatment.and.post |
    id.and.day.of.week_in.factor |
    0 |
    id_in.factor + day_in.factor
)

# # 3. For models using `hdd_soil`
model_fes_hourly_soil_id <- formula(
  kwh ~
    hdd_soil +
    is_treated_r + hdd_soil:is_treated_r +
    is_treatment.period + hdd_soil:is_treatment.period +
    is_treatment.and.post + hdd_soil:is_treatment.and.post |
    id_in.factor |
    0 |
    id_in.factor + day_in.factor
)
model_fes_hourly_soil_id.and.day.of.week <- formula(
  kwh ~
    hdd_soil +
    is_treated_r + hdd_soil:is_treated_r +
    is_treatment.period + hdd_soil:is_treatment.period +
    is_treatment.and.post + hdd_soil:is_treatment.and.post |
    id.and.day.of.week_in.factor |
    0 |
    id_in.factor + day_in.factor
)

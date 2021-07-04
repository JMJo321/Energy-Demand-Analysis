# Description
# : Define Regression Models for CER Data

# --------------------------------------------------
# To load required libraries
# --------------------------------------------------
# (Not Applicable)


# --------------------------------------------------
# To set working directory
# --------------------------------------------------
# (In each script)


# --------------------------------------------------
# To define parameter(s)
# --------------------------------------------------
# (In each script)


# --------------------------------------------------
# Define Regression Models
# --------------------------------------------------
# ------- Models for Estimating Temperature Response -------
# # 1. Models without FEs
# # 1.1. Models for Day-level Data
# # 1.1.1. Models for a Sample excluding Observations of Control Group
model_ols_daily_excl.control_linear <- formula(
  kwh ~
    hdd + treatment.and.post + hdd:treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)
model_ols_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd + I(hdd^2) + treatment.and.post + hdd:treatment.and.post +
      I(hdd^2):treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)
# # 1.1.2. Models for a Sample including Observations of Control Group
model_ols_daily_incl.control_linear <- formula(
  kwh ~
    hdd + is_treated_r + hdd:is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
      treatment.and.post + hdd:treatment.and.post |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_ols_daily_incl.control_quadratic <- formula(
  kwh ~
    hdd + I(hdd^2) +
      is_treated_r + hdd:is_treated_r + I(hdd^2):is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
        I(hdd^2):is_treatment.period +
      treatment.and.post + hdd:treatment.and.post +
        I(hdd^2):treatment.and.post |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# # 2. Models with FEs
# # 2.1. Models for Day-level Data
# # 2.1.1. Models for a Sample excluding Observations of Control Group
model_fes_daily_excl.control_linear <- formula(
  kwh ~
    hdd + treatment.and.post + hdd:treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd + I(hdd^2) + treatment.and.post + hdd:treatment.and.post +
      I(hdd^2):treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
# # 2.1.2. Models for a Sample including Observations of Control Group
model_fes_daily_incl.control_linear <- formula(
  kwh ~
    hdd + is_treated_r + hdd:is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
      treatment.and.post + hdd:treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_linear_variation1 <- formula(
  kwh ~
    hdd + hdd:is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
      treatment.and.post + hdd:treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_quadratic <- formula(
  kwh ~
    hdd + I(hdd^2) +
      is_treated_r + hdd:is_treated_r + I(hdd^2):is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
        I(hdd^2):is_treatment.period +
      treatment.and.post + hdd:treatment.and.post +
        I(hdd^2):treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_quadratic_variation1 <- formula(
  kwh ~
    hdd + I(hdd^2) +
      hdd:is_treated_r + I(hdd^2):is_treated_r +
      is_treatment.period + hdd:is_treatment.period +
        I(hdd^2):is_treatment.period +
      treatment.and.post + hdd:treatment.and.post +
        I(hdd^2):treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)

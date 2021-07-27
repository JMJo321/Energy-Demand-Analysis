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
# ------- Models for Estimating Temp. Response: with Daily Consumption -------
# # 1. Models without FEs
# # 1.1. Models for a Sample excluding Observations of Control Group
model_ols_daily_excl.control_linear <- formula(
  kwh ~
    hdd_all + treatment.and.post + hdd_all:treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)
model_ols_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) + treatment.and.post +
      hdd_all:treatment.and.post + I(hdd_all^2):treatment.and.post |
      0 |
      0 |
      id.and.day.of.week_in.factor + month_in.factor
)
# # 1.2. Models for a Sample including Observations of Control Group
model_ols_daily_incl.control_linear <- formula(
  kwh ~
    hdd_all + is_treated_r + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      treatment.and.post + hdd_all:treatment.and.post |
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
      treatment.and.post + hdd_all:treatment.and.post +
        I(hdd_all^2):treatment.and.post |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# # 2. Models with FEs
# # 2.1. Models for a Sample excluding Observations of Control Group
model_fes_daily_excl.control_linear <- formula(
  kwh ~
    hdd_all + treatment.and.post + hdd_all:treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_excl.control_quadratic <- formula(
  kwh ~
    hdd_all + I(hdd_all^2) + treatment.and.post +
      hdd_all:treatment.and.post + I(hdd_all^2):treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
# # 2.2. Models for a Sample including Observations of Control Group
model_fes_daily_incl.control_linear <- formula(
  kwh ~
    hdd_all + is_treated_r + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      treatment.and.post + hdd_all:treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)
model_fes_daily_incl.control_linear_variation1 <- formula(
  kwh ~
    hdd_all + hdd_all:is_treated_r +
      is_treatment.period + hdd_all:is_treatment.period +
      treatment.and.post + hdd_all:treatment.and.post |
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
      treatment.and.post + hdd_all:treatment.and.post +
        I(hdd_all^2):treatment.and.post |
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
      treatment.and.post + hdd_all:treatment.and.post +
        I(hdd_all^2):treatment.and.post |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# ------- Models for Estimating Temp. Response: with Hourly Consumption -------
# # 1. Models without FEs
model_ols_hourly <- formula(
  kwh ~
    hd_by.season.and.rate.period:rate.period +
      I(hd_by.season.and.rate.period^2):rate.period +
    is_treated_r:rate.period +
      hd_by.season.and.rate.period:is_treated_r:rate.period +
      I(hd_by.season.and.rate.period^2):is_treated_r:rate.period +
    is_treatment.period:rate.period +
      hd_by.season.and.rate.period:is_treatment.period:rate.period +
      I(hd_by.season.and.rate.period^2):is_treatment.period:rate.period +
    treatment.and.post:rate.period +
      hd_by.season.and.rate.period:treatment.and.post:rate.period +
      I(hd_by.season.and.rate.period^2):treatment.and.post:rate.period |
    0 |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)


# # 2. Models with FEs
model_fes_hourly_base <- formula(
  kwh ~
    hd_by.season.and.rate.period:rate.period +
      I(hd_by.season.and.rate.period^2):rate.period +
    is_treated_r:rate.period +
      hd_by.season.and.rate.period:is_treated_r:rate.period +
      I(hd_by.season.and.rate.period^2):is_treated_r:rate.period +
    is_treatment.period:rate.period +
      hd_by.season.and.rate.period:is_treatment.period:rate.period +
      I(hd_by.season.and.rate.period^2):is_treatment.period:rate.period +
    treatment.and.post:rate.period +
      hd_by.season.and.rate.period:treatment.and.post:rate.period +
      I(hd_by.season.and.rate.period^2):treatment.and.post:rate.period |
    id.and.day.of.week_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)

model_fes_hourly_var1 <- formula(
  kwh ~
    hd_by.season.and.rate.period:rate.period +
      I(hd_by.season.and.rate.period^2):rate.period +
    is_treated_r:rate.period +
      hd_by.season.and.rate.period:is_treated_r:rate.period +
      I(hd_by.season.and.rate.period^2):is_treated_r:rate.period +
    is_treatment.period:rate.period +
      hd_by.season.and.rate.period:is_treatment.period:rate.period +
      I(hd_by.season.and.rate.period^2):is_treatment.period:rate.period +
    treatment.and.post:rate.period +
      hd_by.season.and.rate.period:treatment.and.post:rate.period +
      I(hd_by.season.and.rate.period^2):treatment.and.post:rate.period |
    id.and.day.of.week_in.factor + month_in.factor |
    0 |
    id.and.day.of.week_in.factor + month_in.factor
)

# Description
# : Define Regression Models

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
# ------- Models related to RD Design -------
# # 1. Models without FEs
# # 1.1. Without Interaction Term(s)
# # 1.1.1. Including Linear Term
lm_rd_wo.fes_linear_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0
  )
felm_rd_wo.fes_linear_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0
  )
felm_rd_wo.fes_linear_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_linear <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_linear_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_linear_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 1.1.2. Including Square Term
lm_rd_wo.fes_quadratic_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2)
  )
felm_rd_wo.fes_quadratic_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2)
  )
felm_rd_wo.fes_quadratic_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_square <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_quadratic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_quadratic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 1.1.3. Including Cubic Term
lm_rd_wo.fes_cubic_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3)
  )
felm_rd_wo.fes_cubic_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3)
  )
felm_rd_wo.fes_cubic_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_cubic <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_cubic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_cubic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )

# # 1.2. With Interaction Term(s)
# # 1.2.1. Including linear term
lm_rd_wo.fes_linear.w.inter_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0
  )
felm_rd_wo.fes_linear.w.inter_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0
  )
felm_rd_wo.fes_linear.w.inter_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_linear.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_linear.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_linear.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 1.2.2. Including square term
lm_rd_wo.fes_square.w.inter_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2)
  )
felm_rd_wo.fes_square.w.inter_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2)
  )
felm_rd_wo.fes_square.w.inter_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_square.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_square.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_square.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 1.2.3. Including cubic term
lm_rd_wo.fes_cubic.w.inter_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3)
  )
felm_rd_wo.fes_cubic.w.inter_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3)
  )
felm_rd_wo.fes_cubic.w.inter_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
lm_rd_wo.fes_cubic.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_cubic.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period
  )
felm_rd_wo.fes_cubic.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      0 |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


# # 2. Models with Account-Premise-IDs FEs
# # 2.1. Without Interaction Term(s)
# # 2.1.1. Including Linear Term
feols_rd_ids.fes_linear_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 |
      ids_in.factor
  )
feols_rd_ids.fes_linear <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_linear_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_linear_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 2.1.2. Including Square Term
feols_rd_ids.fes_square <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_quadratic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_quadratic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 2.1.3. Including Cubic Term
feols_rd_ids.fes_cubic <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      I(kwh_total_in.percent_normalize_t1_period0^2) +
      I(kwh_total_in.percent_normalize_t1_period0^3) +
        cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_cubic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_cubic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )

# # 2.2. With Interaction Term(s)
# # 2.2.1. Including linear term
feols_rd_ids.fes_linear.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_linear.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_linear.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 2.2.2. Including square term
feols_rd_ids.fes_square.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_square.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_square.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 2.2.3. Including cubic term
feols_rd_ids.fes_cubic.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_cubic.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor
  )
felm_rd_ids.fes_cubic.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


# # 3. Models with Year-Month FEs
# # 3.1. Without Interaction Term(s)
# # 3.1.1. Including Linear Term
feols_rd_bym.fes_linear_no.controls <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.controls_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.controls_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
feols_rd_bym.fes_linear <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 3.1.2. Including Square Term
feols_rd_bym.fes_square <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 3.1.3. Including Cubic Term
feols_rd_bym.fes_cubic <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_cubic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_cubic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )

# # 3.2. With Interaction Term(s)
# # 3.2.1. Including linear term
feols_rd_bym.fes_linear.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 3.2.2. Including square term
feols_rd_bym.fes_square.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 3.2.3. Including cubic term
feols_rd_bym.fes_cubic.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_cubic.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_cubic.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


# # 4. Models with Account-Premise-IDs and Year-Month FEs
# # 4.1. Without Interaction Term(s)
# # 4.1.1. Including Linear Term
feols_rd_both.fes_linear <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_linear_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_linear_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 4.1.2. Including Square Term
feols_rd_both.fes_square <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_quadratic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_quadratic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 4.1.3. Including Cubic Term
feols_rd_both.fes_cubic <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_cubic_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_cubic_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )

# # 4.2. With Interaction Term(s)
# # 4.2.1. Including linear term
feols_rd_both.fes_linear.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_linear.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_linear.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 4.2.2. Including square term
feols_rd_both.fes_square.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_square.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_square.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 4.2.3. Including cubic term
feols_rd_both.fes_cubic.w.inter <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_cubic.w.inter_no.clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor
  )
felm_rd_both.fes_cubic.w.inter_clustering <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
        I(kwh_total_in.percent_normalize_t1_period0^3) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^3) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      ids_in.factor +
        billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


# # 5. Models with Different Dependent/Running Variables
# # 5.1. Backward Cases
# # 5.1.1. Models without Interaction Term(s)
# # 5.1.1.1. Linear Models
# # 5.1.1.1.1. Linear Models with Clustering
felm_rd_bym.fes_linear_clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.1.1.1.2. Linear Models without Clustering
felm_rd_bym.fes_linear_no.clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )

# # 5.1.1.2. Square Models
# # 5.1.1.2.1. Square Models with Clustering
felm_rd_bym.fes_quadratic_clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
        I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
        I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
        I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
        I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
        I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.1.1.2.2. Square Models without Clustering
felm_rd_bym.fes_quadratic_no.clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
        I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
        I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
        I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
        I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
        I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )

# # 5.1.2. Models with Interaction Term(s)
# # 5.1.2.1. Linear Models
# # 5.1.2.1.1. Linear Models with Clustering
felm_rd_bym.fes_linear.w.inter_clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
      is_treated_t1_periodp1:kwh_total_in.percent_normalize_t1_periodp1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
      is_treated_t1_periodp2:kwh_total_in.percent_normalize_t1_periodp2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
      is_treated_t1_periodp3:kwh_total_in.percent_normalize_t1_periodp3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
      is_treated_t1_periodp4:kwh_total_in.percent_normalize_t1_periodp4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
      is_treated_t1_periodp5:kwh_total_in.percent_normalize_t1_periodp5 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.1.2.1.2. Linear Models without Clustering
felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
      is_treated_t1_periodp1:kwh_total_in.percent_normalize_t1_periodp1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
      is_treated_t1_periodp2:kwh_total_in.percent_normalize_t1_periodp2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
      is_treated_t1_periodp3:kwh_total_in.percent_normalize_t1_periodp3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
      is_treated_t1_periodp4:kwh_total_in.percent_normalize_t1_periodp4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
      is_treated_t1_periodp5:kwh_total_in.percent_normalize_t1_periodp5 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
# # 5.1.2.2. Square Models
# # 5.1.2.2.1. Square Models with Clustering
felm_rd_bym.fes_square.w.inter_clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
        I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      is_treated_t1_periodp1:kwh_total_in.percent_normalize_t1_periodp1 +
        is_treated_t1_periodp1:I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
        I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      is_treated_t1_periodp2:kwh_total_in.percent_normalize_t1_periodp2 +
        is_treated_t1_periodp2:I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
        I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      is_treated_t1_periodp3:kwh_total_in.percent_normalize_t1_periodp3 +
        is_treated_t1_periodp3:I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
        I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      is_treated_t1_periodp4:kwh_total_in.percent_normalize_t1_periodp4 +
        is_treated_t1_periodp4:I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
        I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      is_treated_t1_periodp5:kwh_total_in.percent_normalize_t1_periodp5 +
        is_treated_t1_periodp5:I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.1.2.2.2. Square Models without Clustering
felm_rd_bym.fes_square.w.inter_no.clustering_backward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp1 +
      kwh_total_in.percent_normalize_t1_periodp1 +
        I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      is_treated_t1_periodp1:kwh_total_in.percent_normalize_t1_periodp1 +
        is_treated_t1_periodp1:I(kwh_total_in.percent_normalize_t1_periodp1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_backward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp2 +
      kwh_total_in.percent_normalize_t1_periodp2 +
        I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      is_treated_t1_periodp2:kwh_total_in.percent_normalize_t1_periodp2 +
        is_treated_t1_periodp2:I(kwh_total_in.percent_normalize_t1_periodp2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_backward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp3 +
      kwh_total_in.percent_normalize_t1_periodp3 +
        I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      is_treated_t1_periodp3:kwh_total_in.percent_normalize_t1_periodp3 +
        is_treated_t1_periodp3:I(kwh_total_in.percent_normalize_t1_periodp3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_backward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp4 +
      kwh_total_in.percent_normalize_t1_periodp4 +
        I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      is_treated_t1_periodp4:kwh_total_in.percent_normalize_t1_periodp4 +
        is_treated_t1_periodp4:I(kwh_total_in.percent_normalize_t1_periodp4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_backward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodp5 +
      kwh_total_in.percent_normalize_t1_periodp5 +
        I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      is_treated_t1_periodp5:kwh_total_in.percent_normalize_t1_periodp5 +
        is_treated_t1_periodp5:I(kwh_total_in.percent_normalize_t1_periodp5^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )

# # 5.2. Backward Cases
# # 5.2.1. Models without Interaction Term(s)
# # 5.2.1.1. Linear Models
# # 5.2.1.1.1. Linear Models with Clustering
felm_rd_bym.fes_linear_clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.2.1.1.2. Linear Models without Clustering
felm_rd_bym.fes_linear_no.clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
# # 5.2.1.2. Square Models
# # 5.2.1.2.1. Square Models with Clustering
felm_rd_bym.fes_quadratic_clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
        I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
        I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
        I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
        I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.2.1.2.2. Square Models without Clustering
felm_rd_bym.fes_quadratic_no.clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
        I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
        I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
        I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_quadratic_no.clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
        I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
# # 5.2.2. Models with Interaction Term(s)
# # 5.2.2.1. Linear Models
# # 5.2.2.1.1. Linear Models with Clustering
felm_rd_bym.fes_linear.w.inter_clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.2.2.1.2. Linear Models without Clustering
felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )

# # 5.2.2.2. Square Models
# # 5.2.2.2.1. Square Models with Clustering
felm_rd_bym.fes_square.w.inter_clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
        I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
        is_treated_t1_periodm1:I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
        I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
        is_treated_t1_periodm2:I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
        I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
        is_treated_t1_periodm3:I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
        I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
        is_treated_t1_periodm4:I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
# # 5.2.2.2.2. Square Models without Clustering
felm_rd_bym.fes_square.w.inter_no.clustering_forward_p1 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
        I(kwh_total_in.percent_normalize_t1_period0^2) +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
        is_treated_t1_period0:I(kwh_total_in.percent_normalize_t1_period0^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_forward_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
        I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
        is_treated_t1_periodm1:I(kwh_total_in.percent_normalize_t1_periodm1^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_forward_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
        I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
        is_treated_t1_periodm2:I(kwh_total_in.percent_normalize_t1_periodm2^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_forward_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
        I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
        is_treated_t1_periodm3:I(kwh_total_in.percent_normalize_t1_periodm3^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_square.w.inter_no.clustering_forward_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
        I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
        is_treated_t1_periodm4:I(kwh_total_in.percent_normalize_t1_periodm4^2) +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )








####
# # Models including an indicator variable for season changes
felm_rd_bym.fes_linear_clustering_season.change <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_season.change_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_season.change_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_season.change_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_season.change_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_season.change_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_season.change_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_season.change_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_season.change_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_season.change_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


felm_rd_bym.fes_linear.w.inter_clustering_season.change <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      is_season.change_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_season.change_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
      is_season.change_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_season.change_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
      is_season.change_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_season.change_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
      is_season.change_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_clustering_season.change_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
      is_season.change_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


felm_rd_bym.fes_linear_no.clustering_season.change <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_season.change_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_season.change_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_season.change_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_season.change_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_season.change_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_season.change_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_season.change_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_no.clustering_season.change_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_season.change_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )


felm_rd_bym.fes_linear.w.inter_no.clustering_season.change <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_period0 +
      kwh_total_in.percent_normalize_t1_period0 +
      is_treated_t1_period0:kwh_total_in.percent_normalize_t1_period0 +
      is_season.change_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_season.change_p2 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm1 +
      kwh_total_in.percent_normalize_t1_periodm1 +
      is_treated_t1_periodm1:kwh_total_in.percent_normalize_t1_periodm1 +
      is_season.change_periodm1 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_season.change_p3 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm2 +
      kwh_total_in.percent_normalize_t1_periodm2 +
      is_treated_t1_periodm2:kwh_total_in.percent_normalize_t1_periodm2 +
      is_season.change_periodm2 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_season.change_p4 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm3 +
      kwh_total_in.percent_normalize_t1_periodm3 +
      is_treated_t1_periodm3:kwh_total_in.percent_normalize_t1_periodm3 +
      is_season.change_periodm3 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear.w.inter_no.clustering_season.change_p5 <-
  formula(
    kwh_daily.avg ~
      is_treated_t1_periodm4 +
      kwh_total_in.percent_normalize_t1_periodm4 +
      is_treated_t1_periodm4:kwh_total_in.percent_normalize_t1_periodm4 +
      is_season.change_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor
  )
# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02X
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

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(
 PATH_NOTE_DESCRIPTIVE.ANALYSIS, "02_RD-Design/02_Plots", sep = "/"
)


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005


# ------- Define function(s) -------
# (Not Applicable)

generate.condition_for.subset_month <-
  function(bw, rate.codes, year_lower, year_upper, month, suffix) {
    if (is.na(bw)) {
      condition <- paste(
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("billing.month_mid == ", month),
        sep = " & "
      )
    } else {
      condition <- paste(
        paste0(
          bw * -1, " <= kwh_total_in.percent_normalize_", suffix, " & ",
          "kwh_total_in.percent_normalize_", suffix, " <= ", bw
        ),
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("billing.month_mid == ", month),
        sep = " & "
      )
    }

  return(condition)
}


generate.condition_for.subset_season <-
  function(bw, rate.codes, year_lower, year_upper, season, suffix) {
    if (is.na(bw) & is.na(season)) {
      condition <- paste(
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        sep = " & "
      )
    } else if (is.na(bw) & !is.na(season)) {
      condition <- paste(
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("season_before_", suffix, " == \"", season, "\""),
        paste0("season_before_", suffix, " == season_after_", suffix),
        sep = " & "
      )
    } else if (!is.na(bw) & is.na(season)) {
      condition <- paste(
        paste0(
          bw * -1, " <= kwh_total_in.percent_normalize_", suffix, " & ",
          "kwh_total_in.percent_normalize_", suffix, " <= ", bw
        ),
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        sep = " & "
      )
    } else {
      condition <- paste(
        paste0(
          bw * -1, " <= kwh_total_in.percent_normalize_", suffix, " & ",
          "kwh_total_in.percent_normalize_", suffix, " <= ", bw
        ),
        paste0(
          paste0("rate_code_normalize_", suffix, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("season_before_", suffix, " == \"", season, "\""),
        paste0("season_before_", suffix, " == season_after_", suffix),
        sep = " & "
      )
    }

  return(condition)
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


## --------------------------------------------------
## X
## --------------------------------------------------
## ------- X -------
#bw <- 60
#rate.codes <- c("RSGH")
#suffix <- "periodm4"
#condition <- paste0(
#  "!is.na(kwh_daily.avg_", suffix, ") & is.finite(kwh_daily.avg_",
#  suffix, ")"
#)
#results_forward_linear_w.inter_5 <-
#  estimate.wBW_terms_felm_for.subsample(
#    dt = dt_for.reg[eval(parse(text = condition))],
#    model = felm_rd_bym.fes_linear.w.inter_clustering_forward_p5,
#    bw = bw,
#    rate.codes = rate.codes,
#    year_lower = YEAR_LOWER,
#    year_upper = YEAR_UPPER,
#    month_response = 7,
#    suffix = suffix
#  )
#results_forward_square_w.inter_5 <-
#  estimate.wBW_terms_felm_for.subsample(
#    dt = dt_for.reg[eval(parse(text = condition))],
#    model = felm_rd_bym.fes_square.w.inter_clustering_forward_p5,
#    bw = bw,
#    rate.codes = rate.codes,
#    year_lower = YEAR_LOWER,
#    year_upper = YEAR_UPPER,
#    month_response = 7,
#    suffix = suffix
#  )
#
#
## --------------------------------------------------
## X
## --------------------------------------------------
## ------- X -------
#dt_for.reg[
#  kwh_total_in.percent_normalize_periodm4 > (825 - 620) / 620,
#  tmp_treated := TRUE
#]
#dt_for.reg[
#  kwh_total_in.percent_normalize_periodm4 <= (825 - 620) / 620,
#  tmp_treated := FALSE
#]
#
#tmp_model <-
#  formula(
#    kwh_daily.avg ~
#      is_treated_periodm4 +
#        tmp_treated +
#      kwh_total_in.percent_normalize_periodm4 +
#      is_treated_periodm4:kwh_total_in.percent_normalize_periodm4 +
#        tmp_treated:kwh_total_in.percent_normalize_periodm4 +
#      cdd_daily.avg_period +
#        hdd_daily.avg_period |
#      billing.ym_mid_in.factor |
#      0 |
#      ids_in.factor + billing.ym_mid_in.factor
#  )
#
#tmp_results <-
#  estimate.wBW_terms_felm_for.subsample(
#    dt = dt_for.reg[eval(parse(text = condition))],
#    model = tmp_model,
#    bw = bw,
#    rate.codes = rate.codes,
#    year_lower = YEAR_LOWER,
#    year_upper = YEAR_UPPER,
#    month_response = 7,
#    suffix = suffix
#  )


# --------------------------------------------------
# X
# --------------------------------------------------
# load("/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/Test.RData")


DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month_Tier-1.parquet"
PATH_TO.LOAD_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD_RESULTS,
  sep = "/"
)
dt_for.plot <-
  pq.to.dt(
    PATH_TO.LOAD_RD_RESULTS,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )



z_95p <- qnorm(1 - 0.05 / 2, mean = 0, sd = 1)
dt_for.plot[
  ,
  `:=` (
    ci_lower = estimates - se_max * z_95p,
    ci_upper = estimates + se_max * z_95p
  )
]

dt_for.plot[, term_btw.periods := factor(term_btw.periods)]


# categories <- c("Forward", "Backward")
categories <- c("Backward")
functional.forms <- c("Linear", "Square")
models <- c("With BYM FEs", "Interaction with BYM FEs")
conditions <- expand.grid(categories, functional.forms, models) %>% setDT(.)
names(conditions) <- c("category", "functional.form", "model")
for (row in 1:conditions[, .N]) {
  tmp_category <- conditions[row]$category %>% as.character(.)
  tmp_functional.form <- conditions[row]$functional.form %>% as.character(.)
  tmp_model <- conditions[row]$model %>% as.character(.)

  if (tmp_functional.form == "Linear") {
    tmp_obj.name_functional.form <- "linear"
    tmp_file.name_functional.form <- "Linear"
  } else if (tmp_functional.form == "Square") {
    tmp_obj.name_functional.form <- "sqaure"
    tmp_file.name_functional.form <- "Square"
  }
  if (tmp_model == "With BYM FEs") {
    tmp_obj.name_model <- "wo.interaction"
    tmp_file.name_model <- "Without-Interaction"
  } else {
    tmp_obj.name_model <- "w.interaction"
    tmp_file.name_model <- "With-Interaction"
  }

  tmp_obj.name <- paste(
    "plot_estimates",
    tmp_obj.name_functional.form,
    tmp_obj.name_model,
    sep = "_"
  )
  tmp_file.name <- paste(
    "Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods",
    tmp_file.name_functional.form,
    tmp_file.name_model,
    tmp_category,
    "Season-Level",
    sep = "_"
  )

  assign(
    tmp_obj.name,
    ggplot(
      dt_for.plot[
        str_detect(var_independent, "^1\\[Treated\\]") &
          category == tmp_category &
          functional.form == tmp_functional.form &
          model == tmp_model
      ],
      aes(
        x = bw_in.str, y = estimates,
        ymin = ci_lower, ymax = ci_upper,
        color = term_btw.periods
      )
    ) +
      facet_grid(rate.code ~ season) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.7) +
      geom_point(position = position_dodge(width = 0.7)) +
      geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
      theme_linedraw() +
      # scale_x_continuous(scales::percent) +
      scale_color_viridis_d() +
      theme(strip.text = element_text(face = "bold")) +
      labs(
        x = "Month for which Treatment Effects are estimated",
        y = "Estimated Treatment Effects",
        color = "Term(s)\nbetween\nPeriods",
        subtitle = paste0(
          tmp_category, " Cases, ",
          str_replace(tmp_file.name_model, "_", " ")
        )
      )
  )

  plot.save(
    paste(DIR_TO.SAVE_PLOTS, tmp_file.name, sep = "/") %>%
      paste0(., ".png"),
    get(tmp_obj.name),
    width = 35, height = 20, units = "cm"
  )
}


# --------------------------------------------------
# Create Plot(s): Scatter Plot(s) with Different Splines
# --------------------------------------------------
# ------- Set conditions to subset a DT -------
bw <- 50
rate.codes <- c("RSGH")
season <- "Summer"
suffix_tier <- "t2"
suffix_period <- "periodm4"
condition_to.subset <- generate.condition_to.subset_season(
  bw = bw, rate.codes = rate.codes,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  season = season, suffix_tier = suffix_tier, suffix_period = suffix_period
)

# ------- Create ggplot object(s) -------
plot_w.splines <-
 ggplot(
   dt_for.reg[eval(parse(text = condition_to.subset))],
   aes(x = kwh_total_in.percent_normalize_t2_periodm4 / 100, y = kwh_daily.avg)
 ) +
   geom_point(alpha = 0.1, size = 0.5) +
   geom_vline(
     xintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.7
   ) +
   geom_smooth(
     aes(color = is_treated_t2_periodm4),
     method = lm, formula = y ~ x, linetype = "dotted"
   ) +
   geom_smooth(
     aes(color = is_treated_t2_periodm4),
     method = lm, formula = y ~ splines::bs(x, 3)
   ) +
   scale_x_continuous(labels = scales::percent) +
   scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
   theme_linedraw() +
   labs(
     x = "Normalized Consumption in Period -4 relative to Base Usage Qty (%)",
     y = "Daily Average Consumption in Period 1 (kWh/Day)",
     color = "Treatment"
   )


# --------------------------------------------------
# X
# --------------------------------------------------
felm_rd_bym.fes_linear_clustering_forward_term1 <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )
felm_rd_bym.fes_linear_clustering_forward_term5 <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_periodm4 +
      cdd_daily.avg_period +
        hdd_daily.avg_period |
      billing.ym_mid_in.factor |
      0 |
      ids_in.factor + billing.ym_mid_in.factor
  )


bw <- 50
rate.codes <- c("RSGH")
month_term1 <- 2
suffix_term1 <- "period0"
month_term5 <- 6
suffix_term5 <- "periodm4"


condition_to.subset_term1 <- generate.condition_for.subset_month(
  bw = bw, rate.codes = rate.codes,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  month = month_term1,
  suffix = suffix_term1
)
condition_to.subset_term5 <- generate.condition_for.subset_month(
  bw = bw, rate.codes = rate.codes,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  month = month_term5,
  suffix = suffix_term5
)

results_term1 <- estimate.wBW_terms_felm_for.subsample(
  dt = dt_for.reg,
  model = felm_rd_bym.fes_linear_clustering_forward_term1,
  bw = bw, rate.codes = rate.codes,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  month_response = month_term1, suffix = suffix_term1
)
results_term5 <- estimate.wBW_terms_felm_for.subsample(
  dt = dt_for.reg,
  model = felm_rd_bym.fes_linear_clustering_forward_term5,
  bw = bw, rate.codes = rate.codes,
  year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
  month_response = month_term5, suffix = suffix_term5
)

cols_to.extract <- c(
  "kwh_total_in.percent_normalize_period0", "is_treated_period0",
  "kwh_total_in.percent_normalize_periodm4", "is_treated_periodm4",
  "kwh_daily.avg"
)
dt_subset_term1 <-
  dt_for.reg[
    eval(parse(text = condition_to.subset_term1)),
    .SD, .SDcols = cols_to.extract
  ]
dt_subset_term1[
  ,
  `:=` (
    residual = results_term1$residuals,
    category = "1-Term"
  )
]

dt_subset_term5 <-
  dt_for.reg[
    eval(parse(text = condition_to.subset_term5)),
    .SD, .SDcols = cols_to.extract
  ]
dt_subset_term5[
  ,
  `:=` (
    residual = results_term5$residuals,
    category = "5-Terms"
  )
]

dt_subset <- rbind(dt_subset_term1, dt_subset_term5)
dt_subset[
  category == "1-Term",
  `:=` (
    kwh_total_in.percent_normalize = kwh_total_in.percent_normalize_period0,
    is_treated = is_treated_period0
  )
]
dt_subset[
  category == "5-Terms",
  `:=` (
    kwh_total_in.percent_normalize = kwh_total_in.percent_normalize_periodm4,
    is_treated = is_treated_periodm4
  )
]


plot_residuals <-
  ggplot(
    dt_subset, aes(x = kwh_total_in.percent_normalize / 100, y = residual)
  ) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.7
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.7
    ) +
    geom_smooth(
      aes(color = is_treated), method = lm, formula = y ~ x, linetype = "dotted"
    ) +
    geom_smooth(
      aes(color = is_treated), method = lm, formula = y ~ splines::bs(x, 3)
    ) +
    facet_grid(category ~ .) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(-0.5, 0.5, by = 0.1)
    ) +
    scale_y_continuous(
      breaks = seq(-60, 60, by = 10), labels = seq(-60, 60, by = 10),
      limits = c(-50, 50)
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold")) +
    labs(
      x = "Normalized Consumption relative to Base Usage Qty (%)",
      y = "Daily Average Consumption in Period 1 (kWh/Day)",
      color = "Treatment"
    )





#test <-
#  ggplot(
#    dt_for.reg[
#      rate_code_normalize %in% c("RSGH") &
#        billing.month_mid_period0 %in% c(6:8) &
#        billing.year_mid_period0 %in% c(2005:2011) &
#        -20 <= kwh_total_in.percent_normalize_period0 &
#        kwh_total_in.percent_normalize_period0 <= 20
#    ]
#  ) +
#    geom_point(
#      aes(x = kwh_total_in.percent_normalize_period0, y = kwh_daily.avg_periodp1),
#      alpha = 0.1, size = 0.5
#    )


# --------------------------------------------------
# X
# --------------------------------------------------
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS,
    "SMUD-Billing-Data_RD-Design_Scatter_Spline-Fittings_Reponse-on-Jun_5-Terms.png",
    sep = "/"
  ),
  plot_w.splines,
  width = 30, height = 20, units = "cm"
)

plot.save(
  paste(
    DIR_TO.SAVE_PLOTS,
    "SMUD-Billing-Data_RD-Design_Scatter_Residuls.png",
    sep = "/"
  ),
  plot_residuals,
  width = 30, height = 25, units = "cm"
)





tmp_1 <- ggplot(dt_for.reg[ids == 4049], aes(x = kwh_total_in.percent_normalize_period0, y = kwh_total_in.percent_normalize)) + geom_point() + scale_y_continuous(limits = c(-100, 300)) + geom_abline(slope = 1, intercept = 0, color = "red") + geom_path(aes(color = as.numeric(billing.ym_mid)))
tmp_2 <- ggplot(dt_for.reg[ids == 634576], aes(x = kwh_total_in.percent_normalize_period0, y = kwh_total_in.percent_normalize)) + geom_point() + scale_y_continuous(limits = c(-100, 300)) + geom_abline(slope = 1, intercept = 0, color = "red") + geom_path(aes(color = as.numeric(billing.ym_mid)))
plot.save("Tmp_1.png", tmp_1, width = 30, height = 25, units = "cm")
plot.save("Tmp_2.png", tmp_2, width = 30, height = 25, units = "cm")


ggplot(dt_for.reg[ids == 4049], aes(x = kwh_daily.avg_period0, y = kwh_daily.avg)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_path(aes(color = as.numeric(billing.ym_mid)))
ggplot(dt_for.reg[ids == 634576], aes(x = kwh_daily.avg_period0, y = kwh_daily.avg)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_path(aes(color = as.numeric(billing.ym_mid)))
# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02G
# #
# > Purpose of the script(s)
# # : Generate plots.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(stringr)
library(lfe)
library(unikn)
library(gridExtra)
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
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_RD <- "RD-Approach"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Approach.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Plots", sep = "/")
# # 2.2. Path at which Table(s) will be saved
DIR_TO.SAVE_TABLES <- paste(PATH_NOTE_DESCRIPTIVE.ANALYSIS, "Tables", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Run regression with a specific bandwidth
estimate.by.bw <- function (model, bw) {
  if (bw == 0) {
    tmp_results <-
      felm(get(model), data = dt_for.reg[is_balanced.ids_bw_na == TRUE])
    return(tmp_results)
  } else {
    tmp_condition <-
      paste0(
        bw * -1,
        paste0(
          " <= kwh_total_in.percent_normalize_period0 & ",
          "kwh_total_in.percent_normalize_period0 <= "
        ),
        bw,
        paste0(" & is_balanced.ids_bw_", bw, " == TRUE")
      )

    tmp_results <-
      felm(get(model), data = dt_for.reg[eval(parse(text = tmp_condition))])
    return(tmp_results)
  }
}

# # 2. Create ggplot objects illustrating spline fits from residuals
create.plots <- function(bw) {
  # ## Run regressions
  results_1o <- estimate.by.bw("model_1o", bw)
  results_2o <- estimate.by.bw("model_2o", bw)
  results_3o <- estimate.by.bw("model_3o", bw)

  # ## Generate a DTs that are used to create ggplot objects
  if (bw == 0) {
    dt_base <-
      dt_for.reg[
        is_balanced.ids_bw_na == TRUE,
        .SD, .SDcols = c("kwh_total_in.percent_normalize_period0", "is_treated")
      ]
  } else {
    dt_base <- dt_for.reg[
      get(paste0("is_balanced.ids_bw_", bw)) == TRUE &
        bw * -1 <= kwh_total_in.percent_normalize_period0 &
        kwh_total_in.percent_normalize_period0 <= bw,
      .SD, .SDcols = c("kwh_total_in.percent_normalize_period0", "is_treated")
    ]
  }

  resiual_1o <- as.data.table(results_1o$residuals)
  setnames(resiual_1o, "kwh_daily.avg", "Linear Model")
  resiual_2o <- as.data.table(results_2o$residuals)
  setnames(resiual_2o, "kwh_daily.avg", "Square Model")
  resiual_3o <- as.data.table(results_3o$residuals)
  setnames(resiual_3o, "kwh_daily.avg", "Cubic Model")

  dt_for.plot <-
    cbind(dt_base, resiual_1o, resiual_2o, resiual_3o) %>%
      melt(
        .,
        id.vars = c("kwh_total_in.percent_normalize_period0", "is_treated"),
        measure.vars = c("Linear Model", "Square Model", "Cubic Model"),
        variable.name = "category",
        value.name = "kwh_daily.avg"
      )

  # ## Make ggplot objects
  grob <-
    ggplot(dt_for.plot) +
      facet_grid(. ~ category) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
        y = "Daily Average Consumption in Period 1, Residuals",
        color = "Treatment"
      ) +
      theme_light()
  plot_1p <-
    grob +
      geom_smooth(
        aes(
          x = kwh_total_in.percent_normalize_period0 / 100,
          y = kwh_daily.avg,
          color = is_treated
        ),
        method = lm, formula = y ~ splines::bs(x, degree = 1)
      ) +
      labs(x = "", y = "") +
      guides(color = "none")
  plot_2p <-
    grob +
      geom_smooth(
        aes(
          x = kwh_total_in.percent_normalize_period0 / 100,
          y = kwh_daily.avg,
          color = is_treated
        ),
        method = lm, formula = y ~ splines::bs(x, degree = 2)
      ) +
      labs(x = "") +
      guides(color = "none")
  plot_3p <-
    grob +
      geom_smooth(
        aes(
          x = kwh_total_in.percent_normalize_period0 / 100,
          y = kwh_daily.avg,
          color = is_treated
        ),
        method = lm, formula = y ~ splines::bs(x, degree = 3)
      ) +
      labs(y = "") +
      guides(color = "none")

  # ## Set limits
  if (bw == 0) {
    plot_1p <-
      plot_1p + scale_x_continuous(labels = scales::percent, limits = c(-1, 1))
    plot_2p <-
      plot_2p + scale_x_continuous(labels = scales::percent, limits = c(-1, 1))
    plot_3p <-
      plot_3p + scale_x_continuous(labels = scales::percent, limits = c(-1, 1))
  }

  return(grid.arrange(plot_1p, plot_2p, plot_3p, ncol = 1))
}


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date= "(^date)|(_from$)|(_to$)",
    is_drop.index_cols= TRUE
  )

# # 2. Check primary keys of the DTs
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Create ggplot objects
# --------------------------------------------------
# ------- For mean of daily average consumption by range -------
# # 1. Generate a DT for plotting
results_t.test_pop.mean <-
  dt_for.reg[
    !is.na(range_kwh_daily.avg_normalize_period0) & is_balanced.ids_bw_50,
    t.test(kwh_daily.avg),
    by = .(range_kwh_daily.avg_normalize_period0)
  ]
results_t.test_pop.mean[
  ,
  category :=
    rep(
      c("conf.int_lower", "conf.int_upper"),
      times = results_t.test_pop.mean[, .N] / 2
    )
]
dt_results_t.test_pop.mean <-
  dcast(
    results_t.test_pop.mean,
    range_kwh_daily.avg_normalize_period0 + estimate ~ category,
    value.var = "conf.int"
  )

# # 2. Generate a ggplot object
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot_mean <-
  ggplot(data = dt_results_t.test_pop.mean) +
    geom_ribbon(
      aes(
        x = range_kwh_daily.avg_normalize_period0,
        ymin = conf.int_lower,
        ymax = conf.int_upper,
        group = 1
      ),
      fill = "grey60", alpha = 0.5
    ) +
    geom_line(
      aes(x = range_kwh_daily.avg_normalize_period0, y = estimate, group = 1),
      color = color.palette[1]
    ) +
    geom_vline(xintercept = "(-1,0]", linetype = "dashed") +
    scale_y_continuous(breaks = seq(15, 35, by = 5), limits = c(15, 35)) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(
      x = "Ranges of Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
      y = "Mean of Daily Average Consumption (kWh per Day)",
      subtitle = "Residential Consumers' Daily Average Consumption by Range of Normalized Consumption in Period relative to Base Usage Qty"
    )


# ------- For spline fits using daily average consumption -------
# # 1. Generate ggplot objects
bws <- c(0, 20, 10, 5)
orders <- 1:3
for (bw in bws) {
  for (order in orders) {
    tmp_go.name <- paste("go_spline", bw, order, sep = "_")

    if (bw == 0) {
      assign(
        tmp_go.name,
        ggplot(
          data = dt_for.reg[is_balanced.ids_bw_na == TRUE],
          aes(
            x = kwh_total_in.percent_normalize_period0 / 100,
            y = kwh_daily.avg,
            color = is_treated
          )
        )
      )
    } else {
      tmp_col.name <- paste0("is_balanced.ids_bw_", bw)
      assign(
        tmp_go.name,
        ggplot(
          data =
            dt_for.reg[
              get(tmp_col.name) == TRUE &
                bw * -1 <= kwh_total_in.percent_normalize_period0 &
                kwh_total_in.percent_normalize_period0 <= bw
            ],
          aes(
            x = kwh_total_in.percent_normalize_period0 / 100,
            y = kwh_daily.avg,
            color = is_treated
          )
        )
      )
    }
  }
}

# # 2. Create ggplot objects
# # 2.1. Set plot options
plot.options <-
  list(
    theme_linedraw(),
    scale_x_continuous(labels = scales::percent),
    labs(
      x = "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
      y = "Daily Average Consumption in Period 1",
      color = "Treatment"
    )
  )
plot.options_na <-
  list(
    theme_linedraw(),
    scale_x_continuous(labels = scales::percent, limits = c(-1, 1)),
    labs(
      x = "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
      y = "Daily Average Consumption in Period 1",
      color = "Treatment"
    )
  )

# # 2.2. Update ggplot objects
plot_spline_na_1 <-
  go_spline_0_1 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 1)) +
    labs(subtitle = "Spline Fits, Degree 1") +
    plot.options_na
  plot_spline_na_2 <-
  go_spline_0_2 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 2)) +
    labs(subtitle = "Spline Fits, Degree 2") +
    plot.options_na
plot_spline_na_3 <-
  go_spline_0_3 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 3)) +
    labs(subtitle = "Spline Fits, Degree 3") +
    plot.options_na

plot_spline_20_1 <-
  go_spline_20_1 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 1)) +
    labs(subtitle = "Spline Fits, Degree 1") +
    plot.options
plot_spline_20_2 <-
  go_spline_20_2 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 2)) +
    labs(subtitle = "Spline Fits, Degree 2") +
    plot.options
plot_spline_20_3 <-
  go_spline_20_3 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 3)) +
    labs(subtitle = "Spline Fits, Degree 3") +
    plot.options

plot_spline_10_1 <-
  go_spline_10_1 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 1)) +
    labs(subtitle = "Spline Fits, Degree 1") +
    plot.options
plot_spline_10_2 <-
  go_spline_10_2 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 2)) +
    labs(subtitle = "Spline Fits, Degree 2") +
    plot.options
plot_spline_10_3 <-
  go_spline_10_3 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 3)) +
    labs(subtitle = "Spline Fits, Degree 3") +
    plot.options

plot_spline_5_1 <-
  go_spline_5_1 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 1)) +
    labs(subtitle = "Spline Fits, Degree 1") +
    plot.options
plot_spline_5_2 <-
  go_spline_5_2 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 2)) +
    labs(subtitle = "Spline Fits, Degree 2") +
    plot.options
plot_spline_5_3 <-
  go_spline_5_3 +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 3)) +
    labs(subtitle = "Spline Fits, Degree 3") +
    plot.options

# # 2.3. Combine ggplot objects
plot_spline_na <-
  grid.arrange(
    plot_spline_na_1, plot_spline_na_2, plot_spline_na_3,
    ncol = 1
  )
plot_spline_20 <-
  grid.arrange(
    plot_spline_20_1, plot_spline_20_2, plot_spline_20_3,
    ncol = 1
  )
plot_spline_10 <-
  grid.arrange(
    plot_spline_10_1, plot_spline_10_2, plot_spline_10_3,
    ncol = 1
  )
plot_spline_5 <-
  grid.arrange(
    plot_spline_5_1, plot_spline_5_2, plot_spline_5_3,
    ncol = 1
  )


# ------- For spline fits using residuals from regressions -------
# # 1. Define models
model_3o <-
  formula(
    kwh_daily.avg ~
      I(kwh_total_in.percent_normalize_period0^3) +
        I(kwh_total_in.percent_normalize_period0^2) +
        kwh_total_in.percent_normalize_period0 +
        cdd_daily.avg_period +
        hdd_daily.avg_period | factor(ids) + factor(billing.ym_mid)
  )
model_2o <-
  formula(
    kwh_daily.avg ~
      I(kwh_total_in.percent_normalize_period0^2) +
        kwh_total_in.percent_normalize_period0 +
        cdd_daily.avg_period +
        hdd_daily.avg_period | factor(ids) + factor(billing.ym_mid)
  )
model_1o <-
  formula(
    kwh_daily.avg ~
      kwh_total_in.percent_normalize_period0 +
        cdd_daily.avg_period +
        hdd_daily.avg_period | factor(ids) + factor(billing.ym_mid)
  )

# # 2. Create ggplot objects
plot_residuals_bw_5 <- create.plots(5)
plot_residuals_bw_10 <- create.plots(10)
plot_residuals_bw_20 <- create.plots(20)
plot_residuals_bw_na <- create.plots(0)


# --------------------------------------------------
# Export ggplot objects in PNG format
# --------------------------------------------------
# ------- Export ggplot objects in PNG format -------
# # 1. Plot of the mean of daily average consumption by range
PLOT.NAME_MEAN <- "SMUD-Billing-Data_RD-Approach_Mean-by-Range.png"
PATH_PLOT_MEAN <- paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_MEAN, sep = "/")
plot.save(
  PATH_PLOT_MEAN,
  plot_mean,
  width = 45, height = 25, units = "cm"
)

# # 2. Plots of spline fits using daily average consumption
PLOT.NAME_SPLINE_BW_5 <- "SMUD-Billing-Data_RD-Approach_Spline_BW-5.png"
PATH_PLOT_SPLINE_BW_5 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SPLINE_BW_5, sep = "/")
plot.save(
  PATH_PLOT_SPLINE_BW_5,
  plot_spline_5,
  width = 30, height = 25, units = "cm"
)

PLOT.NAME_SPLINE_BW_10 <- "SMUD-Billing-Data_RD-Approach_Spline_BW-10.png"
PATH_PLOT_SPLINE_BW_10 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SPLINE_BW_10, sep = "/")
plot.save(
  PATH_PLOT_SPLINE_BW_10,
  plot_spline_10,
  width = 30, height = 25, units = "cm"
)

PLOT.NAME_SPLINE_BW_20 <- "SMUD-Billing-Data_RD-Approach_Spline_BW-20.png"
PATH_PLOT_SPLINE_BW_20 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SPLINE_BW_20, sep = "/")
plot.save(
  PATH_PLOT_SPLINE_BW_20,
  plot_spline_20,
  width = 30, height = 25, units = "cm"
)

PLOT.NAME_SPLINE_BW_NA <- "SMUD-Billing-Data_RD-Approach_Spline_BW-NA.png"
PATH_PLOT_SPLINE_BW_NA <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SPLINE_BW_NA, sep = "/")
plot.save(
  PATH_PLOT_SPLINE_BW_NA,
  plot_spline_na,
  width = 30, height = 25, units = "cm"
)

# # 3. Plots of spline fits using residuals from regressions
PLOT.NAME_RESIDUALS_BW_5 <- "SMUD-Billing-Data_RD-Approach_Residuals_BW-5.png"
PATH_PLOT_RESIDUALS_BW_5 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_RESIDUALS_BW_5, sep = "/")
plot.save(
  PATH_PLOT_RESIDUALS_BW_5,
  plot_residuals_bw_5,
  width = 35, height = 25, units = "cm"
)

PLOT.NAME_RESIDUALS_BW_10 <- "SMUD-Billing-Data_RD-Approach_Residuals_BW-10.png"
PATH_PLOT_RESIDUALS_BW_10 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_RESIDUALS_BW_10, sep = "/")
plot.save(
  PATH_PLOT_RESIDUALS_BW_10,
  plot_residuals_bw_10,
  width = 35, height = 25, units = "cm"
)

PLOT.NAME_RESIDUALS_BW_20 <- "SMUD-Billing-Data_RD-Approach_Residuals_BW-20.png"
PATH_PLOT_RESIDUALS_BW_20 <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_RESIDUALS_BW_20, sep = "/")
plot.save(
  PATH_PLOT_RESIDUALS_BW_20,
  plot_residuals_bw_20,
  width = 35, height = 25, units = "cm"
)

PLOT.NAME_RESIDUALS_BW_NA <- "SMUD-Billing-Data_RD-Approach_Residuals_BW-NA.png"
PATH_PLOT_RESIDUALS_BW_NA <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_RESIDUALS_BW_NA, sep = "/")
plot.save(
  PATH_PLOT_RESIDUALS_BW_NA,
  plot_residuals_bw_na,
  width = 35, height = 25, units = "cm"
)

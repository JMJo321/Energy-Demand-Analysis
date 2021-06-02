# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02L
# #
# > Purpose of the script(s)
# # :

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
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
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")
# # 1.2. SMUD Residential Rate Schedules
DIR_TO.LOAD_SCHEDULES <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_SCHEDULES <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_SCHEDULES <- paste(
  PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_SCHEDULES, FILE_TO.LOAD_SCHEDULES,
  sep = "/"
)

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, "02_RD-Design/02_Plots", sep = "/"
)


# ------- Define parameter(s) -------
# # 1. To select observations that will be used to make plots
NORMALIZED.MONTHLY.QTY_UPPER <- 500
MONTHLY.QTY_UPPER <- 7500
DAILY.QTY_UPPER <- 250

# # 2. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005
# ## Note:
# ## There are no observation in summer in 2004.


# ------- Define function(s) -------
# # 1. Generate a DT including t-Test results, which is used to make plots
do_t.test <- function(ranges, rate.codes, season) {
  if (is.null(rate.codes) | is.null(season)) {
    tmp_results_t.test <-
      dt_for.reg[
        !is.na(range_kwh_total_normalize_period0) &
          range_kwh_total_normalize_period0 %in% ranges,
        t.test(kwh_daily.avg),
        keyby = .(range_kwh_total_normalize_period0)
      ]
  } else {
    tmp_results_t.test <-
      dt_for.reg[
        !is.na(range_kwh_total_normalize_period0) &
          range_kwh_total_normalize_period0 %in% ranges &
          rate_code_normalize_period0 %in% rate.codes &
          season_before_period0 == season &
          season_before_period0 == season_after_period0,
        t.test(kwh_daily.avg),
        keyby = .(range_kwh_total_normalize_period0)
      ]
  }
  tmp_results_t.test[
    ,
    category := rep(
      c("conf.int_lower", "conf.int_upper"),
      times = tmp_results_t.test[, .N] / 2
    )
  ]

  return(
    dcast(
      tmp_results_t.test,
      range_kwh_total_normalize_period0 + estimate ~ category,
      value.var = "conf.int"
    )
  )
}

# # 2. Generate a string that will be used to subset rows
generator_subset.condition_for.subsample <- function(
  bw, rate.codes, season, year_lower, year_upper, suffix
) {
  if (is.na(bw)) {
    return(
      paste(
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
    )
  } else {
    return(
      paste(
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
    )
  }
}

# # 3. Generate a DT containing decile info. by bandwidth
extract_decile.info_by.bw <- function(
  bws_in.list, rate.codes, season, year_lower, year_upper, suffix
) {
  tmp_conditions <-
    lapply(
      X = bws_in.list, FUN = generator_subset.condition_for.subsample,
      rate.codes = rate.codes, season = season,
      year_lower = year_lower, year_upper = year_upper,
      suffix = suffix
    )

  get.tmp_results <- function(condition) {
    tmp_results <-
      dt_for.reg[eval(parse(text = condition))]$kwh_daily.avg %>%
        quantile(., probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE) %>%
        as.data.table(., keep.rownames = TRUE)
    names(tmp_results) <- c("decile", "value")

    return(tmp_results)
  }

  tmp_dt <- rbindlist(
    lapply(X = tmp_conditions, FUN = get.tmp_results),
    idcol = "bw"
  )
  tmp_dt[
    ,
    `:=` (
      rate.codes = str_c(rate.codes, collapse = " & "),
      season = season
    )
  ]

  return(tmp_dt)
}


# --------------------------------------------------
# Load Datasets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a data file
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )

# # 2. Check primary keys of the DT
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Residential Rate Schedule Data -------
# # 1. Load a data file
dt_rrs <-
  pq.to.dt(
    PATH_TO.LOAD_SCHEDULES,
    reg.ex_date = "(^date)|(^season_)",
    is_drop.index_cols = TRUE
  )

# # 2. Check primary keys of the DT
stopifnot(dt_rrs[, .N, by = .(date, rate_code_normalize)][N > 1, .N] == 0)


# --------------------------------------------------
# Make Plots: Mean of Daily Average Consumption
# --------------------------------------------------
# ------- Create objects that will be used to make plots -------
# # 1. Subset ranges
ranges <-
  dt_for.reg[
    !is.na(range_kwh_total_normalize_period0), .N,
    keyby = .(range_kwh_total_normalize_period0)
  ][
    1:200
  ]$range_kwh_total_normalize_period0
# ## Note: Only care about observations up to "(199,200]".


# # 2. Generate DTs containing t-test results
# # 2.1. By using the full sample
dt_results_t.test_mean <-
  do_t.test(ranges = ranges, rate.codes = NULL, season = NULL)

# # 2.2. By Rate Code and Season: RSGH
# # 2.2.1. For RSGH and Summer
dt_results_t.test_mean_g.code_summer <-
  do_t.test(ranges = ranges, rate.codes = c("RSGH"), season = "Summer")
# # 2.2.2. For RSGH and Winter
dt_results_t.test_mean_g.code_winter <-
  do_t.test(ranges = ranges, rate.codes = c("RSGH"), season = "Winter")

# # 2.3. By Rate Code and Season: RSCH & RSEH
# # 2.3.1. For RSCH & RSEH and Summer
dt_results_t.test_mean_ce.codes_summer <-
  do_t.test(ranges = ranges, rate.codes = c("RSCH", "RSEH"), season = "Summer")
# # 2.3.2. For RSCH & RSEH and Winter
dt_results_t.test_mean_ce.codes_winter <-
  do_t.test(ranges = ranges, rate.codes = c("RSCH", "RSEH"), season = "Winter")

# # 2.4. Make a DT from the DTs above
dt_results_t.test_mean_by.code <- rbind(
  dt_results_t.test_mean_g.code_summer[
    , `:=`(rate.codes = "RSGH", season = "Summer")
  ],
  dt_results_t.test_mean_g.code_winter[
    , `:=`(rate.codes = "RSGH", season = "Winter")
  ],
  dt_results_t.test_mean_ce.codes_summer[
    , `:=`(rate.codes = "RSCH & RSEH", season = "Summer")
  ],
  dt_results_t.test_mean_ce.codes_winter[
    , `:=`(rate.codes = "RSCH & RSEH", season = "Winter")
  ]
)


# # 3. Generate a DT containing Base Usage Information of SMUD Rate Schedules
dt_base.usage <-
  dt_rrs[
    !is.na(tier_2_qty_upto_in_kwh) &
      rate_code_normalize %in% c("RSCH", "RSEH", "RSGH"),
    .N,
    keyby = .(
      rate_code_normalize, season,
      tier_1_qty_upto_in_kwh, tier_2_qty_upto_in_kwh
    )
  ][
    , N := NULL
  ]
dt_base.usage[
  ,
  tier_2_qty_in.percent :=
    (tier_2_qty_upto_in_kwh - tier_1_qty_upto_in_kwh) / tier_1_qty_upto_in_kwh
]
dt_base.usage[
  ,
  tier_2_qty_in.percent_rounded :=
    round(tier_2_qty_in.percent, digits = 2) * 100
]
dt_base.usage[
  ,
  x.intercept_tier2 := paste0(
    "(", tier_2_qty_in.percent_rounded - 1, ",",
    tier_2_qty_in.percent_rounded, "]"
  )
]
dt_base.usage[rate_code_normalize == "RSGH", rate.codes := "RSGH"]
dt_base.usage[is.na(rate.codes), rate.codes := "RSCH & RSEH"]
dt_base.usage[, season_before_period0 := season]

dt_base.usage_reduced <-
  dt_base.usage[
    , .N, keyby = .(rate.codes, season, x.intercept_tier2)
  ][
    , N := NULL
  ]


# ------- Set plot options -------
# # 1. To create object used in setting plot options
ranges_for.plot <-
  dt_results_t.test_mean[
    , id := 1:.N
  ][
    , remainder := id %% 10
  ][
    id == 1 | remainder == 0
  ]$range_kwh_total_normalize_period0
color.palette <- unikn::usecol(pal = pal_signal, n = 3)

# # 2. To set plot options
plot.options <- list(
  geom_vline(
    xintercept = "(-1,0]", linetype = "dashed", color = "black", alpha = 0.7
  ),
  scale_x_discrete(breaks = ranges_for.plot),
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 65)),
  labs(
    x = "Ranges of Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
    y = "Mean of Daily Average Consumption (kWh per Day)",
    color = "", fill = "", shape = "", linetype = ""
  ),
  theme_linedraw(),
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text = element_text(face = "bold")
  )
)


# ------- Generate ggplot objects -------
# # 1. By using the full sample
plot_mean <-
  ggplot(
    dt_results_t.test_mean,
    aes(
      x = range_kwh_total_normalize_period0,
      y = estimate, ymin = conf.int_lower, ymax = conf.int_upper,
      group = 1
    )
  ) +
    geom_ribbon(fill = "grey70", alpha = 0.7) +
    geom_line(color =  color.palette[1]) +
    plot.options

# # 2. By Rate Code: Facet by Season
plot_mean_by.code_facet.y.season <-
  ggplot(
    dt_results_t.test_mean_by.code,
    aes(
      x = range_kwh_total_normalize_period0,
      y = estimate, ymin = conf.int_lower, ymax = conf.int_upper,
      group = rate.codes
    )
  ) +
    facet_grid(season ~ .) +
    geom_vline(
      data = dt_base.usage_reduced,
      aes(
        xintercept = x.intercept_tier2,
        color = rate.codes, linetype = rate.codes
      )
    ) +
    geom_ribbon(aes(fill = rate.codes), alpha = 0.3) +
    geom_point(
        data = dt_results_t.test_mean_by.code[
          range_kwh_total_normalize_period0 %in% ranges_for.plot
        ],
          aes(
              x = range_kwh_total_normalize_period0,
                y = estimate,
                color = rate.codes, shape = rate.codes
          )
    ) +
    geom_line(aes(color = rate.codes)) +
    scale_linetype_manual(values = c("dotted", "dashed")) +
    scale_shape_manual(values = c(15, 16)) +
    plot.options

# # 3. By Rate Code: Facet by Rate Code
plot_mean_by.code_facet.y.code <-
  ggplot(
    dt_results_t.test_mean_by.code,
    aes(
      x = range_kwh_total_normalize_period0,
      y = estimate, ymin = conf.int_lower, ymax = conf.int_upper,
      group = season
    )
  ) +
    facet_grid(rate.codes ~ .) +
    geom_vline(
      data = dt_base.usage_reduced,
      aes(
        xintercept = x.intercept_tier2,
        color = season, linetype = season
      )
    ) +
    geom_ribbon(aes(fill = season), alpha = 0.3) +
    geom_point(
        data = dt_results_t.test_mean_by.code[
          range_kwh_total_normalize_period0 %in% ranges_for.plot
        ],
          aes(
              x = range_kwh_total_normalize_period0,
                y = estimate,
                color = season, shape = season
          )
    ) +
    geom_line(aes(color = season)) +
    scale_linetype_manual(values = c("dotted", "dashed")) +
    scale_shape_manual(values = c(15, 16)) +
    plot.options


# --------------------------------------------------
# Make Plots: Scatter Plots using Abs. Values of Consumption
# --------------------------------------------------
# ------- Create objects that will be used to make plots -------
# # 1. Add data field(s)
# # 1.1. Add a column showing total consumption during period 0
dt_for.reg[, kwh_total_period0 := kwh_daily.avg_period0 * period_len_period0]
# # 1.2.
bws <- seq(10, 50, by = 10)
seasons <- c("Summer", "Winter")
for (bw in bws) {
  for (season in seasons) {
    dt_for.reg[
      eval(
        parse(
          text = generator_subset.condition_for.subsample(
            bw = bw, rate.codes = c("RSGH"), season = season,
            year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
            suffix = "period0"
          )
        )
      ),
      paste0("is_sub.sample_g.code_", tolower(season), "_bw.", bw) := TRUE
    ]
  }
}
for (bw in bws) {
  for (season in seasons) {
    dt_for.reg[
      eval(
        parse(
          text = generator_subset.condition_for.subsample(
            bw = bw, rate.codes = c("RSCH", "RSEH"), season = season,
            year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
            suffix = "period0"
          )
        )
      ),
      paste0("is_sub.sample_ce.codes_", tolower(season), "_bw.", bw) := TRUE
    ]
  }
}


# # 2. Generate a DT indicating the limit value of each treatment status
dt_limits_by.treatment.status <- data.table(
  is_treated_period0 = c(TRUE, FALSE),
  limit = c(
    dt_for.reg[is_treated_period0 == TRUE]$kwh_total_period0 %>%
      min(., na.rm = TRUE),
    dt_for.reg[is_treated_period0 == FALSE]$kwh_total_period0 %>%
      max(., na.rm = TRUE)
  )
)

# # 3.
# # 3.1.
bws_in.list <- as.list(bws)
names(bws_in.list) <- paste0(bws, "%")
# # 3.2.
rate.codes_in.list <- list(
  c("RSGH"), c("RSGH"), c("RSCH", "RSEH"), c("RSCH", "RSEH")
)
# # 3.3.
dt_deciles_by.bw <-
  mapply(
    FUN = extract_decile.info_by.bw,
    rate.codes = rate.codes_in.list,
    season = as.list(rep(c("Summer", "Winter"), times = 2)),
    MoreArgs = list(
      bws_in.list = bws_in.list,
      year_lower = YEAR_LOWER, year_upper = YEAR_UPPER,
      suffix = "period0"
    ),
    SIMPLIFY = FALSE
  ) %>%
    rbindlist(.)
dt_deciles_by.bw[, season_before_period0 := season]
dt_deciles_by.bw[, decile := factor(decile, levels = paste0(seq(90, 10, by = -10), "%"), ordered = TRUE)]


# ------- Set plot options -------
plot.options_scatter_abs <-
  list(
    theme_linedraw(),
    labs(
      x = "Monthly Consumption in Period 0 (kWh/Month)",
      y = "Daily Average Consumption in Period 1 (kWh/Day)",
      color = "Deciles"
    ),
    theme(
      strip.text = element_text(face = "bold")
    )
  )


# ------- Create ggplot objects -------
# # 1. By using the full sample
plot_scatter_abs <-
  ggplot(dt_for.reg) +
    geom_point(
        aes(x = kwh_total_period0, y = kwh_daily.avg),
        alpha = 0.1
    ) +
    geom_vline(
      data = dt_limits_by.treatment.status,
      aes(xintercept = limit),
      linetype = "dashed", color = "black", alpha = 0.7
    ) +
    facet_grid(is_treated_period0 ~ .) +
    scale_x_continuous(
      labels = scales::comma,
      limits = c(0, MONTHLY.QTY_UPPER),
      breaks = seq(0, MONTHLY.QTY_UPPER, by = 1000)
    ) +
    scale_y_continuous(labels = scales::comma, limits = c(0, DAILY.QTY_UPPER)) +
    plot.options_scatter_abs

# # 2.
for (tmp_bw in bws) {
  for (code in c("RSGH", "RSCH")) {
    if (code %in% c("RSCH", "RSEH")) {
      tmp_str <- "is_sub.sample_ce.codes_"
      tmp_code_short <- code
      tmp_code_long <- "RSCH & RSEH"
    } else {
      tmp_str <- "is_sub.sample_g.code_"
      tmp_code_short <- code
      tmp_code_long <- code
    }
    tmp_file.name <- paste0(
      "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis_",
      paste0(str_replace(tmp_code_long, " & ", "-and-")),
      paste0("_BW-", tmp_bw),
      ".png"
    )

    tmp_plot <-
      ggplot(
        dt_for.reg[
          eval(
            parse(
              text = paste(
                paste0(tmp_str, tolower("Summer"), "_bw.", tmp_bw, " == TRUE"),
                paste0(tmp_str, tolower("Winter"), "_bw.", tmp_bw, " == TRUE"),
                sep = " | "
              )
            )
          )
        ]
      ) +
        geom_point(aes(x = kwh_total_period0, y = kwh_daily.avg), alpha = 0.1) +
        geom_vline(data = dt_base.usage[rate_code_normalize == tmp_code_short], aes(xintercept = tier_1_qty_upto_in_kwh), color = "black", linetype = "dashed", alpha = 0.5) +
        geom_vline(data = dt_base.usage[rate_code_normalize == tmp_code_short], aes(xintercept = tier_2_qty_upto_in_kwh), color = "black", linetype = "dashed", alpha = 0.5) +
        geom_hline(
          data = dt_deciles_by.bw[bw == paste0(tmp_bw, "%") & rate.codes == tmp_code_long],
          aes(yintercept = value, color = decile),
          linetype = "dotdash"
        ) +
        facet_grid(is_treated_period0 ~ season_before_period0) +
        scale_y_continuous(breaks = seq(0, 160, by = 20), limits = c(0, 130)) +
        scale_color_brewer(palette = "Spectral") +
        plot.options_scatter_abs

    if (code %in% c("RSCH", "RSEH")) {
      tmp_plot_scaled <-
        tmp_plot +
          scale_x_continuous(
            breaks = seq(0, 2000, by = 200),
            limits = c(300, 1700),
            labels = scales::comma
          )
    } else {
      tmp_plot_scaled <-
        tmp_plot +
          scale_x_continuous(
            breaks = seq(0, 2000, by = 100),
            limits = c(300, 1050),
            labels = scales::comma
          )
    }

    plot.save(
      paste(DIR_TO.SAVE_PLOTS, tmp_file.name, sep = "/"),
      tmp_plot_scaled,
      width = 25, height = 13, units = "cm"
    )
  }
}




# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------
plot.options_scatter <-
  list(
    theme_linedraw(),
    labs(
      x = "Normalized Consumption in Period 0 relative to Base Usage Qty",
      y = "Daily Average Consumption in Period 1 (kWh/Day)",
      color = "Treatment"
    )
  )

grob_scatter_base <-
  ggplot(dt_for.reg) +
    geom_point(
      aes(x = kwh_total_in.percent_normalize_period0 / 100, y = kwh_daily.avg),
      alpha = 0.1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    scale_x_continuous(
      labels = scales::percent,
      limits = c(-1, NORMALIZED.MONTHLY.QTY_UPPER / 100),
      breaks = seq(-1, NORMALIZED.MONTHLY.QTY_UPPER / 100, by = 1)
    ) +
    scale_y_continuous(labels = scales::comma) +
    plot.options_scatter


plot_scatter_d1 <-
  grob_scatter_base +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 1)
    ) +
    labs(subtitle = "Spline Fits, Degree 1")

plot_scatter_d2 <-
  grob_scatter_base +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 2)
    ) +
    labs(subtitle = "Spline Fits, Degree 2")

plot_scatter_d3 <-
  grob_scatter_base +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 3)
    ) +
    labs(subtitle = "Spline Fits, Degree 3")





# -------  -------
# ## Note:
# ## In this script, "outlier" means observations with exceptionally large
# ## value for `kwh_daily.avg`.
grob_scatter_base_excl.outliers <-
  ggplot(dt_for.reg) +
    geom_point(
      aes(x = kwh_total_in.percent_normalize_period0 / 100, y = kwh_daily.avg),
      alpha = 0.1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    scale_x_continuous(
      labels = scales::percent,
      limits = c(-1, NORMALIZED.MONTHLY.QTY_UPPER / 100)
    ) +
    scale_y_continuous(
      labels = scales::comma, limits = c(0, DAILY.QTY_UPPER)
    ) +
    plot.options_scatter


plot_scatter_d1_excl.outliers <-
  grob_scatter_base_excl.outliers +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 1)
    ) +
    labs(subtitle = "Spline Fits, Degree 1")

plot_scatter_d2_excl.outliers <-
  grob_scatter_base_excl.outliers +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 2)
    ) +
    labs(subtitle = "Spline Fits, Degree 2")

plot_scatter_d3_excl.outliers <-
  grob_scatter_base_excl.outliers +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated
      ),
      method = lm, formula = y ~ splines::bs(x, degree = 3)
    ) +
    labs(subtitle = "Spline Fits, Degree 3")







# --------------------------------------------------
# Make Plots:
# --------------------------------------------------
# -------  -------
plot.options_scatter_relation <-
  list(
    theme_linedraw(),
    scale_x_continuous(
      labels = scales::percent,
      limits = c(-1, NORMALIZED.MONTHLY.QTY_UPPER / 100),
      breaks = seq(-1, NORMALIZED.MONTHLY.QTY_UPPER / 100, by = 1)
    ),
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, MONTHLY.QTY_UPPER),
      breaks = seq(0, MONTHLY.QTY_UPPER, by = 1000)
    ),
    labs(
      x = "Normalized Consumption in Period 0 relative to Base Usage Qty (%)",
      y = "Monthly Consumption in Period 0 (kWh/Month)",
      color = "Treatment"
    )
  )


# -------  -------
plot_scatter_relation <-
  ggplot(dt_for.reg) +
    geom_point(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_total_period0,
        color = is_treated
      ),
      alpha = 0.1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    plot.options_scatter_relation




# --------------------------------------------------
# Export Plots created in PNG format
# --------------------------------------------------
# ------- Export plots showing the mean by range -------
# # 1. For plot created by using the full sample
PLOT.NAME_MEAN <- "SMUD-Billing-Data_RD-Design_Mean-by-Range.png"
PATH_PLOT_MEAN <- paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_MEAN, sep = "/")
plot.save(
  PATH_PLOT_MEAN,
  plot_mean,
  width = 25, height = 15, units = "cm"
)

# # 2. For plot created based on rate codes and seasons
# # 2.1. Plot being facetted by season
PLOT.NAME_MEAN_BY.CODE_FACET.BY.SEASON <-
  "SMUD-Billing-Data_RD-Design_Mean-by-Range_By-Rate-Code-and-Season_Facet-by-Season.png"
PATH_PLOT_MEAN_BY.CODE_FACET.BY.SEASON <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_MEAN_BY.CODE_FACET.BY.SEASON, sep = "/")
plot.save(
  PATH_PLOT_MEAN_BY.CODE_FACET.BY.SEASON,
  plot_mean_by.code_facet.y.season,
  width = 25, height = 20, units = "cm"
)

# # 2.2. Plot being facetted by rate code
PLOT.NAME_MEAN_BY.CODE_FACET.BY.CODE <-
  "SMUD-Billing-Data_RD-Design_Mean-by-Range_By-Rate-Code-and-Season_Facet-by-Code.png"
PATH_PLOT_MEAN_BY.CODE_FACET.BY.CODE <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_MEAN_BY.CODE_FACET.BY.CODE, sep = "/")
plot.save(
  PATH_PLOT_MEAN_BY.CODE_FACET.BY.CODE,
  plot_mean_by.code_facet.y.code,
  width = 25, height = 20, units = "cm"
)


# ------- Export scatter plots using consumption in abs. values -------
# # 1.
PLOT.NAME_SCATTER_ABS <-
  "SMUD-Billing-Data_RD-Design_Scatter_Absolute-Consumption-in-H-Axis.png"
PATH_PLOT_SCATTER_ABS <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SCATTER_ABS, sep = "/")
plot.save(
  PATH_PLOT_SCATTER_ABS,
  plot_scatter_abs,
  width = 30, height = 25, units = "cm"
)

# # 2.



# -------  -------
PLOT.NAME_SCATTER <-
  "SMUD-Billing-Data_RD-Design_Scatter_Spline-Fittings.png"
PATH_PLOT_SCATTER <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SCATTER, sep = "/")
plot.save(
  PATH_PLOT_SCATTER,
  grid.arrange(plot_scatter_d1, plot_scatter_d2, plot_scatter_d3, ncol = 1),
  width = 35, height = 55, units = "cm"
)
# ## Note:
# ## Saving this plot takes about more than 1 hour.


PLOT.NAME_SCATTER_EXCL.OUTLIERS <-
  "SMUD-Billing-Data_RD-Design_Scatter_Spline-Fittings_Excluding-Outliers.png"
PATH_PLOT_SCATTER_EXCL.OUTLIERS <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SCATTER_EXCL.OUTLIERS, sep = "/")
plot.save(
  PATH_PLOT_SCATTER_EXCL.OUTLIERS,
  grid.arrange(
    plot_scatter_d1_excl.outliers,
    plot_scatter_d2_excl.outliers,
    plot_scatter_d3_excl.outliers,
    ncol = 1
  ),
  width = 35, height = 55, units = "cm"
)





# -------  -------
PLOT.NAME_SCATTER_RELATIONSHIP <-
  "SMUD-Billing-Data_RD-Design_Scatter_Relationship.png"
PATH_PLOT_SCATTER_RELATIONSHIP <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_SCATTER_RELATIONSHIP, sep = "/")
plot.save(
  PATH_PLOT_SCATTER_RELATIONSHIP,
  plot_scatter_relation,
  width = 30, height = 20, units = "cm"
)

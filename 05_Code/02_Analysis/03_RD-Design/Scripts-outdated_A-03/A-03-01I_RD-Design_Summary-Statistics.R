# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02I
# #
# > Purpose of the script(s)
# # : To generate tables and plots that show summary statistics.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(stringr)
library(zoo)
library(ggplot2)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste(
  "/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep= "/"
)
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data for running Regressions
DIR_TO.LOAD_RD <- "01_RD-Design"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Design.parquet"
PATH_TO.LOAD_RD <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD,
  sep= "/"
)
# # 1.2. SMUD Residential Rates
PATH_TO.LOAD_RRS <- paste(
  PATH_DATA_INTERMEDIATE, "SMUD", "Residential-Rate-Schedules",
  "SMUD_Residential-Rate-Schedules_Panel.parquet",
  sep = "/"
)

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Table(s) will be saved
DIR_TO.SAVE_RD <- "02_RD-Design"
DIR_TO.SAVE_RD_TABLES <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, DIR_TO.SAVE_RD,
  "01_Tables/Raw-Tables",
  sep = "/"
)
# # 2.2. Path at which Plot(s) will be saved
DIR_TO.SAVE_RD_PLOTS <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, DIR_TO.SAVE_RD,
  "02_Plots",
  sep = "/"
)


# ------- Define parameter(s) -------
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005


# ------- Define function(s) -------
# # 1. Create a DT condaining Summary Statistics
get_dt_summary.stats <- function(condition, month, bw, rate.codes, season) {
  tmp_dt <-
    dt_for.reg[
      eval(parse(text = condition))
    ][
      billing.month_mid_period0 == month
    ]

  if (tmp_dt[, .N] == 0) {
    tmp_results <-
      data.table(
        statistics = c("Mean", "Std. Dev.", "Min.", "Median", "Max."),
        kwh_daily.avg = rep(NA, times = 5),
        kwh_total_in.percent_normalize_period0 = rep(NA, times = 5),
        kwh_total_period0 = rep(NA, times = 5)
      )
  } else {
    tmp_results <-
      data.table(
        statistics = c("Mean", "Std. Dev.", "Min.", "Median", "Max."),
        kwh_daily.avg = c(
          mean(tmp_dt$kwh_daily.avg, na.rm = TRUE),
          sd(tmp_dt$kwh_daily.avg, na.rm = TRUE),
          min(tmp_dt$kwh_daily.avg, na.rm = TRUE),
          median(tmp_dt$kwh_daily.avg, na.rm = TRUE),
          max(tmp_dt$kwh_daily.avg, na.rm = TRUE)
        ),
        kwh_total_in.percent_normalize_period0 = c(
          mean(tmp_dt$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
          sd(tmp_dt$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
          min(tmp_dt$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
          median(tmp_dt$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
          max(tmp_dt$kwh_total_in.percent_normalize_period0, na.rm = TRUE)
        ),
        kwh_total_period0 = c(
          mean(tmp_dt$kwh_total_period0, na.rm = TRUE),
          sd(tmp_dt$kwh_total_period0, na.rm = TRUE),
          min(tmp_dt$kwh_total_period0, na.rm = TRUE),
          median(tmp_dt$kwh_total_period0, na.rm = TRUE),
          max(tmp_dt$kwh_total_period0, na.rm = TRUE)
        )
      )
  }
  tmp_results[
    ,
    `:=` (
      bw = bw,
      rate.codes = str_c(rate.codes, collapse = " & "),
      season = season,
      month = month
    )
  ]

  return(tmp_results)
}

# # 2. Generate a DT containing Summary Statistics for a Month
extract_summary.stats_for.a.month <- function(
  conditions_in.list, inputs_from.expand.grid, month
) {
  return(
    mapply(
      FUN = get_dt_summary.stats,
      condition = conditions_in.list,
      bw = as.list(inputs_from.expand.grid[, "bw"]),
      rate.codes = as.list(inputs_from.expand.grid[, "rate.codes"]),
      season = as.list(inputs_from.expand.grid[, "season"]),
      MoreArgs = list(
        month = month
      ),
      SIMPLIFY = FALSE
    ) %>% rbindlist(.)
  )
}




####

#count_by.percentile <- function(percentile_in.str, col) {
#  tmp_number <- str_extract(percentile_in.str, "[:digit:]+") %>% as.numeric(.)
#  return(
#    dt_for.reg[
#      get(col) <= quantile(dt_for.reg[, get(col)], probs = tmp_number / 100),
#      .N
#    ]
#  )
#}
#
#codes <- c("RSCH", "RSEH")
#condition <- paste0("rate_code_normalize_period0 %in% c(\"", str_c(codes, collapse = "\", \""), "\")")
#
#count_by.percentile_for.subsamples <-
#  function(
#    percentile_in.str, col,
#    rate.codes_in.vector, year_lower, year_upper, season_in.str
#  ) {
#    tmp_number <-
#      str_extract(percentile_in.str, "[:digit:]+") %>% as.numeric(.)
#    tmp_condition <- paste(
#      paste0(
#        "rate_code_normalize_period0 %in% c(\"",
#        str_c(rate.codes_in.vector, collapse = "\", \""),
#        "\")"
#      ),
#      paste0(
#        "billing.year_mid_period0 %in% c(",
#        year_lower, ":", year_upper,
#        ")"
#      ),
#      paste0("season_before_period0 == ", season_in.str),
#      "season_before_period0 == season_after_period0",
#      sep = " & "
#    )
#
#    return(
#      dt_for.reg[
#        eval(parse(text = tmp_condition))
#      ][
#        get(col) <=
#          quantile(
#            dt_for.reg[eval(parse(text = tmp_condition))][, get(col)],
#            probs = tmp_number / 100
#          ),
#        .N
#      ]
#    )
#  }
#
#
#
#summary_consumption <-
#  function(rate.codes_in.vector, year_lower, year_upper, season_in.str) {
#    tmp_condition <- paste(
#      paste0(
#        "rate_code_normalize_period0 %in% c(\"",
#        str_c(rate.codes_in.vector, collapse = "\", \""),
#        "\")"
#      ),
#      paste0(
#        "billing.year_mid_period0 %in% c(",
#        year_lower, ":", year_upper,
#        ")"
#      ),
#      paste0("season_before_period0 == ", season_in.str),
#      "season_before_period0 == season_after_period0",
#      sep = " & "
#    )
#
#    dt_summary <- data.table(
#      statistics = c("Mean", "Std. Dev.", "Min.", "Median", "Max."),
#      kwh_daily.avg = c(
#        mean(
#          dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#          na.rm = TRUE
#        ),
#        sd(
#          dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#          na.rm = TRUE
#        ),
#        min(
#          dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#          na.rm = TRUE
#        ),
#        median(
#          dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#          na.rm = TRUE
#        ),
#        max(
#          dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#          na.rm = TRUE
#        )
#      ),
#      kwh_total_in.percent_normalize_period0 = c(
#        mean(
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ]$kwh_total_in.percent_normalize_period0,
#          na.rm = TRUE
#        ),
#        sd(
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ]$kwh_total_in.percent_normalize_period0,
#          na.rm = TRUE
#        ),
#        min(
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ]$kwh_total_in.percent_normalize_period0,
#          na.rm = TRUE
#        ),
#        median(
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ]$kwh_total_in.percent_normalize_period0,
#          na.rm = TRUE
#        ),
#        max(
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ]$kwh_total_in.percent_normalize_period0,
#          na.rm = TRUE
#        )
#      )
#    )
#    dt_summary[
#      ,
#      `:=` (
#        rate_code_normalize = str_c(rate.codes_in.vector, collapse = ", "),
#        season = season_in.str
#      )
#    ]
#
#    setcolorder(dt_summary, c(1, 4, 5))
#    return(dt_summary)
#}
#
#
#
#
#distribution_by.rate.code <-
#  function(rate.codes_in.vector, year_lower, year_upper, season_in.str) {
#    tmp_condition <- paste(
#      paste0(
#        "rate_code_normalize_period0 %in% c(\"",
#        str_c(rate.codes_in.vector, collapse = "\", \""),
#        "\")"
#      ),
#      paste0(
#        "billing.year_mid_period0 %in% c(",
#        year_lower, ":", year_upper,
#        ")"
#      ),
#      paste0("season_before_period0 == ", season_in.str),
#      "season_before_period0 == season_after_period0",
#      sep = " & "
#    )
#
#    tmp_dt_dist_daily <- as.data.table(
#      quantile(
#        dt_for.reg[eval(parse(text = tmp_condition))]$kwh_daily.avg,
#        probs = seq(0, 1, by = 0.01)
#      ),
#      keep.rownames = TRUE
#    )
#    names(tmp_dt_dist_daily) <- c("statistic", "kwh_daily_period1")
#    tmp_dt_dist_daily[
#      ,
#      count_daily :=
#        lapply(
#          statistic, count_by.percentile_for.subsamples,
#          col = "kwh_daily.avg",
#          rate.codes_in.vector = rate.codes_in.vector,
#          year_lower = year_lower,
#          year_upper = year_upper,
#          season_in.str = season_in.str
#        ) %>%
#          transpose(.)
#    ]
#
#    tmp_dt_dist_normalized.monthly <- as.data.table(
#      quantile(
#        dt_for.reg[
#          eval(parse(text = tmp_condition))
#        ]$kwh_total_in.percent_normalize_period0,
#        probs = seq(0, 1, by = 0.01)
#      ),
#      keep.rownames = TRUE
#    )
#    names(tmp_dt_dist_normalized.monthly) <- c(
#      "statistic", "normalized.monthly_period1"
#    )
#    tmp_dt_dist_normalized.monthly[
#      ,
#      count_monthly :=
#        lapply(
#          statistic, count_by.percentile_for.subsamples,
#          col = "kwh_total_in.percent_normalize_period0",
#          rate.codes_in.vector = rate.codes_in.vector,
#          year_lower = year_lower,
#          year_upper = year_upper,
#          season_in.str = season_in.str
#        ) %>%
#          transpose(.)
#    ]
#
#    tmp_dt_dist <-
#      tmp_dt_dist_daily[tmp_dt_dist_normalized.monthly, on = .(statistic)]
#    tmp_dt_dist[
#      ,
#      `:=` (
#        rate.codes = str_c(rate.codes_in.vector, collapse = ", "),
#        season = season_in.str
#      )
#    ]
#    return(tmp_dt_dist)
#  }
#
#
#distribution_by.bw.and.rate.code <-
#  function(rate.codes_in.vector, year_lower, year_upper, season_in.str) {
#    tmp_condition <- paste(
#      paste0(
#        "rate_code_normalize_period0 %in% c(\"",
#        str_c(rate.codes_in.vector, collapse = "\", \""),
#        "\")"
#      ),
#      paste0(
#        "billing.year_mid_period0 %in% c(",
#        year_lower, ":", year_upper,
#        ")"
#      ),
#      paste0("season_before_period0 == ", season_in.str),
#      "season_before_period0 == season_after_period0",
#      sep = " & "
#    )
#
#    dt_obs_subsample_by.bw <- setDT(NULL)
#    cols <- names(dt_for.reg)[str_detect(names(dt_for.reg), "^is_balanced.ids")]
#    cols_by_ids <- c("id_account", "id_premise")
#    cols_by_bym <- c("billing.ym_mid")
#    for (col in cols) {
#      tmp_bw <-
#        str_extract(col, "bw_.+?$") %>% str_replace(., "bw_", "") %>%
#          (function(x) if (x == "na") {x} else {as.numeric(x)})
#
#      if (tmp_bw == "na") {
#        tmp_hh <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][, .N, by = cols_by_ids][, .N]
#        tmp_bym <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][, .N, by = cols_by_bym][, .N]
#        tmp_treatment <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (0 < kwh_total_in.percent_normalize_period0), .N
#          ]
#        tmp_control <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (kwh_total_in.percent_normalize_period0 <= 0), .N
#          ]
#      } else {
#        tmp_hh <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#              kwh_total_in.percent_normalize_period0 <= tmp_bw),
#            .N,
#            by = cols_by_ids
#          ][, .N]
#        tmp_bym <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#              kwh_total_in.percent_normalize_period0 <= tmp_bw),
#            .N,
#            by = cols_by_bym
#          ][, .N]
#        tmp_treatment <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (0 < kwh_total_in.percent_normalize_period0 &
#              kwh_total_in.percent_normalize_period0 <= tmp_bw),
#            .N
#          ]
#        tmp_control <-
#          dt_for.reg[
#            eval(parse(text = tmp_condition))
#          ][
#            (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#              kwh_total_in.percent_normalize_period0 <= 0),
#            .N
#          ]
#      }
#      tmp_dt <- data.table(
#        bw = paste0(tmp_bw, "%"),
#        hh = tmp_hh,
#        bym = tmp_bym,
#        control = tmp_control,
#        treatment = tmp_treatment
#      )
#      tmp_dt[
#        ,
#        `:=` (
#          rate.codes = str_c(rate.codes_in.vector, collapse = ", "),
#          season = season_in.str
#        )
#      ]
#      dt_obs_subsample_by.bw <- rbind(dt_obs_subsample_by.bw, tmp_dt)
#    }
#
#    dt_obs_subsample_by.bw <-
#      rbind(dt_obs_subsample_by.bw, dt_obs_subsample_by.bw[1])
#    dt_obs_subsample_by.bw <- dt_obs_subsample_by.bw[-1]
#    dt_obs_subsample_by.bw[, total := control + treatment]
#
#    return(dt_obs_subsample_by.bw)
#  }



# --------------------------------------------------
# Load Data Sets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <- pq.to.dt(
  PATH_TO.LOAD_RD,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)
gc(reset = TRUE, full = TRUE)

# # 2. Check primary keys of the DT
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)

# # 3. Add data fields
dt_for.reg[, kwh_total_period0 := kwh_daily.avg_period0 * period_len_period0]


# ------- Load SMUD Residential Rate Data -------
# # 1. Load SMUD Residential Rate Data
dt_rrs <- pq.to.dt(
  PATH_TO.LOAD_RRS,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)

# # 2. Check primary keys of the DT
stopifnot(dt_rrs[, .N, by = .(date, rate_code_normalize)][N > 1, .N] == 0)


# --------------------------------------------------
# Create Consumption Summary Tables & Plots
# --------------------------------------------------
# ------- Consumption Summary: For the Unrestricted Sample -------
# # 1. Generate a DT
dt_consumption_summary <-
  data.table(
    statistics = c("Mean", "Std. Dev.", "Min.", "Median", "Max."),
    kwh_daily.avg = c(
      mean(dt_for.reg$kwh_daily.avg, na.rm = TRUE),
      sd(dt_for.reg$kwh_daily.avg, na.rm = TRUE),
      min(dt_for.reg$kwh_daily.avg, na.rm = TRUE),
      median(dt_for.reg$kwh_daily.avg, na.rm = TRUE),
      max(dt_for.reg$kwh_daily.avg, na.rm = TRUE)
    ),
    kwh_total_in.percent_normalize_period0 = c(
      mean(dt_for.reg$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
      sd(dt_for.reg$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
      min(dt_for.reg$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
      median(dt_for.reg$kwh_total_in.percent_normalize_period0, na.rm = TRUE),
      max(dt_for.reg$kwh_total_in.percent_normalize_period0, na.rm = TRUE)
    ),
    kwh_total_period0 = c(
      mean(dt_for.reg$kwh_total_period0, na.rm = TRUE),
      sd(dt_for.reg$kwh_total_period0, na.rm = TRUE),
      min(dt_for.reg$kwh_total_period0, na.rm = TRUE),
      median(dt_for.reg$kwh_total_period0, na.rm = TRUE),
      max(dt_for.reg$kwh_total_period0, na.rm = TRUE)
    )
  )


# # 2. Create a Huxtable
# # 2.1. Convert the DT to a huxtable
hux.table_consumption_summary <- as_hux(dt_consumption_summary)

# # 2.2. Modify the huxtable
# # 2.2.1. Change number format
number_format(hux.table_consumption_summary)[-1, -1] <- list(
  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
  )
# # 2.2.2. Change column names
contents(hux.table_consumption_summary)[1, ] <-
  c(
    "Statistic", "Daily Average Consumption",
    "Normalized Monthly Consumption", "Actual Monthly Consumption"
  )
# # 2.2.3. Add a row including measurement units
hux.table_consumption_summary <-
  insert_row(
    hux.table_consumption_summary,
    c("", "(kWh/Day)", "(%)", "(kWh/Month)"),
    after = 1
  )
# # 2.2.4. Align values
align(hux.table_consumption_summary)[, 1] <- "center"
align(hux.table_consumption_summary)[1:2, ] <- "center"
# # 2.2.5. Add lines
right_border(hux.table_consumption_summary)[, 1:3] <- brdr(0.4, "solid")
top_border(hux.table_consumption_summary)[1, ] <- brdr(0.4, "double")
bottom_border(hux.table_consumption_summary)[2, ] <- brdr(0.4, "solid")
hux.table_consumption_summary <-
  set_bottom_border(
    hux.table_consumption_summary, final(1), , brdr(0.4, "double")
  )


# --------------------------------------------------
# Create By-Consumption-Level Consumption Summary Tables & Plots
# --------------------------------------------------
# ------- Consumption Summary by Decile and Month -------
cols_by <- c("billing.month_mid")

# # 1. Create DTs by Rate Code
# # 1.1. For RSGH
tmp_dt_g.code_mean <- dt_for.reg[
  rate_code_normalize %in% c("RSGH"),
  lapply(.SD, mean, na.rm = TRUE),
  .SDcols = c("kwh_total", "kwh_daily.avg"),
  keyby = cols_by
][, category := "Mean"]
tmp_dt_g.code_decile <- dt_for.reg[
  rate_code_normalize %in% c("RSGH"),
  lapply(.SD, quantile, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE),
  .SDcols = c("kwh_total", "kwh_daily.avg"),
  keyby = cols_by
][, category := paste0(order(kwh_total) * 10, "%"), by = .(billing.month_mid)]

# # 1.2. For RSCH and RSEH
tmp_dt_ce.codes_mean <- dt_for.reg[
  rate_code_normalize %in% c("RSCH", "RSEH"),
  lapply(.SD, mean, na.rm = TRUE),
  .SDcols = c("kwh_total", "kwh_daily.avg"),
  keyby = cols_by
][, category := "Mean"]
tmp_dt_ce.codes_decile <- dt_for.reg[
  rate_code_normalize %in% c("RSCH", "RSEH"),
  lapply(.SD, quantile, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE),
  .SDcols = c("kwh_total", "kwh_daily.avg"),
  keyby = cols_by
][, category := paste0(order(kwh_total) * 10, "%"), by = .(billing.month_mid)]


# # 2. Combine DTs created
# # 2.1. Add a data field, and then combine DTs
dt_consumption_by.month <- rbind(
  tmp_dt_g.code_mean[, rate.codes := "RSGH"],
  tmp_dt_g.code_decile[, rate.codes := "RSGH"],
  tmp_dt_ce.codes_mean[, rate.codes := "RSCH & RSEH"],
  tmp_dt_ce.codes_decile[, rate.codes := "RSCH & RSEH"]
)

# # 2.2. Convert data type to factor
dt_consumption_by.month[
  ,
  category_in.factor := factor(
    category, levels = c(paste0(seq(90, 10, by = -10), "%"), "Mean")
  )
]
dt_consumption_by.month[
  ,
  billing.month_mid := factor(
    billing.month_mid, levels = c(1:12), labels = month.abb
  )
]


# # 3. Make Plot(s)
# # 3.1. Define common plot options
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <- list(
  theme_linedraw(),
  facet_grid(rate.codes ~ .),
  scale_y_continuous(labels = scales::comma),
  scale_color_viridis_d(direction = -1),
  labs(
    x = "",
    y = "kWh per Month",
    color = "Deciles"
  ),
  theme(strip.text = element_text(face = "bold"))
)

# # 3.2. Make ggplot object(s)
plot_consumption_by.month <-
  ggplot(
    dt_consumption_by.month[category != "Mean"],
    aes(
      x = billing.month_mid, y = kwh_total,
      color = category_in.factor, group = category
    )
  ) +
    geom_line(alpha = 0.6) +
    geom_line(
      data = dt_consumption_by.month[category == "Mean"],
      aes(x = billing.month_mid, y = kwh_total, group = category),
      color = color.palette[1]
    ) +
    geom_point(
      data = dt_consumption_by.month[category == "Mean"],
      aes(x = billing.month_mid, y = kwh_total),
      color = color.palette[1]
    ) +
    plot.options

# # 3.3. Export plot(s) in PNG format
plot.save(
  paste(
    DIR_TO.SAVE_RD_PLOTS,
    "SMUD-Billing-Data_RD-Design_Consumption-by-Month.png",
    sep = "/"
  ),
  plot_consumption_by.month,
  width = 25, height = 25, units = "cm"
)


# --------------------------------------------------
# Create By-Bandwidth Consumption Summary Tables and Plots
# --------------------------------------------------
# ------- Consumption Summary by Rate Code and Month -------
# # 1. Generate a DT containing summary statistics
# # 1.1. Create objects that will be used to make a DT
list_bws <- as.list(seq(10, 50, by = 10))
list_rate.codes <- list(c("RSCH", "RSEH"), c("RSGH"))
list_seasons <- list("Summer", "Winter")
list_inputs <- expand.grid(list_bws, list_rate.codes, list_seasons)
names(list_inputs) <- c("bw", "rate.codes", "season")
list_conditions <- mapply(
  FUN = generator_subset.condition_for.subsample,
  bw = list_inputs$bw,
  rate.codes = list_inputs$rate.codes,
  season = list_inputs$season,
  MoreArgs = list(
    year_lower = YEAR_LOWER, year_upper = YEAR_UPPER, suffix = "period0"
  ),
  SIMPLIFY = FALSE
)

# # 1.2. Generate a DT
dt_consumption_summary_by.month <-
  lapply(
    X = c(1:12), FUN = extract_summary.stats_for.a.month,
    conditions_in.list = list_conditions,
    inputs_from.expand.grid = list_inputs
  ) %>% rbindlist(.)
dt_consumption_summary_by.month[, bw := paste0(bw, "%")]
setcolorder(
  dt_consumption_summary_by.month,
  c("rate.codes", "bw", "month", "season", "statistics")
)
setkeyv(dt_consumption_summary_by.month, c("rate.codes", "bw", "month"))


# # 2. Create a Huxtable
# # 2.1. Convert the DT to a huxtable
hux.table_consumption_summary_by.month <-
  as_hux(dt_consumption_summary_by.month[!is.na(kwh_daily.avg)])

# # 2.2. Modify the huxtable
# # 2.2.1. Change number format
number_format(hux.table_consumption_summary_by.month)[-1, -1:-5] <- list(
  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
  )
# # 2.2.2. Change column names
contents(hux.table_consumption_summary_by.month)[1, ] <-
  c(
    "Rate Codes", "Bandwidth", "Month", "Season", "Statistic",
    "Daily Average Consumption", "Normalized Monthly Consumption",
    "Actual Monthly Consumption"
  )
# # 2.2.3. Add a row including measurement units
hux.table_consumption_summary_by.month <-
  insert_row(
    hux.table_consumption_summary_by.month,
    c(rep("", times = 5), "(kWh/Day)", "(%)", "(kWh/Month)"),
    after = 1
  )
# # 2.2.4. Align values
align(hux.table_consumption_summary_by.month)[, 1:5] <- "center"
align(hux.table_consumption_summary_by.month)[1:2, ] <- "center"
# # 2.2.5. Add lines
top_border(hux.table_consumption_summary_by.month)[1, ] <- brdr(0.4, "double")
bottom_border(hux.table_consumption_summary_by.month)[2, ] <- brdr(0.4, "solid")
hux.table_consumption_summary_by.month <-
  set_bottom_border(
    hux.table_consumption_summary_by.month, final(1), , brdr(0.4, "double")
  )


# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------










# -------  -------

#dt_dist_daily <- as.data.table(
#  quantile(dt_for.reg$kwh_daily.avg, probs = seq(0, 1, by = 0.01)),
#  keep.rownames = TRUE
#)
#names(dt_dist_daily) <- c("statistics", "kwh_daily_period1")
#dt_dist_daily[
#  ,
#  count_daily :=
#    lapply(statistics, count_by.percentile, col = "kwh_daily.avg") %>%
#      transpose(.)
#]
#
#dt_dist_normalized.monthly <- as.data.table(
#  quantile(
#    dt_for.reg$kwh_total_in.percent_normalize_period0,
#    probs = seq(0, 1, by = 0.01)
#  ),
#  keep.rownames = TRUE
#)
#names(dt_dist_normalized.monthly) <- c(
#  "statistics", "normalized.monthly_period1"
#)
#dt_dist_normalized.monthly[
#  ,
#  count_monthly :=
#    lapply(
#      statistics,
#      count_by.percentile,
#      col = "kwh_total_in.percent_normalize_period0"
#    ) %>%
#      transpose(.)
#]
#
#dt_consumption_dist <- dt_dist_daily[dt_dist_normalized.monthly, on = .(statistics)]
#
#
#
#hux.table_consumption_dist <- as_hux(dt_consumption_dist)
#
#number_format(hux.table_consumption_dist)[-1, c(2, 4)] <- list(
#  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
#  )
#number_format(hux.table_consumption_dist)[-1, c(3,5)] <- fmt_pretty()
#
#contents(hux.table_consumption_dist)[1, ] <- c("", "Value (kWh)", "N", "Value (%)", "N")
#
#hux.table_consumption_dist <- insert_row(hux.table_consumption_dist, c("Percentile", "Daily Average Consumption", "", "Normalized Monthly Consumption", ""), after = 0)
#colspan(hux.table_consumption_dist)[1, 2] <- 2
#colspan(hux.table_consumption_dist)[1, 4] <- 2
##hux.table_consumption_dist <- merge_cells(hux.table_consumption_dist, 1, 2:3)
##hux.table_consumption_dist <- merge_cells(hux.table_consumption_dist, 1, 4:5)
#
#col_width(hux.table_consumption_dist) <- c(1, 1.5, 4, 1.5, 4)
#
#align(hux.table_consumption_dist)[, 1] <- "center"
#align(hux.table_consumption_dist)[1:2, ] <- "center"
#
#right_border(hux.table_consumption_dist)[, c(1, 3)] <- brdr(0.4, "solid")
#top_border(hux.table_consumption_dist)[1, ] <- brdr(0.4, "double")
#bottom_border(hux.table_consumption_dist)[1, 2:5] <- brdr(0.4, "solid")
#bottom_border(hux.table_consumption_dist)[2, ] <- brdr(0.4, "solid")
#hux.table_consumption_dist <- set_bottom_border(hux.table_consumption_dist, final(1), , brdr(0.4, "double"))
#
#col_width(hux.table_consumption_dist) <- c(2, 3, 4, 3, 4)
#
#
#
#
#
#hux.table_consumption_dist_short <- as_hux(dt_consumption_dist[statistics %in% c("0%", "25%", "50%", "75%", "100%")])
#
#number_format(hux.table_consumption_dist_short)[-1, c(2, 4)] <- list(
#  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
#  )
#number_format(hux.table_consumption_dist_short)[-1, c(3,5)] <- fmt_pretty()
#
#contents(hux.table_consumption_dist_short)[1, ] <- c("", "Value (kWh)", "N", "Value (%)", "N")
#
#hux.table_consumption_dist_short <- insert_row(hux.table_consumption_dist_short, c("Percentile", "Daily Average Consumption", "", "Normalized Monthly Consumption", ""), after = 0)
#colspan(hux.table_consumption_dist_short)[1, 2] <- 2
#colspan(hux.table_consumption_dist_short)[1, 4] <- 2
##hux.table_consumption_dist_short <- merge_cells(hux.table_consumption_dist_short, 1, 2:3)
##hux.table_consumption_dist_short <- merge_cells(hux.table_consumption_dist_short, 1, 4:5)
#
#col_width(hux.table_consumption_dist_short) <- c(1, 1.5, 4, 1.5, 4)
#
#align(hux.table_consumption_dist_short)[, 1] <- "center"
#align(hux.table_consumption_dist_short)[1:2, ] <- "center"
#
#
#right_border(hux.table_consumption_dist_short)[, c(1, 3)] <- brdr(0.4, "solid")
#top_border(hux.table_consumption_dist_short)[1, ] <- brdr(0.4, "double")
#bottom_border(hux.table_consumption_dist_short)[1, 2:5] <- brdr(0.4, "solid")
#bottom_border(hux.table_consumption_dist_short)[2, ] <- brdr(0.4, "solid")
#hux.table_consumption_dist_short <- set_bottom_border(hux.table_consumption_dist_short, final(1), , brdr(0.4, "double"))
#
#
#
#
#
## -------  -------
#dt_obs_restricted_by.bw <- setDT(NULL)
#cols <- names(dt_for.reg)[str_detect(names(dt_for.reg), "^is_balanced.ids")]
#cols_by_ids <- c("id_account", "id_premise")
#cols_by_bym <- c("billing.ym_mid")
#for (col in cols) {
#  tmp_bw <-
#    str_extract(col, "bw_.+?$") %>% str_replace(., "bw_", "") %>%
#      (function(x) if (x == "na") {x} else {as.numeric(x)})
#  tmp_hh <- dt_for.reg[get(col) == TRUE, .N, by = cols_by_ids][, .N]
#  if (tmp_bw == "na") {
#    tmp_hh <- dt_for.reg[get(col) == TRUE, .N, by = cols_by_ids][, .N]
#    tmp_bym <- dt_for.reg[get(col) == TRUE, .N, by = cols_by_bym][, .N]
#    tmp_treatment <-
#      dt_for.reg[
#        get(col) == TRUE & (0 < kwh_total_in.percent_normalize_period0), .N
#      ]
#    tmp_control <-
#      dt_for.reg[
#        get(col) == TRUE & (kwh_total_in.percent_normalize_period0 <= 0), .N
#      ]
#  } else {
#    tmp_hh <-
#      dt_for.reg[
#        get(col) == TRUE &
#          (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#            kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N,
#        by = cols_by_ids
#      ][, .N]
#    tmp_bym <-
#      dt_for.reg[
#        get(col) == TRUE &
#          (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#            kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N,
#        by = cols_by_bym
#      ][, .N]
#    tmp_treatment <-
#      dt_for.reg[
#        get(col) == TRUE &
#          (0 < kwh_total_in.percent_normalize_period0 &
#            kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N
#      ]
#    tmp_control <-
#      dt_for.reg[
#        get(col) == TRUE &
#          (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#            kwh_total_in.percent_normalize_period0 <= 0),
#        .N
#      ]
#  }
#  tmp_dt <- data.table(
#    bw = paste0(tmp_bw, "%"),
#    hh = tmp_hh,
#    bym = tmp_bym,
#    control = tmp_control,
#    treatment = tmp_treatment
#  )
#  dt_obs_restricted_by.bw <- rbind(dt_obs_restricted_by.bw, tmp_dt)
#}
#
#dt_obs_restricted_by.bw <-
#  rbind(dt_obs_restricted_by.bw, dt_obs_restricted_by.bw[1])
#dt_obs_restricted_by.bw <- dt_obs_restricted_by.bw[-1]
#dt_obs_restricted_by.bw[, total := control + treatment]
#
#hux.table_obs_restricted_by.bw <- as_hux(dt_obs_restricted_by.bw)
#number_format(hux.table_obs_restricted_by.bw)[-1, ] <- fmt_pretty()
#
#contents(hux.table_obs_restricted_by.bw)[1, ] <-
#  c("", "", "Billing", "Control", "Treatment", "Total")
#contents(hux.table_obs_restricted_by.bw)[
#  dt_obs_restricted_by.bw[, .N] + 1, 1
#] <- "N/A"
#
#hux.table_obs_restricted_by.bw <-
#  insert_row(
#    hux.table_obs_restricted_by.bw,
#    c("Bandwidth", "Households", "Year-Month", "Observations", "", ""),
#    after = 0
#  )
#colspan(hux.table_obs_restricted_by.bw)[1, 4] <- 3
#
#align(hux.table_obs_restricted_by.bw)[, 1] <- "center"
#align(hux.table_obs_restricted_by.bw)[1:2, ] <- "center"
#
#right_border(hux.table_obs_restricted_by.bw)[, 1:3] <- brdr(0.4, "solid")
#right_border(hux.table_obs_restricted_by.bw)[-1, 4] <- brdr(0.4, "solid")
#right_border(hux.table_obs_restricted_by.bw)[-1, 5] <- brdr(0.4, "double")
#top_border(hux.table_obs_restricted_by.bw)[1, ] <- brdr(0.4, "double")
#bottom_border(hux.table_obs_restricted_by.bw)[2, ] <- brdr(0.4, "solid")
#bottom_border(hux.table_obs_restricted_by.bw)[1, 4:6] <- brdr(0.4, "solid")
#hux.table_obs_restricted_by.bw <-
#  set_bottom_border(
#    hux.table_obs_restricted_by.bw, final(1), , brdr(0.4, "double")
#  )
#
#col_width(hux.table_obs_restricted_by.bw) <- c(2, 3, 3, 3, 3, 3)
#
#
#
## -------  -------
#dt_obs_unrestricted_by.bw <- setDT(NULL)
#cols <- names(dt_for.reg)[str_detect(names(dt_for.reg), "^is_balanced.ids")]
#cols_by_ids <- c("id_account", "id_premise")
#cols_by_bym <- c("billing.ym_mid")
#for (col in cols) {
#  tmp_bw <-
#    str_extract(col, "bw_.+?$") %>% str_replace(., "bw_", "") %>%
#    (function(x) if (x == "na") {x} else {as.numeric(x)})
#
#  if (tmp_bw == "na") {
#    tmp_hh <- dt_for.reg[, .N, by = cols_by_ids][, .N]
#    tmp_bym <- dt_for.reg[, .N, by = cols_by_bym][, .N]
#    tmp_treatment <-
#      dt_for.reg[
#        (0 < kwh_total_in.percent_normalize_period0), .N
#      ]
#    tmp_control <-
#      dt_for.reg[
#        (kwh_total_in.percent_normalize_period0 <= 0), .N
#      ]
#  } else {
#    tmp_hh <-
#      dt_for.reg[
#        (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#          kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N,
#        by = cols_by_ids
#      ][, .N]
#    tmp_bym <-
#      dt_for.reg[
#        (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#          kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N,
#        by = cols_by_bym
#      ][, .N]
#    tmp_treatment <-
#      dt_for.reg[
#        (0 < kwh_total_in.percent_normalize_period0 &
#          kwh_total_in.percent_normalize_period0 <= tmp_bw),
#        .N
#      ]
#    tmp_control <-
#      dt_for.reg[
#        (tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#          kwh_total_in.percent_normalize_period0 <= 0),
#        .N
#      ]
#  }
#  tmp_dt <- data.table(
#    bw = paste0(tmp_bw, "%"),
#    hh = tmp_hh,
#    bym = tmp_bym,
#    control = tmp_control,
#    treatment = tmp_treatment
#  )
#  dt_obs_unrestricted_by.bw <- rbind(dt_obs_unrestricted_by.bw, tmp_dt)
#}
#
#dt_obs_unrestricted_by.bw <-
#  rbind(dt_obs_unrestricted_by.bw, dt_obs_unrestricted_by.bw[1])
#dt_obs_unrestricted_by.bw <- dt_obs_unrestricted_by.bw[-1]
#dt_obs_unrestricted_by.bw[, total := control + treatment]
#
#hux.table_obs_unrestricted_by.bw <- as_hux(dt_obs_unrestricted_by.bw)
#number_format(hux.table_obs_unrestricted_by.bw)[-1, ] <- fmt_pretty()
#
#contents(hux.table_obs_unrestricted_by.bw)[1, ] <-
#  c("", "", "Billing", "Control", "Treatment", "Total")
#contents(hux.table_obs_unrestricted_by.bw)[
#  dt_obs_unrestricted_by.bw[, .N] + 1, 1
#] <- "N/A"
#
#hux.table_obs_unrestricted_by.bw <-
#  insert_row(
#    hux.table_obs_unrestricted_by.bw,
#    c("Bandwidth", "Households", "Year-Month", "Observations", "", ""),
#    after = 0
#  )
#colspan(hux.table_obs_unrestricted_by.bw)[1, 4] <- 3
#
#align(hux.table_obs_unrestricted_by.bw)[, 1] <- "center"
#align(hux.table_obs_unrestricted_by.bw)[1:2, ] <- "center"
#
#right_border(hux.table_obs_unrestricted_by.bw)[, 1:3] <- brdr(0.4, "solid")
#right_border(hux.table_obs_unrestricted_by.bw)[-1, 4] <- brdr(0.4, "solid")
#right_border(hux.table_obs_unrestricted_by.bw)[-1, 5] <- brdr(0.4, "double")
#top_border(hux.table_obs_unrestricted_by.bw)[1, ] <- brdr(0.4, "double")
#bottom_border(hux.table_obs_unrestricted_by.bw)[2, ] <- brdr(0.4, "solid")
#bottom_border(hux.table_obs_unrestricted_by.bw)[1, 4:6] <- brdr(0.4, "solid")
#hux.table_obs_unrestricted_by.bw <-
#  set_bottom_border(
#    hux.table_obs_unrestricted_by.bw, final(1), , brdr(0.4, "double")
#  )
#
##col_width(hux.table_obs_unrestricted_by.bw) <- c(1, 2, 3, 3, 3)
#
#
## -------  -------
#
#dt_bym.dist_by.bw <- setDT(NULL)
#cols <- names(dt_for.reg)[str_detect(names(dt_for.reg), "^is_balanced.ids")]
#for (col in cols) {
#  tmp_bw <-
#    str_extract(col, "bw_.+?$") %>% str_replace(., "bw_", "") %>%
#    (function(x) if (x == "na") {x} else {as.numeric(x)})
#
#  if (tmp_bw == "na") {
#    tmp_bym_dist <- dt_for.reg[, .N, by = .(billing.ym_mid)]
#    tmp_share <-
#      tmp_bym_dist[
#        , share := N / tmp_bym_dist[, sum(N, na.rm = TRUE)]
#      ]$share
#  } else {
#    tmp_bym_dist <-
#      dt_for.reg[
#        tmp_bw * -1 <= kwh_total_in.percent_normalize_period0 &
#          kwh_total_in.percent_normalize_period0 <= tmp_bw,
#        .N,
#        by = .(billing.ym_mid)
#      ]
#    tmp_share <-
#      tmp_bym_dist[
#        , share := N / tmp_bym_dist[, sum(N, na.rm = TRUE)]
#      ]$share
#  }
#  tmp_dt <- data.table(
#    bw = paste0(tmp_bw, "%"),
#    n = length(tmp_share),
#    mean = mean(tmp_share, na.rm = TRUE),
#    sd = sd(tmp_share, na.rm = TRUE),
#    p0 = quantile(tmp_share, probs = 0.00, na.rm = TRUE),
#    p25 = quantile(tmp_share, probs = 0.25, na.rm = TRUE),
#    p50 = quantile(tmp_share, probs = 0.50, na.rm = TRUE),
#    p75 = quantile(tmp_share, probs = 0.75, na.rm = TRUE),
#    p100 = quantile(tmp_share, probs = 1.00, na.rm = TRUE)
#  )
#  dt_bym.dist_by.bw <- rbind(dt_bym.dist_by.bw, tmp_dt)
#}
#
#dt_bym.dist_by.bw <- rbind(dt_bym.dist_by.bw, dt_bym.dist_by.bw[1])
#dt_bym.dist_by.bw <- dt_bym.dist_by.bw[-1]
#
#
#hux.table_bym.dist_by.bw <- as_hux(dt_bym.dist_by.bw)
#number_format(hux.table_bym.dist_by.bw)[, 2] <- fmt_pretty()
#number_format(hux.table_bym.dist_by.bw)[-1, -1:-2] <- fmt_percent(digits = 5)
#
#contents(hux.table_bym.dist_by.bw)[1, ] <-
#  c("Bandwidth", "N", "Mean", "Std. Dev.", "P0", "P25", "P50", "P75", "P100")
#contents(hux.table_bym.dist_by.bw)[
#  dt_bym.dist_by.bw[, .N] + 1, 1
#] <- "N/A"
#
#
#align(hux.table_bym.dist_by.bw)[, 1] <- "center"
#align(hux.table_bym.dist_by.bw)[1, ] <- "center"
#
#right_border(hux.table_bym.dist_by.bw)[, 1] <- brdr(0.4, "solid")
#right_border(hux.table_bym.dist_by.bw)[, 2] <- brdr(0.4, "solid")
#top_border(hux.table_bym.dist_by.bw)[1, ] <- brdr(0.4, "double")
#bottom_border(hux.table_bym.dist_by.bw)[1, ] <- brdr(0.4, "solid")
#hux.table_bym.dist_by.bw <-
#  set_bottom_border(
#    hux.table_bym.dist_by.bw, final(1), , brdr(0.4, "double")
#  )
#
#col_width(hux.table_bym.dist_by.bw) <- c(3, 1.5, rep(3, times = 7))
#
#
#
## -------  -------
#
#dt_obs_by.rate.code <- dt_for.reg[, .N, keyby = .(rate_code_normalize, year(period_from))] %>% dcast(., rate_code_normalize ~ year)
#dt_obs_by.rate.code[, total_by.rate.code := rowSums(.SD, na.rm = TRUE), .SDcols = names(dt_obs_by.rate.code)[-1]]
#dt_obs_by.rate.code <- rbind(
#  dt_obs_by.rate.code,
#  cbind(
#    data.table("rate_code_normalize" = "total_by.year"),
#    dt_obs_by.rate.code[
#      ,
#      lapply(.SD, sum, na.rm = TRUE),
#      .SDcols = names(dt_obs_by.rate.code)[-1]
#    ]
#  )
#)
#
#hux.table_obs_by.rate.code <- as_hux(dt_obs_by.rate.code)
#
#number_format(hux.table_obs_by.rate.code)[-1, ] <- fmt_pretty()
#
#contents(hux.table_obs_by.rate.code)[1, 1] <- "Rate Code"
#contents(hux.table_obs_by.rate.code)[1, length(names(dt_obs_by.rate.code))] <-
#  "Total by Rate Code"
#contents(hux.table_obs_by.rate.code)[dt_obs_by.rate.code[, .N] + 1, 1] <-
#  "Total by Year"
#
#align(hux.table_obs_by.rate.code)[, 1] <- "center"
#align(hux.table_obs_by.rate.code)[1, ] <- "center"
#
#
#right_border(hux.table_obs_by.rate.code)[, 1] <- brdr(0.4, "solid")
#bottom_border(hux.table_obs_by.rate.code)[1, ] <- brdr(0.4, "solid")
#
#
## --------------------------------------------------
##
## --------------------------------------------------
## -------  -------
#dt_consumption_summary_by.rate.code <- setDT(NULL)
#list_rate.codes <- list(c("RSCH", "RSEH"), c("RSGH"))
#for (code in list_rate.codes) {
#  for (season in c("Summer", "Winter")) {
#    tmp_summary <- summary_consumption(code, YEAR_LOWER, YEAR_UPPER, season)
#    dt_consumption_summary_by.rate.code <- rbind(
#      dt_consumption_summary_by.rate.code, tmp_summary
#    )
#  }
#}
#
#hux.table_consumption_summary_by.rate.codes <-
#  as_hux(dt_consumption_summary_by.rate.code)
#
#number_format(hux.table_consumption_summary_by.rate.codes)[-1, 4:5] <- list(
#  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
#  )
#
#contents(hux.table_consumption_summary_by.rate.codes)[1, ] <-
#  c("Statistic", "Rate Code", "Season", "Daily Average Consumption", "Normalized Monthly Consumption")
#
#bottom_border(hux.table_consumption_summary_by.rate.codes)[1, ] <- brdr(0.4, "solid")
#
#
#
## -------  -------
#
#list_rate.codes <- list(c("RSCH", "RSEH"), c("RSCH", "RSEH"), "RSGH", "RSGH")
#list_seasons <- list("Summer", "Winter", "Summer", "Winter")
#list_results <-
#  mapply(
#    distribution_by.rate.code,
#    rate.codes_in.vector = list_rate.codes, season_in.str = list_seasons,
#    year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
#  )
#
#dt_dist_by.rate.code <- rbindlist(list(list_results[, 1], list_results[, 2], list_results[, 3], list_results[, 4]))
#
#
#hux.table_consumption_dist_short_by.rate.code <- as_hux(dt_dist_by.rate.code[statistic %in% paste0(seq(0, 100, by = 25), "%")])
#
#number_format(hux.table_consumption_dist_short_by.rate.code)[-1, c(3, 5)] <- fmt_pretty()
#number_format(hux.table_consumption_dist_short_by.rate.code)[-1, c(2, 4)] <- list(
#  function(x) formatC(x, digits = 2, width = 6, format = "f", big.mark = ",")
#  )
#
#contents(hux.table_consumption_dist_short_by.rate.code)[1, ] <- c(
#  "Quantile",
#  "Daily Average Consumption", "N of Daily",
#  "Normalized Monthly Consumption", "N of Monthly",
#  "Rate Code", "Season"
#)
#
#bottom_border(hux.table_consumption_dist_short_by.rate.code)[1, ] <- brdr(0.4, "solid")
#
#
## -------  -------
#
#list_rate.codes <- list(c("RSCH", "RSEH"), c("RSCH", "RSEH"), "RSGH", "RSGH")
#list_seasons <- list("Summer", "Winter", "Summer", "Winter")
#list_results <-
#  mapply(
#    distribution_by.bw.and.rate.code,
#    rate.codes_in.vector = list_rate.codes, season_in.str = list_seasons,
#    year_lower = YEAR_LOWER, year_upper = YEAR_UPPER
#  )
#
##dt_dist_by.bw.and.rate.code <- rbindlist(list(list_results[, 1], list_results[, 2], list_results[, 3], list_results[, 4]))
#dt_dist_by.bw.and.rate.code <- rbindlist(list(list_results[, 4]))
#
#hux.table_consumption_dist_by.bw.and.rate.code <- as_hux(dt_dist_by.bw.and.rate.code)
#
#number_format(hux.table_consumption_dist_by.bw.and.rate.code)[-1, -1] <- fmt_pretty()
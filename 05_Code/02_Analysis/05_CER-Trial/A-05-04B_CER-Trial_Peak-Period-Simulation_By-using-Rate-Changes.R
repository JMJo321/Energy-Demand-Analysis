# < Description > *
# > Script Group Indicator Number and Name:
# # A-05, CER Trial
# #
# > Script Number(s):
# # A-05-04B
# #
# > Purpose of the script(s):
# # Create table(s) and plot(s) that illustrate treatment effects in terms of
# # household's hourly electricity consumption, Use the estimates obtained
# # by using rate changes.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(ggplot2)
library(huxtable)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Dataset(s)/Script(s) is(are) loaded
# # 1.1. For the For-Regression DT
FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY <-
  "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  "CER",
  FILE_TO.LOAD_CER_FOR.REGRESSION_ELECTRICITY,
  sep = "/"
)

# # 1.2. For Regression Results
FILE_TO.LOAD_CER_ESTIMATES <- paste0(
  "CER_Estimates_Rate-Period-Level-Treatment-Effect_",
  "By-Season-and-Tariff_With-Rate-Changes_by-Rate-Period.RData"
)
PATH_TO.LOAD_CER_ESTIMATES <- paste(
  PATH_DATA_ANALYSIS,
  "04_CER",
  FILE_TO.LOAD_CER_ESTIMATES,
  sep = "/"
)


# # 2. Path(s) to which Plot(s) will be stored
DIR_TO.SAVE_CER_PLOT <- paste(
  PATH_NOTE,
  "07_CER-Trials", "02_Figures",
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. A List that include labels of kwh-related variables
list_category.descriptions <- list(
  kwh = "Pre-Treatment Average Consumption",
  kwh_non.temp.effect = "Treatment Effect: Non-Temperature",
  kwh_temp.effect = "Treatment Effect: Temperature",
  kwh_effects = "Treatment Effect: Total",
  kwh_after.adding.effects = "Post-Treatment Average Consumption"
)


# ------- Define function(s) -------
# # 1. To extract HDDs from a Range of HDDs
get_hdd <- function (range_hdd) {
  hdd <-
    str_replace_all(range_hdd, "(\\()|(\\[)|(])", "") %>%
      str_split(., ",", simplify = TRUE) %>%
      as.numeric(.) %>%
      sum(., na.rm = TRUE) / 2
  return(hdd)
}


# ------------------------------------------------------------------------------
# Load Data for Simulation(s)
# ------------------------------------------------------------------------------
# ------- Load Data -------
# # 1. Load the For-Regression DT
dt_for.reg <- arrow::read_parquet(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# # 2. Load a DT that includes Regression Results
load(PATH_TO.LOAD_CER_ESTIMATES)


# ------------------------------------------------------------------------------
# Estimate the Distribution of HDDs
# ------------------------------------------------------------------------------
# ------- Estimate the Distribution of HDDs -------
# # 1. Estimate the Distribution of HDDs
# # 1.1. Add data fields that indicates ranges of HDDs
dt_for.reg[
  ,
  `:=` (
    range_hdd_65f = cut(
      hdd_all_65f, breaks = seq(0, 48, by = 2), include.lowest = TRUE
    ),
    range_hdd_60f = cut(
      hdd_all_60f, breaks = seq(0, 48, by = 2), include.lowest = TRUE
    )
  )
]

# # 1.2. Create a DT that includes the weight of each range whose ref. temp. is
# #      65 degrees of Farenheit
dt_distribution_hdd_65f <- dt_for.reg[
  is_in.sample_incl.control == TRUE, .N, by = .(date, range_hdd_65f)
][
  , N := NULL
][
  , .N, keyby = .(range_hdd_65f)
]
setnames(
  dt_distribution_hdd_65f,
  old = c("range_hdd_65f", "N"),
  new = c("range_hdd", "n")
)
dt_distribution_hdd_65f[
  ,
  weight := n / sum(dt_distribution_hdd_65f$n, na.rm = TRUE)
]

# # 1.3. Create a DT that includes the weight of each range whose ref. temp. is
# #      60 degrees of Farenheit
dt_distribution_hdd_60f <- dt_for.reg[
  is_in.sample_incl.control == TRUE, .N, by = .(date, range_hdd_60f)
][
  , N := NULL
][
  , .N, keyby = .(range_hdd_60f)
]
setnames(
  dt_distribution_hdd_60f,
  old = c("range_hdd_60f", "N"),
  new = c("range_hdd", "n")
)
dt_distribution_hdd_60f[
  ,
  weight := n / sum(dt_distribution_hdd_60f$n, na.rm = TRUE)
]

# # 1.4. Combine the DTs created above
dt_distribution_hdd <- rbind(
  dt_distribution_hdd_60f[, ref.temp_f := 65],
  dt_distribution_hdd_65f[, ref.temp_f := 60]
)


# ------------------------------------------------------------------------------
# Create Representative Load Profile(s) by Range of HDDs
# ------------------------------------------------------------------------------
# ------- Create Representativ Load Profile(s) by Range of HDDs -------
# ## Note:
# ## Exploit consumption history of households in the control group.

# # 1. Create Load Profile(s)
# # 1.1. Load profile based on 65 degrees of Fahrenheit
dt_avg.consumption_65f <- dt_for.reg[
  is_in.sample_incl.control == TRUE & group == "Control",
  lapply(.SD, mean, na.rm = TRUE),
  .SDcols = "kwh",
  keyby = .(range_hdd_65f, interval_30min)
]
setnames(
  dt_avg.consumption_65f,
  old = "range_hdd_65f",
  new = "range_hdd"
)

# # 1.2. Load profile based on 60 degrees of Fahrenheit
dt_avg.consumption_60f <- dt_for.reg[
  is_in.sample_incl.control == TRUE & group == "Control",
  lapply(.SD, mean, na.rm = TRUE),
  .SDcols = "kwh",
  keyby = .(range_hdd_60f, interval_30min)
]
setnames(
  dt_avg.consumption_60f,
  old = "range_hdd_60f",
  new = "range_hdd"
)

# # 1.3. Combine the load profiles created above
dt_avg.consumption <- rbind(
  dt_avg.consumption_60f[, ref.temp_f := 60],
  dt_avg.consumption_65f[, ref.temp_f := 65]
)


# # 2. Modify the DT created above
# # 2.1. Add data fields
# # 2.1.1. Extract HDDs from the Ranges of HDDs
dt_avg.consumption[
  ,
  hdd := lapply(range_hdd, get_hdd) %>% as.vector(., mode = "numeric")
]
# # 2.1.2. Add the information about the distribution of HDDs
dt_avg.consumption <- merge(
  x = dt_avg.consumption,
  y = dt_distribution_hdd,
  by = c("range_hdd", "ref.temp_f"),
  all.x = TRUE
)


# ------------------------------------------------------------------------------
# Compute the Weighted Average Load Profile, based on the Distribution of HDDs
# ------------------------------------------------------------------------------
# ------- Create a DT by Extracing Estimates -------
# # 1. Create a DT that includes Estimated Treatment Effects
dt_estimates_selected <-
  dt_estimates_by.season.and.tariff_w.rate.change_by.rate.period[
    model == "FEs: i-BY-w + d-BY-w + m-BY-w"
  ][
    str_detect(term, "treatment.and.post")
  ]


# # 2. Modify the DT created above
# # 2.1. Add data field(s)
# # 2.1.1. Add a column that shows the category of treatment effects
dt_estimates_selected[
  str_detect(term, "hdd"),
  category := "Temperature Effect"
]
dt_estimates_selected[
  is.na(category),
  category := "Non-Temperature Effect"
]


# # 3. Do a Test
stopifnot(
  dt_estimates_selected[
    , .N, by = .(model, ref.temp_f, season, rate.period, category)
  ][
    N > 1, .N
  ] == 0
)


# ------ Create a DT incl. both the average consumption and the estimates ------
# # 1. Create Objects that will be used later
# # 1.1. A DT that includes subsetting conditions
dt_cases <- dt_estimates_selected[
  , .N, by = .(model, ref.temp_f, season, rate.period)
][
  , N := NULL
]

# # 1.2. A DT that includes the mapping from intervals to rate periods
dt_intervals.and.rate.periods <- dt_for.reg[
  , .N, by = .(rate.period_detail_level1, interval_30min)
][
  , N := NULL
]


# # 2. Create a DT that includes both the average consumption and the estimates
# # 2.1. Create a DT by merging two DTs
dt_load.profiles <- setDT(NULL)
# ## Note:
# ## An empty DT to which data will be appended.

for (row.idx in 1:dt_cases[, .N]) {
  # ## Create temporary objects
  tmp_model <- dt_cases[row.idx]$model
  tmp_ref.temp_f <- dt_cases[row.idx]$ref.temp_f
  tmp_season <- dt_cases[row.idx]$season
  tmp_rate.period <- dt_cases[row.idx]$rate.period

  # ## Make a subsetting condition in str
  tmp_condition <- paste(
    paste0("model == '", tmp_model, "'"),
    paste0("ref.temp_f == ", tmp_ref.temp_f),
    paste0("season == '", tmp_season, "'"),
    paste0("rate.period == '", tmp_rate.period, "'"),
    sep = " & "
  )

  # ## Extract treatment effects based on the subsetting condition
  tmp_temp.effect <- dt_estimates_selected[
    eval(parse(text = tmp_condition))
  ][
    category == "Temperature Effect"
  ] %>%
    .$estimate
  tmp_non.temp.effect <- dt_estimates_selected[
    eval(parse(text = tmp_condition))
  ][
    category == "Non-Temperature Effect"
  ] %>%
    .$estimate

  # ## Extract average consumption by using the temporary objects
  tmp_dt <- dt_avg.consumption[
    ref.temp_f == tmp_ref.temp_f &
      interval_30min %in%
        dt_intervals.and.rate.periods[
          rate.period_detail_level1 == tmp_rate.period
        ]$interval_30min
  ]

  # ## Add data fields to the DT created above
  tmp_dt[
    ,
    `:=` (
      model = tmp_model,
      season = tmp_season,
      rate.period = tmp_rate.period
    )
  ]
  tmp_dt[
    ,
    `:=` (
      effect_temp = tmp_temp.effect,
      effect_non.temp = tmp_non.temp.effect
    )
  ]

  # ## Append the DT
  dt_load.profiles <- rbind(dt_load.profiles, tmp_dt)
}


# # 2.2. Modify the DT created above
# # 2.2.1. Add data fields
# # 2.2.1.1. Columns that show treatment effects in terms of kWh
dt_load.profiles[
  ,
  `:=` (
    kwh_temp.effect = effect_temp * hdd,
    kwh_non.temp.effect = effect_non.temp
  )
]
dt_load.profiles[, kwh_effects := kwh_temp.effect + kwh_non.temp.effect]


# ------------------------------------------------------------------------------
# Do Simulations
# ------------------------------------------------------------------------------
# ------- Create a DT that will be used for Simulations -------
# # 1. Create a DT from the DT including household's average consumption
dt_simulation_base <- copy(
  dt_avg.consumption[
    ref.temp_f == 60,
    .(range_hdd, ref.temp_f, interval_30min, kwh, hdd)
  ]
)


# # 2. Modify the DT created above
# # 2.1. Add data fields
# # 2.1.1. Add a column that shows the mapping from 30-minute interval to
# #        1-hour interval
dt_intervals <- dt_for.reg[
  , .N, by = .(interval_30min, interval_hour)
][
  , N := 1:.N, by = .(interval_hour)
]
dt_simulation_base <- merge(
  x = dt_simulation_base,
  y = dt_intervals[, .(interval_30min, interval_hour)],
  by = "interval_30min",
  all.x = TRUE
)
# # 2.1.2. Add a column that shows the mapping from 30-minute interval to
# #        rate periods
cols_by <- c("rate.period", "interval_30min")
dt_simulation_base <- merge(
  x = dt_simulation_base,
  y = dt_load.profiles[, .N, by = cols_by][, .SD, .SDcols = cols_by],
  by = "interval_30min",
  all.x = TRUE
)
# # 2.1.3. Add a column that indicates the length of each rate periods
dt_rate.period.length <- dt_load.profiles[
  ,
  .N, by = .(rate.period, interval_30min)
][
  ,
  .N, by = .(rate.period)
][
  ,
  length_hours := N / 2
][
  ,
  N := NULL
]
dt_simulation_base <- merge(
  x = dt_simulation_base,
  y = dt_rate.period.length[, .(rate.period, length_hours)],
  by = "rate.period",
  all.x = TRUE
)

# # 2.2. Reorder columns
cols_reorder <- c(
  "range_hdd", "hdd", "ref.temp_f",
  "interval_30min", "interval_hour", "length_hours",
  "kwh"
)
setcolorder(dt_simulation_base, cols_reorder)


# ------- Perform Simulation(s) -------
# # 1. Create a DT for the Simulation
# # 1.1. Create a DT, and then add a data field that shows the mapping from
# #      ranges of HDDs to tariff structures
interval_a <- dt_simulation_base[, .N, keyby = .(range_hdd)][1:4]$range_hdd
interval_b <- dt_simulation_base[, .N, keyby = .(range_hdd)][5:8]$range_hdd
interval_c <- dt_simulation_base[, .N, keyby = .(range_hdd)][9:12]$range_hdd
interval_d <- dt_simulation_base[, .N, keyby = .(range_hdd)][13:16]$range_hdd
dt_simulation_interval <-
  copy(dt_simulation_base) %>%
    .[
      range_hdd %in% interval_a,
      `:=` (assignment = "A", rate.change = 6)
    ] %>%
    .[
      range_hdd %in% interval_b,
      `:=` (assignment = "B", rate.change = 12)
    ] %>%
    .[
      range_hdd %in% interval_c,
      `:=` (assignment = "C", rate.change = 18)
    ] %>%
    .[
      range_hdd %in% interval_d,
      `:=` (assignment = "D", rate.change = 24)
    ] %>%
    .[, assignment := factor(assignment, levels = c("A", "B", "C", "D"))]

# # 2. Modify the DT created above
# # 2.1. Add data fields
# # 2.1.1. Add columns that show estimated treatment effects
dt_simulation_interval <- merge(
  x = dt_simulation_interval,
  y = dt_load.profiles[
    ref.temp_f == 60,
    .N,
    keyby = .(model, season, rate.period, effect_temp, effect_non.temp)
  ][
    , N := NULL
  ],
  by.x = "rate.period",
  by.y = "rate.period",
  all.x = TRUE
)
# # 2.1.2. Add columns that illustrate the simulated treatment effects,
# #        in terms of consumption
dt_simulation_interval[
  ,
  `:=` (
    kwh_temp.effect = effect_temp * rate.change * hdd,
    kwh_non.temp.effect = effect_non.temp * rate.change
  )
]
dt_simulation_interval[
  ,
  `:=` (
    kwh_effects = kwh_temp.effect + kwh_non.temp.effect,
    kwh_after.adding.effects = kwh + kwh_temp.effect + kwh_non.temp.effect
  )
]

# # 3. Aggregate the simulation result above at rate-period level
cols_by <- c("range_hdd", "hdd", "rate.period", "length_hours", "assignment")
cols_sd <- c(
  "kwh",
  "effect_temp", "effect_non.temp",
  "kwh_temp.effect", "kwh_non.temp.effect", "kwh_effects",
  "kwh_after.adding.effects"
)
dt_simulation_interval_agg <-
  dt_simulation_interval[
    , lapply(.SD, sum, na.rm = TRUE), .SDcols = cols_sd, by = cols_by
  ]

# ## Note:
# ## Additional sumulations can be performed, if necessary.


# ------- Create DT(s) for Plot(s) -------
# # 1. Create DT(s) by melting the simulation result(s) obtained above
# # 1.1. For Case 1
# # 1.1.1. Create a DT by melting the DT including the simulation result for
# #        Case 1
id.vars <- names(dt_simulation_interval_agg)[
  str_detect(names(dt_simulation_interval_agg), "kwh", negate = TRUE)
]
measure.vars <- names(dt_simulation_interval_agg)[
  str_detect(names(dt_simulation_interval_agg), "kwh", negate = FALSE)
]
dt_simulation_interval_agg_melt <- melt(
  dt_simulation_interval_agg,
  id.vars = id.vars,
  measure.vars = measure.vars,
  variable.name = "category",
  value.name = "kwh",
  variable.factor = FALSE
)
# # 1.1.2. Modify the DT created above
# # 1.1.2.1. Add data fields
dt_simulation_interval_agg_melt[, kwh_for.plot := kwh]
dt_simulation_interval_agg_melt[kwh < 0, kwh_for.plot := kwh * -1]
# ## Note:
# ## This variable is necessary to use the `geom_area` function.
dt_simulation_interval_agg_melt[
  , range_hdd := factor(range_hdd, ordered = TRUE)
]
dt_simulation_interval_agg_melt[
  ,
  category_desc := (
    lapply(category, function (x) list_category.descriptions[[x]]) %>%
      as.character(.) %>%
      factor(., levels = (list_category.descriptions %>% as.character(.)))
  )
]


# ------------------------------------------------------------------------------
# Create Plot(s)
# ------------------------------------------------------------------------------
# ------- Create Objects that will be used to make ggplot Objects -------
# # 1. Define the common options
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)

# # 2. Create Color Palette(s)
color.pal_signal <- unikn::usecol(pal_signal, n = 3)


# ------- Create a ggplot Object: for Simulation Result(s) -------
# # 1. Create objects that will be utilized to make ggplot Objects
# # 1.1. Make a vector that includes column names used to create ggplot objects
categories_for.plot <- c(
  "kwh_temp.effect", "kwh_non.temp.effect", "kwh_after.adding.effects"
)

# # 1.2. Make a DT that includes annotations regarding tariffs
dt_annotate <- data.table(
  x = c(5, 13, 21, 28),
  y = rep(2.5, times = 4),
  label = seq(6, 24, by = 6) %>% paste0("Rate Change: ", ., " Cents per kWh")
)


# # 2. Create ggplot object(s)
plot_simulation <-
  ggplot(
    data = dt_simulation_interval_agg_melt[
      str_detect(rate.period, "^Peak") & category %in% categories_for.plot
    ]
  ) +
    geom_vline(xintercept = c(9, 17, 25), alpha = 0.3, linetype = "dashed") +
    geom_area(
      aes(x = hdd, y = kwh_for.plot / length_hours, fill = category_desc),
      alpha = 0.4
    ) +
    geom_line(
      data = dt_simulation_interval_agg_melt[
        str_detect(rate.period, "^Peak") &
          category %in% c("kwh", "kwh_after.adding.effects")
      ],
      aes(x = hdd, y = kwh_for.plot / length_hours, linetype = category_desc)
    ) +
    annotate(
      "text",
      x = dt_annotate$x, y = dt_annotate$y, label = dt_annotate$label,
      color = "grey", size = 4
    ) +
    scale_x_continuous(
      breaks = dt_simulation_interval_agg_melt[, .N, keyby = .(hdd)]$hdd,
      labels = dt_simulation_interval_agg_melt[
        , .N, keyby = .(range_hdd)
      ]$range_hdd %>% as.character(.)
    ) +
    scale_fill_manual(
      values = color.pal_signal,
      labels = c(
        "Treatment Effect: Non-Temperatue",
        "Treatment Effect: Temperatue",
        "Consumption NOT explained by Treatment Effects"
      )
    ) +
    labs(
      x = "\nIntervals of HDDs",
      y = "Average Consumption  (kWh per 1-Hour Interval)\n",
      fill = "Treatment Effects",
      linetype = "Average Consumptions"
    ) +
    plot.options


# ------- Save the ggplot objects created above in PNG format -------
# # 1. For Simulation Result(s)
plot.save(
  paste(
    DIR_TO.SAVE_CER_PLOT,
    "CER_Simulation_By-using-Rate-Changes.png",
    sep = "/"
  ),
  plot_simulation,
  width = 50, height = 25, units = "cm"
)

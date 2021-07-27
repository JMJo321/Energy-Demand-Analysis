# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-04A
# #
# > Purpose of the script(s)
# # : Descriptive Analysis - Electricity Consumption over time

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
library(latex2exp)
library(ggplot2)
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
# # 1. Path(s) from which Dataset(s) is(are) loaded
# # 1.1. For Metering Data
DIR_TO.LOAD_CER <- "CER"
FILE_TO.LOAD_CER_METERING_ELECTRICITY <-
  "CER_Extended-Metering_Electricity.RData"
PATH_TO.LOAD_CER_METERING_ELECTRICITY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CER, "Metering",
  FILE_TO.LOAD_CER_METERING_ELECTRICITY,
  sep = "/"
)

# # 2. Path(s) to which Plots will be stored
DIR_TO.SAVE_PLOT <- paste(
  PATH_NOTE, "07_CER-Trials", "02_Figures", "Descriptive-Analysis",
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Treatement Begin Date
DATE_BEGIN.OF.TREATMENT <- as.Date("2010-01-01")


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create DTs that are for making plots
# ------------------------------------------------------------------------------
# ------- Load the combined metering dataset -------
# # 1. Load the dataset
load(PATH_TO.LOAD_CER_METERING_ELECTRICITY)


# # 2. Modify the DT loaded
# # 2.1. Add column(s)
# # 2.1.1. Add columns indicating whether each observation is in the treatment
# #        period
dt_metering_e[
  is_treatment.period == TRUE,
  is_treatment.period_in.factor := "Treatment"
]
dt_metering_e[
  is_treatment.period == FALSE,
  is_treatment.period_in.factor := "Baseline"
]
dt_metering_e[
  ,
  is_treatment.period_in.factor := factor(is_treatment.period_in.factor)
]
# # 2.1.2. Add columns indicating whether each observation is treated or not
dt_metering_e[
  is_treated_r == TRUE,
  is_treated_r_in.factor := "Treatment Group"
]
dt_metering_e[
  is_treated_r == FALSE,
  is_treated_r_in.factor := "Control Group"
]
dt_metering_e[
  ,
  is_treated_r_in.factor := factor(is_treated_r_in.factor)
]
# # 2.1.3. Add a column showing ranges of temperature
dt_metering_e[, range_temp_f := cut(temp_f, breaks = seq(10, 80, by = 2))]


# ------- Create DT(s) -------
# # 0. Set conditions for subsetting the DT
# # 0.1. For Consumption by Hour of Day
conditions_by.date <- paste(
  "alloc_group == '1'", # Residential only
  "is_weekend == FALSE", # TOU pricing was active on nonholiday weekdays
  "is_holiday == FALSE", # TOU pricing was active on nonholiday weekdays
  sep = " & "
)
# # 0.2. For Consumption over Time
conditions_by.hour <- paste(
  conditions_by.date,
  "7 <= month(date)", # The first date of the baseline period was July 13, 2009
  sep = " & "
)


# # 1. Consumption by Hour of Day
# # 1.1. Consumption by Hour of Day
# # 1.1.1. DT for consumption by hour of day
cols_by_hour <- c(
  "interval_hour", "is_treated_r_in.factor", "is_treatment.period_in.factor"
)
dt_avg.kwh_hour <- dt_metering_e[
  eval(parse(text = conditions_by.hour)),
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_hour
][
  , dummy_hour := interval_hour + 0.5
]
# # 1.1.2. DT for consumption by hour of day and percentile
# # 1.1.2.1. Compute hourly average consumption by hour of day for each ID
dt_avg.kwh_id.and.hour <- dt_metering_e[
  eval(parse(text = conditions_by.hour)),
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = c("id", cols_by_hour)
]
# # 1.1.2.2. Compute percentiles by using observations during baseline period
percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
dt_percentiles <- dt_avg.kwh_id.and.hour[
  is_treatment.period_in.factor == "Baseline",
  lapply(.SD, quantile, probs = percentiles, na.rm = TRUE),
  .SDcols = "kwh",
  by = cols_by_hour
]
# # 1.1.2.3. Assign each ID to one of percentiles
label_percentiles <- c(
  "[0%,10%]", "(10%,25%]", "(25%,50%]", "(50%,75%]", "(75%,90%]", "(90%,100%]"
)
dt_for.loop <- dt_percentiles[
  , .N, by = .(interval_hour, is_treated_r_in.factor)
][
  , N:= NULL
]
for (row in 1:dt_for.loop[, .N]) {
  tmp_interval <- dt_for.loop[row]$interval_hour
  tmp_group <- dt_for.loop[row]$is_treated_r_in.factor
  tmp_kwh_percentile <- dt_percentiles[
    interval_hour == tmp_interval & is_treated_r_in.factor == tmp_group
  ]$kwh %>%
    sort(., decreasing = FALSE) %>%
    c(0, ., 1000) # 1000 means a number large enough
  dt_avg.kwh_id.and.hour[
    interval_hour == tmp_interval & is_treated_r_in.factor == tmp_group &
      is_treatment.period_in.factor == "Baseline",
    range_percentile := cut(
      kwh,
      breaks = tmp_kwh_percentile,
      include.lowest = TRUE,
      labels = label_percentiles
    )
  ]
}
dt_avg.kwh_id.and.hour[
  ,
  range_percentile := factor(
    range_percentile,
    levels = label_percentiles[seq(length(label_percentiles), 1, by = -1)]
  )
]
# # 1.1.2.4. Do a simple test
stopifnot(
  dt_avg.kwh_id.and.hour[
    !is.na(range_percentile),
    .N, by = .(id, interval_hour, range_percentile)
  ][
    N > 1
  ] == 0
)
# # 1.1.2.5. Create a DT by using the DT created above
cols_extract <- names(dt_avg.kwh_id.and.hour)[
  str_detect(names(dt_avg.kwh_id.and.hour), "^range_", negate = TRUE)
]
dt_avg.kwh_hour.and.percentile <- merge(
  x = dt_avg.kwh_id.and.hour[,.SD, .SDcols = cols_extract],
  y = dt_avg.kwh_id.and.hour[
    !is.na(range_percentile),
    .N, by = .(id, interval_hour, range_percentile)
  ][
    , N := NULL
  ],
  by = c("id", "interval_hour"),
  all.x = TRUE
) %>%
  .[
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    keyby = c(cols_by_hour, "range_percentile")
  ]
dt_avg.kwh_hour.and.percentile[, dummy_hour := interval_hour + 0.5]

# # 1.2. Consumption by Hour of Day and Treatment (Tariff and Stimulus)
cols_by_hour.and.treatment <- c(
  cols_by_hour,
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc"
)
dt_avg.kwh_hour.and.treatment <- dt_metering_e[
  eval(parse(text = conditions_by.hour)),
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_hour.and.treatment
][
  , dummy_hour := interval_hour + 0.5
]


# # 2. Consumption over Time
cols_by_date <- c(
  "date",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc"
)
dt_avg.kwh_date <- dt_metering_e[
  eval(parse(text = conditions_by.date)),
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = c("id", cols_by_date)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_date
]


# ------------------------------------------------------------------------------
# Create Plots
# ------------------------------------------------------------------------------
# ------- Set common plot options -------
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Create ggplot objects: W.R.T. Average Consumption -------
# # 1. For Average Consumption by Hour of Day
# # 1.1. For Average Consumption by Hour of Day
plot_avg.kwh_interval <-
  ggplot(
    data = dt_avg.kwh_hour,
    aes(
      x = dummy_hour, y = kwh,
      color = is_treatment.period_in.factor,
      shape = is_treatment.period_in.factor,
      linetype = is_treatment.period_in.factor
    )
  ) +
    geom_line(lwd = 0.7, alpha = 0.7) +
    geom_point(size = 2.0, alpha = 0.7) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(is_treated_r_in.factor ~ ., nrow = 1) +
    scale_x_continuous(breaks = seq(0, 24, by = 2)) +
    scale_y_continuous(breaks = seq(0.25, 1.75, by = 0.25)) +
    scale_shape_manual(values = c(15, 16)) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    labs(
      x = "Hour of Day",
      y = "Hourly Average Consumption  (kWh per Hour)",
      color = "Periods",
      shape = "Periods",
      linetype = "Periods"
    ) +
    plot.options

# # 1.2. For Average Consumption by Hour of Day and Percentile
plot_avg.kwh_interval.and.perentile <-
  ggplot(
    data = dt_avg.kwh_hour.and.percentile,
    aes(
      x = dummy_hour, y = kwh,
      color = range_percentile,
      shape = is_treatment.period_in.factor,
      linetype = is_treatment.period_in.factor
    )
  ) +
    geom_line(lwd = 0.7, alpha = 0.7) +
    geom_point(size = 2.0, alpha = 0.7) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(is_treated_r_in.factor ~ ., nrow = 1) +
    scale_x_continuous(breaks = seq(0, 24, by = 2)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 3.5, by = 0.5)) +
    scale_color_brewer(palette = "Spectral") +
    scale_shape_manual(values = c(15, 16)) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    labs(
      x = "Hour of Day",
      y = "Hourly Average Consumption  (kWh per Hour)",
      color = "Percentiles",
      shape = "Periods",
      linetype = "Periods"
    ) +
    plot.options

# # 1.3. For Average Consumption by Hour of Day and Treatment
# #      (Tariff and Stimulus)
plot_avg.kwh_interval.and.treatment <-
  ggplot(
    data = dt_avg.kwh_hour.and.treatment[
      alloc_r_tariff %in% c("A", "B", "C", "D")
    ]
  ) +
    geom_line(
      aes(x = dummy_hour, y = kwh, color = is_treatment.period_in.factor)
    ) +
    geom_line(
      data = dt_avg.kwh_hour.and.treatment[
        alloc_r_tariff == "E",
        .(interval_hour, dummy_hour, kwh, is_treatment.period_in.factor)
      ],
      aes(x = dummy_hour, y = kwh, color = is_treatment.period_in.factor),
      linetype = 'dashed', alpha = 0.7
    ) +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    facet_grid(alloc_r_stimulus_desc ~ alloc_r_tariff_desc) +
    scale_x_continuous(breaks = seq(0, 24, by = 2)) +
    labs(
      x = "Hour of Day",
      y = "Consumption (kWh per Hour)",
      color = "Periods"
    ) +
    plot.options


# # 2. For Average Consumption over Time
# # 2.1. By Tariff
plot_avg.kwh_date_by.tariff <-
  ggplot(
    data = dt_avg.kwh_date[alloc_r_tariff %in% c("A", "B", "C", "D")]
  ) +
    geom_line(aes(x = date, y = kwh, color = alloc_r_stimulus_desc)) +
    geom_line(
      data = dt_avg.kwh_date[alloc_r_tariff == "E", .(date, kwh)],
      aes(x = date, y = kwh),
      linetype = 'dashed', alpha = 0.5
    ) +
    geom_vline(
      aes(xintercept = as.Date("2010-01-01")),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(. ~ alloc_r_tariff_desc) +
    scale_x_date(
      breaks = seq.Date(
        from = as.Date("2009-07-01"), to = as.Date("2011-01-01"),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    scale_color_viridis_d() +
    labs(
      x = "",
      y = "Consumption (kWh per Day)",
      color = "Stimuli"
    ) +
    plot.options

# # 2.2. By Stimulus
plot_avg.kwh_date_by.stimulus <-
  ggplot(
    data = dt_avg.kwh_date[alloc_r_tariff %in% c("A", "B", "C", "D")]
  ) +
    geom_line(aes(x = date, y = kwh, color = alloc_r_tariff_desc)) +
    geom_line(
      data = dt_avg.kwh_date[alloc_r_tariff == "E", .(date, kwh)],
      aes(x = date, y = kwh),
      linetype = 'dashed', alpha = 0.5
    ) +
    geom_vline(
      aes(xintercept = as.Date("2010-01-01")),
      color = "grey", linetype = "dotdash"
    ) +
    facet_wrap(. ~ alloc_r_stimulus_desc) +
    scale_x_date(
      breaks = seq.Date(
        from = as.Date("2009-07-01"), to = as.Date("2011-01-01"),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    scale_color_viridis_d() +
    labs(
      x = "",
      y = "Consumption (kWh per Day)",
      color = "Tariffs"
    ) +
    plot.options


# ------------------------------------------------------------------------------
# Save ggplot objects in PNG format
# ------------------------------------------------------------------------------
# ------- Save plots created above in PNG format -------
# # 1. Plots regarding Average Consumption
# # 1.1. For Average Consumption by Hour of Day
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Hour-Level_By-Treatment-Group_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_interval,
  width = 40, height = 20, units = "cm"
)
# # 1.2. For Average Consumption by Hour of Day and Percentile
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Hour-Level_By-Treatment-Group-and-Percentile_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_interval.and.perentile,
  width = 40, height = 20, units = "cm"
)
# # 1.3. For Average Consumption by Hour of Day and Treatment (Tariff and
# #      Stimulus)
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Hour-Level_By-Tariff-and-Stimulus_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_interval.and.treatment,
  width = 50, height = 25, units = "cm"
)

# # 1.2. For Average Consumption over Time
# # 1.2.1. By Tariff
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Date-Level_By-Tariff_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_date_by.tariff,
  width = 50, height = 25, units = "cm"
)
# # 1.2.2. By Stimulus
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Average-Consumption_Date-Level_By-Stimulus_Electricity.png",
    sep = "/"
  ),
  plot_avg.kwh_date_by.stimulus,
  width = 50, height = 25, units = "cm"
)

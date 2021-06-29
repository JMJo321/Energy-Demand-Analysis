# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-04D
# #
# > Purpose of the script(s)
# # : Descriptive Analysis - Time-of-Use Tariff Structure, excluding the
# #   Weekend Tariff.

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
# # 1. Path(s) to which Plot(s) will be stored
DIR_TO.SAVE_PLOT <- paste(
  PATH_NOTE, "07_CER-Trials", "02_Figures", "Descriptive-Analysis",
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Create a DT that includes a tariff structure
gen_tariff.structure <- function (
  alloc_r_tariff_, alloc_r_tariff_desc_,
  rate_cents.per.kwh_night,
  rate_cents.per.kwh_day,
  rate_cents.per.kwh_peak
) {
  # ## Create an empty DT
  dt <- data.table(
    interval_hour = seq(0, 24, by = 1),
    alloc_r_tariff = rep(alloc_r_tariff_, 25),
    alloc_r_tariff_desc = rep(alloc_r_tariff_desc_, 25)
  )
  # ## Add information
  dt[
    interval_hour %in% c(seq(0, 7, by = 1), seq(23, 24, by = 1)),
    `:=` (
      rate_desc = "Night",
      rate_cents.per.kwh = rate_cents.per.kwh_night
    )
  ]
  dt[
    interval_hour %in% c(seq(8, 16, by = 1), seq(19, 22, by = 1)),
    `:=` (
      rate_desc = "Day",
      rate_cents.per.kwh = rate_cents.per.kwh_day
    )
  ]
  dt[
    interval_hour %in% seq(17, 18, by = 1),
    `:=` (
      rate_desc = "Peak",
      rate_cents.per.kwh = rate_cents.per.kwh_peak
    )
  ]
  # ## Return the DT created
  return (dt)
}


# ------------------------------------------------------------------------------
# Create a DT that contain TOU Tariff Structures
# ------------------------------------------------------------------------------
# ------- Create a DT that contain TOU Tariff Structure -------
# # 1. Create a DT
dt_tou <- rbind(
  gen_tariff.structure(
    alloc_r_tariff_ = "E",
    alloc_r_tariff_desc_ = "Control",
    rate_cents.per.kwh_night = 14.1,
    rate_cents.per.kwh_day = 14.1,
    rate_cents.per.kwh_peak = 14.1
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "A",
    alloc_r_tariff_desc_ = "Tariff A",
    rate_cents.per.kwh_night = 12.0,
    rate_cents.per.kwh_day = 14.0,
    rate_cents.per.kwh_peak = 20.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "B",
    alloc_r_tariff_desc_ = "Tariff B",
    rate_cents.per.kwh_night = 11.0,
    rate_cents.per.kwh_day = 13.5,
    rate_cents.per.kwh_peak = 26.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "C",
    alloc_r_tariff_desc_ = "Tariff C",
    rate_cents.per.kwh_night = 10.0,
    rate_cents.per.kwh_day = 13.0,
    rate_cents.per.kwh_peak = 32.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "D",
    alloc_r_tariff_desc_ = "Tariff D",
    rate_cents.per.kwh_night = 9.0,
    rate_cents.per.kwh_day = 12.5,
    rate_cents.per.kwh_peak = 38.0
  )
)


# # 2. Modify the DT created above
# # 2.1. Add column(s)
# # 2.1.1. Add a column, in factor type, that shows tariff description
dt_tou[
  ,
  alloc_r_tariff_desc_in.factor := factor(
    alloc_r_tariff_desc,
    levels = c("Control", "Tariff A", "Tariff B", "Tariff C", "Tariff D")
  )
]


# ------------------------------------------------------------------------------
# Create and Save Plot(s)
# ------------------------------------------------------------------------------
# ------- Create ggplot Object(s) -------
# # 1. Plot for TOU Tariff Structures
plot_tou <-
  ggplot() +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    geom_step(
      data = dt_tou,
      aes(
        x = interval_hour,
        y = rate_cents.per.kwh,
        color = alloc_r_tariff_desc_in.factor
      ),
      lwd = 0.7
    ) +
    annotate(
      geom = "text",
      x = 3.5, y = 41, label = "Night",
      color = "grey", size = 4
    ) +
    annotate(
      geom = "text",
      x = 12.5, y = 41, label = "Day",
      color = "grey", size = 4
    ) +
    annotate(
      geom = "text",
      x = 18, y = 41, label = "Peak",
      color = "grey", size = 4
    ) +
    annotate(
      geom = "text",
      x = 24, y = 41, label = "Night",
      color = "grey", size = 4
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 1), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(5, 45, by = 5), labels = scales::comma) +
    scale_color_viridis_d() +
    labs(x = "", y = "Rate  (Cents per kWh)", color = "Tariffs") +
    theme_linedraw()


# ------- Save plot(s) created above in PNG format -------
# # 1. Plot(s) for TOU Tariff Structures
plot.save(
  paste(
    DIR_TO.SAVE_PLOT,
    "CER_Time-of-Use-Tariff-Structure.png",
    sep = "/"
  ),
  plot_tou,
  width = 40, height = 25, units = "cm"
)

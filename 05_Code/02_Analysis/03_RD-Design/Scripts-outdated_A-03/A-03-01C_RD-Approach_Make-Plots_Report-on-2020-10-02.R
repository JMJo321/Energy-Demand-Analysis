# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02C
# #
# > Purpose of the script(s)
# # : Create Plots.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(grid)
library(gridExtra)
library(ggplot2)
library(stargazer)
library(sandwich)
library(stringr)
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
# # 1. For making plots
# # 1.1. Set upper limit of daily average consumption
DAILY.AVG_UPPER.LIMIT <- 60
DAILY.AVG_IN.PERCENT_UPPER.LIMIT <- 200

# # 1.2. Set the number of observations randomly sampled
SAMPLE.SIZE <- 10^4 * 3


# ------- Define function(s) -------
# (Not Applicable)


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
# Generate ggplot objects
# --------------------------------------------------
# ------- Set plot options commonly used -------
plot.options <-
  list(
    theme_linedraw(),
    labs(
      x = paste0(
        "Normalized Consumption in Period 0 relative to Baseline Allowance (%)"
      ),
      color = "Treatment"
    ),
    theme(
      plot.title = element_text(face = "bold.italic", size = 17)
    )
  )


# ------- Generate ggplot objects -------
# # 1. By using Relative Monthly Consumption in Period 1 as the dependent
# #    variable
# # 1.1. Make a ggplot object: Parametric Fits with scatter points
plot_relative.monthly <-
  ggplot(
    dt_for.reg[sample(c(1:nrow(dt_for.reg)), SAMPLE.SIZE)]
  ) +
    geom_point(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_in.percent_period1 / 100
      ),
      alpha = 0.2
    ) +
    geom_smooth(
      data = dt_for.reg,
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_in.percent_period1 / 100,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    plot.options +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Consumption in Period 1 relative to that in Period 0 (%)",
      #title = "Changes in Electricity Consumption \n",
      subtitle = "Parametric Fits with (ramdomly sampled) scatter points"
    )

# # 1.2. Make a ggplot object: Parametric Fits only
plot_relative.monthly_zoom <-
  ggplot(dt_for.reg) +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_in.percent_period1 / 100,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    plot.options +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Consumption in Period 1 relative to that in Period 0 (%)",
      subtitle = "\nParametric Fits only"
    )

# # 1.3. Combine the two ggplot objects
plot_relative.monthly_combine <-
  grid.arrange(
    plot_relative.monthly, plot_relative.monthly_zoom,
    nrow = 2
    #,
    #bottom = textGrob(
    #  paste0(
    #    "\n Note: Parametric fits are generated from regressions that include ",
    #    "a constant, a treatment dummy, and a linear term for ",
    #    "normalized consumption in period 0. \n",
    #    "Only use observations whose monthly consumption in period 1 ",
    #    "relative to that in period 0 is less than or equal to 200%. \n"
    #  ),
    #  just = "right",
    #  gp = gpar(fontsize = 10, fontface = "italic")
    #)
  )


# # 2. By using Relative Daily Average Consumption in Period 1 as the dependent
# #    variable
# # 2.1. Make a ggplot object: Parametric Fits with scatter points
plot_relative.daily <-
  ggplot(
    dt_for.reg[
      relative.kwh_daily.avg_in.percent_period1 <=
        DAILY.AVG_IN.PERCENT_UPPER.LIMIT
    ][
      sample(c(1:nrow(dt_for.reg)), SAMPLE.SIZE)
    ]
  ) +
    geom_point(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_daily.avg_in.percent_period1 / 100
      ),
      alpha = 0.2
    ) +
    geom_smooth(
      data = dt_for.reg[
        relative.kwh_daily.avg_in.percent_period1 <=
          DAILY.AVG_IN.PERCENT_UPPER.LIMIT
      ],
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_daily.avg_in.percent_period1 / 100,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    plot.options +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = paste0(
        "Daily Average Consumption in Period 1",
        " relative to that in Period 0 (%)"
      ),
      #title = "Changes in Electricity Consumption \n",
      subtitle = "Parametric Fits with (ramdomly sampled) scatter points"
    )

# # 2.2. Make a ggplot object: Parametric Fits only
plot_relative.daily_zoom <-
  ggplot(
    dt_for.reg[
      relative.kwh_daily.avg_in.percent_period1 <=
        DAILY.AVG_IN.PERCENT_UPPER.LIMIT
    ]
  ) +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = relative.kwh_daily.avg_in.percent_period1 / 100,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    plot.options +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = paste0(
        "Daily Average Consumption in Period 1",
        " relative to that in Period 0 (%)"
      ),
      subtitle = "\nParametric Fits only"
    )

# # 2.3. Combine the two ggplot objects
plot_relative.daily_combine <-
  grid.arrange(
    plot_relative.daily, plot_relative.daily_zoom,
    nrow = 2
    #,
    #bottom = textGrob(
    #  paste0(
    #    "\n Note: Parametric fits are generated from regressions that include ",
    #    "a constant, a treatment dummy, and a linear term for ",
    #    "normalized monthly consumption in period 0. \n",
    #    "Only use observations whose daily average consumption in period 1 ",
    #    "is less than or equal to 200%. \n"
    #  ),
    #  just = "right",
    #  gp = gpar(fontsize = 10, fontface = "italic")
    #)
  )


# # 3. By using Daily Average Consumption in Period 1 as the dependent
# #    variable
# # 3.1. Make a ggplot object: Parametric Fits with scatter points
plot_daily <-
  ggplot(
    dt_for.reg[
      kwh_daily.avg <= DAILY.AVG_UPPER.LIMIT
    ][
      sample(c(1:nrow(dt_for.reg)), SAMPLE.SIZE)
    ]
  ) +
    geom_point(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg
      ),
      alpha = 0.2
    ) +
    geom_smooth(
      data = dt_for.reg[kwh_daily.avg <= DAILY.AVG_UPPER.LIMIT],
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    plot.options +
    scale_x_continuous(labels = scales::percent) +
    labs(
      y = "Daily Average Consumption in Period 1 (kWh/Day)",
      #title = "Changes in Electricity Consumption \n",
      subtitle = "Parametric Fits with (ramdomly sampled) scatter points"
    )

# # 3.2. Make a ggplot object: Parametric Fits only
plot_daily_zoom <-
  ggplot(dt_for.reg[kwh_daily.avg <= DAILY.AVG_UPPER.LIMIT]) +
    geom_smooth(
      aes(
        x = kwh_total_in.percent_normalize_period0 / 100,
        y = kwh_daily.avg,
        color = is_treated_total
      ),
      method = lm, formula = y ~ x
    ) +
    plot.options +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::percent) +
    labs(
      y = "Daily Average Consumption in Period 1 (kWh/Day)",
      subtitle = "\nParametric Fits only"
    )

# # 3.3. Combine the two ggplot objects
plot_daily_combine <-
  grid.arrange(
    plot_daily, plot_daily_zoom,
    nrow = 2
    #,
    #bottom = textGrob(
    #  paste0(
    #    "\n Note: Parametric fits are generated from regressions that include ",
    #    "a constant, a treatment dummy, and a linear term for ",
    #    "normalized monthly consumption in period 0. \n",
    #    "Only use observations whose daily average consumption in period 1 ",
    #    "is less than or equal to 60 kWh/Day. \n"
    #  ),
    #  just = "right",
    #  gp = gpar(fontsize = 10, fontface = "italic")
    #)
  )


# --------------------------------------------------
# Export Results
# --------------------------------------------------
# ------- Save plots in PNG format -------
# # 1. Plots using Relative Monthly Consumption
PLOT.NAME_RELATIVE.MONTHLY <-
  "SMUD-Billing-Data_RD-Approach_Relative-Monthly-Consumption.png"
PATH_PLOT_RELATIVE.MONTHLY <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_RELATIVE.MONTHLY, sep = "/")
plot.save(
  PATH_PLOT_RELATIVE.MONTHLY,
  plot_relative.monthly_combine,
  width = 30, height = 30, units = "cm"
)

# # 2. Plots using Relative Daily Average Consumption
PLOT.NAME_DAILY <-
  "SMUD-Billing-Data_RD-Approach_Relative-Daily-Average-Consumption.png"
PATH_PLOT_DAILY <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_DAILY, sep = "/")
plot.save(
  PATH_PLOT_DAILY,
  plot_relative.daily_combine,
  width = 30, height = 30, units = "cm"
)

# # 3. Plots using Daily Average Consumption
PLOT.NAME_DAILY <-
  "SMUD-Billing-Data_RD-Approach_Daily-Average-Consumption.png"
PATH_PLOT_DAILY <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_DAILY, sep = "/")
plot.save(
  PATH_PLOT_DAILY,
  plot_daily_combine,
  width = 30, height = 30, units = "cm"
)

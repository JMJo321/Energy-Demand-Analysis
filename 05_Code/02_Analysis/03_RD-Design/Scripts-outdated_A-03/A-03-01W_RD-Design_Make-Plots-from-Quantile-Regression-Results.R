# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02W
# #
# > Purpose of the script(s)
# # : To

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(unikn)
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
FILE_TO.LOAD_RD_RESULTS <-
  "DT_For-Regression_RD-Design_Quantile-Regression-Results.parquet"
PATH_TO.LOAD_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD_RESULTS,
  sep = "/"
)

# # 2. Paths at which Output will be saved
# # 2.1. Path at which Plot(s) will be saved
DIR_TO.SAVE_PLOTS <- paste(
  PATH_NOTE_DESCRIPTIVE.ANALYSIS, "02_RD-Design/02_Plots", sep = "/"
)


## ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load and Modify Regression Results
# --------------------------------------------------
# ------- Load Regression Results -------
dt_for.plot <-
  pq.to.dt(
    PATH_TO.LOAD_RD_RESULTS,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )


dt_for.plot[, tau_in.factor := factor(tau, levels = (seq(0.9, 0.1, by = -0.1) %>% as.character(.)))]


# --------------------------------------------------
# Generate ggplot object(s) and export it as PNG format
# --------------------------------------------------





# ------- Set Plot Options -------
color.palette <-
  unikn::usecol(pal = pal_unikn, n = dt_for.plot[, .N, by = .(bw_in.str)][, .N])
plot.options <-
  list(
    theme_linedraw(),
    facet_grid(var_independent ~ model, scales = "free_y"),
    scale_color_brewer(palette = "Spectral"),
    #scale_color_manual(values = color.palette),
    scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1), labels = scales::percent_format(accuracy = 1)),
    scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)),
    labs(
      x = "Bandwidths",
      y = "Point Estimates",
      color = expression("\u03C4")
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Generate ggplot Object(s)  -------
plot <-
  ggplot(
    dt_for.plot[
      str_detect(var_independent, "Constant", negate = TRUE)
    ][
      rate.code == "RSGH" & season == "Summer"
    ],
    aes(x = bw_in.percent/100, y = estimates, color = tau_in.factor)
  ) +
    geom_line() +
    geom_point() +
    plot.options




plot.save(
  paste(DIR_TO.SAVE_PLOTS, "Regression-Results_RD-Design_Quantile-Regressions", sep = "/"),
  plot,
  width = 30, height = 35, units = "cm"
)



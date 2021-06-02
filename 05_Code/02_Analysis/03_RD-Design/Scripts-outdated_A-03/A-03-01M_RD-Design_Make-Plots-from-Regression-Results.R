# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02M
# #
# > Purpose of the script(s)
# # : To generate plots based on regression results that are obtained from
# #   unrestricted sample.

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
DIR_TO.LOAD_RD_RESULTS <- "01_RD-Design"
FILE_TO.LOAD_RD_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_Unrestricted-Samples.parquet"
PATH_TO.LOAD_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD_RESULTS, FILE_TO.LOAD_RD_RESULTS,
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
# Load Regression Results
# --------------------------------------------------
# ------- Load Regression Results -------
# # 1. Regression Results for Linear Models
dt_rd_results <-
  pq.to.dt(
    PATH_TO.LOAD_RD_RESULTS,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )


# --------------------------------------------------
# Create DTs that will be used later
# --------------------------------------------------
# ------- Create a DT containing Regression Results: Linear Models -------
# # 1. Add columns showing 95% confidence interval
z_95p <- qnorm(1 - 0.05 / 2, mean = 0, sd = 1)
dt_rd_results[
  ,
  `:=` (
    ci_lower = estimates - se_max * z_95p,
    ci_upper = estimates + se_max * z_95p
  )
]

# # 2. Generate a DT containing the estimated treatment effects only
dt_for.plot <- dt_rd_results[var_independent == "1[Treated]"]

# # 3. Modify the DT created
# # 3.1. Generate a data field
dt_for.plot[str_detect(model, "Interaction"), category := "With Interaction"]
dt_for.plot[is.na(category), category := "Without Interaction"]
dt_for.plot[
  ,
  category := factor(
    category,
    levels = c("Without Interaction", "With Interaction"),
    ordered = TRUE
  )
]
# # 3.2. Change values of a data field: "model"
dt_for.plot[, model := str_replace(model, "Interaction w", "W")]
levels_model <- c(
  "Without FEs and Controls", "Without FEs", "With IDs FEs", "With BYM FEs",
  "With Both FEs"
)
dt_for.plot[
  , model := factor(model, levels = levels_model, ordered = TRUE)
]


# --------------------------------------------------
# Generate ggplot objects
# --------------------------------------------------
# ------- Set Plot Options commonly used -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_linedraw(),
    facet_grid(model ~ category),
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent),
    scale_y_continuous(
      breaks = seq(-1, 1, by = 0.2),
      limits = c(-0.7, 0.5),
      labels = scales::comma
    ),
    scale_color_viridis_d(),
    scale_fill_viridis_d(),
    labs(
      x = "Bandwidth",
      y = "Estimated Treatment Effects",
      color = "Functional\nForms",
      fill = "Functional\nForms"
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Generate ggplot options -------
plot_estimates_all <-
  ggplot(dt_for.plot) +
    geom_ribbon(
      aes(
        x = bw_in.percent / 100, ymin = ci_lower, ymax = ci_upper,
        fill = functional.form
      ),
      alpha = 0.4
    ) +
    geom_line(
      aes(x = bw_in.percent / 100, y = estimates, color = functional.form)
    ) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = "black") +
    plot.options


# --------------------------------------------------
# Export Plots
# --------------------------------------------------
# ------- Export Plots in PNG format -------
PLOT.NAME_ALL <-
  "Regression-Results_RD-Design_Treatment-Effect-by-BW_All-Models.png"
PATH_PLOT_ALL <-
  paste(DIR_TO.SAVE_PLOTS, PLOT.NAME_ALL, sep = "/")
plot.save(
  PATH_PLOT_ALL,
  plot_estimates_all,
  width = 30, height = 30, units = "cm"
)


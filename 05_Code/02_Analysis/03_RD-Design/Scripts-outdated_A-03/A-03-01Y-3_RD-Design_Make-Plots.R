# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02Y-2
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
FILE_TO.LOAD_RD_RESULTS <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods_NOT-by-Month_Tier-2.parquet"
PATH_TO.LOAD_RD_RESULTS <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD_RESULTS,
  sep = "/"
)

# # 2. Paths at which Output will be saved
DIR_TO.SAVE_PLOTS <- paste(
 PATH_NOTE_DESCRIPTIVE.ANALYSIS, "02_RD-Design/02_Plots", sep = "/"
)


# ------- Define parameter(s) -------
# # 1. To set a range of years
YEAR_UPPER <- 2011
YEAR_LOWER <- 2005


# ------- Define function(s) -------
# (NOT Applicable)

# --------------------------------------------------
# X
# --------------------------------------------------
# ------- X -------
dt_for.plot <- pq.to.dt(
  PATH_TO.LOAD_RD_RESULTS,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)


# --------------------------------------------------
# X
# --------------------------------------------------
# ------- X -------
z_95p <- qnorm(1 - 0.05 / 2, mean = 0, sd = 1)
dt_for.plot[
  ,
  `:=` (
    ci_lower = estimates - se_max * z_95p,
    ci_upper = estimates + se_max * z_95p
  )
]

dt_for.plot[, term_btw.periods := factor(term_btw.periods)]

plot_test <-
  ggplot(
    dt_for.plot[
      str_detect(var_independent, "^1\\[Treated\\]") &
        category == "Backward" &
        functional.form == "Linear" &
        model == "Interaction with BYM FEs"
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
    geom_errorbar(width = 0.5, position = position_dodge(width = 0.7)) +
    scale_color_viridis_d() +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold")) +
    labs(
      x = "Month for which Treatment Effects are estimated",
      y = "Estimated Treatment Effects",
      color = "Term(s)\nbetween\nPeriods",
      subtitle = "Backward Cases with Linear Term"
    )

plot.save(
  paste(
    DIR_TO.SAVE_PLOTS,
    "Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Linear_With-Interaction_Backward_Season-Level_Tier-2.png",
    sep = "/"
  ),
  plot_test,
  width = 50, height = 25, units = "cm"
)

# --------------------------------------------------
# X
# --------------------------------------------------
# ------- X -------

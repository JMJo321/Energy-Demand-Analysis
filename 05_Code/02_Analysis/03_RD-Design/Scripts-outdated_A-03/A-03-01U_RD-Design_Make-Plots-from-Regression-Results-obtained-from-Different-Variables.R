# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02U
# #
# > Purpose of the script(s)
# # : To generate a plot based on regression results obtained from regressions
# #   with different dependent/running variables.

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
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Term-between-Periods.parquet"
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


# ------- Modify the DT loaded -------
# # 1. Add columns showing lower and upper bounds of 95% Confidence Interval
z_95p <- qnorm(1 - 0.05 / 2, mean = 0, sd = 1)
dt_for.plot[
  ,
  `:=` (
    ci_lower = estimates - se_max * z_95p,
    ci_upper = estimates + se_max * z_95p
  )
]

# # 2. Add a column indicating abbreviation of months
dt_for.plot[
  ,
  month_response_in.str :=
    factor(month.abb[month_response], levels = month.abb)
]


# --------------------------------------------------
# Generate ggplot object(s) and export it as PNG format
# --------------------------------------------------
# ------- Set Plot Options -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_linedraw(),
    facet_grid(bw_in.str ~ model),
    #scale_color_manual(values = c("#E78070", "#59BAC4")),
    #scale_fill_manual(values = c("#E78070", "#59BAC4")),
    #scale_color_grey(start = 0.6, end = 0.1),
    #scale_fill_grey(start = 0.6, end = 0.1),
    labs(
      x = "Month for which Treatment Effects are estimated",
      y = "Estimated Treatment Effects",
      color = "Term(s)\nbetween\nPeriods"
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Generate ggplot Object(s)  -------
functional.forms <- dt_for.plot[, .N, by = .(functional.form)]$functional.form
models <- dt_for.plot[, .N, by = .(model)]$model
rate.codes <- dt_for.plot[, .N, by = .(rate.code)]$rate.code
categories <- dt_for.plot[, .N, by = .(category)]$category
combinations <-
  expand.grid(functional.forms, models, rate.codes, categories) %>% setDT(.)
names(combinations) <- c("functional.form", "model", "rate.code", "category")
for (row in 1:combinations[, .N]) {
  if (combinations[row]$functional.form == "Linear") {
    tmp_obj.name_functional.form <- "linear"
    tmp_file.name_functional.form <- "Linear"
  } else if (combinations[row]$functional.form == "Square") {
    tmp_obj.name_functional.form <- "sqaure"
    tmp_file.name_functional.form <- "Square"
  }
  if (combinations[row]$model == "With BYM FEs") {
    tmp_obj.name_model <- "wo.interaction"
    tmp_file.name_model <- "Without-Interaction"
  } else {
    tmp_obj.name_model <- "w.interaction"
    tmp_file.name_model <- "With-Interaction"
  }
  if (combinations[row]$rate.code == "RSGH") {
    tmp_obj.name_rate.code <- "g.code"
    tmp_file.name_rate.code <- "RSGH"
  } else {
    tmp_obj.name_rate.code <- "ce.codes"
    tmp_file.name_rate.code <- "RSCH-and-RSEH"
  }
  tmp_obj.name <- paste(
    "plot_estimates",
    tmp_obj.name_functional.form,
    tmp_obj.name_model,
    tmp_obj.name_rate.code,
    tolower(combinations[row]$category),
    sep = "_"
  )
  if (
    combinations[row]$functional.form == "Linear" &
      combinations[row]$rate.code == "RSGH"
  ) {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-0.8, 0.8, by = 0.2),
        limits = c(-0.7, 0.75),
        labels = scales::comma_format(accuracy = 0.1)
      )
  } else if (
    combinations[row]$functional.form == "Linear" &
      combinations[row]$rate.code == "RSCH, RSEH"
  ) {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-2.0, 1.0, by = 0.5),
        limits = c(-2.0, 1.0),
        labels = scales::comma_format(accuracy = 0.1)
      )
  } else if (
    combinations[row]$functional.form == "Square" &
      combinations[row]$rate.code == "RSGH"
  ) {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-1.0, 0.8, by = 0.2),
        limits = c(-0.9, 0.8),
        labels = scales::comma_format(accuracy = 0.1)
      )
  } else if (
    combinations[row]$functional.form == "Square" &
      combinations[row]$rate.code == "RSCH, RSEH"
  ) {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-2.0, 2.0, by = 0.5),
        limits = c(-2.0, 1.6),
        labels = scales::comma_format(accuracy = 0.1)
      )
  }

  assign(
    tmp_obj.name,
    ggplot(
      dt_for.plot[
        str_detect(var_independent, "^1\\[Treated\\]") &
          functional.form == combinations[row]$functional.form &
          model == combinations[row]$model &
          rate.code == combinations[row]$rate.code &
          category == combinations[row]$category
          # ## Note:
          # ## Adding an indicator variable related to change in season
          # ## makes ingorable changes only.
      ]
    ) +
      geom_point(
        aes(
          x = month_response_in.str,
          y = estimates,
          color = (
              if (combinations[row]$category == "Forward") {
                factor(term_btw.periods)
              } else {
                factor(term_btw.periods, levels = c(5:1))
              }
          )
        ),
        position = position_dodge(width = 0.4)
      ) +
      geom_hline(
        yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7
      ) +
      #geom_vline(
      #  xintercept = c("May", "Oct"), linetype = "dotted", color = "#E78070"
      #) +
      geom_errorbar(
        aes(
          x = month_response_in.str,
          y = estimates,
          ymin = ci_lower, ymax = ci_upper,
          color = (
              if (combinations[row]$category == "Forward") {
                factor(term_btw.periods)
              } else {
                factor(term_btw.periods, levels = c(5:1))
              }
          )
        ),
        width = 0.3,
        position = position_dodge(width = 0.4)
      ) +
      plot.options +
      range_y.axis +
      labs(
        subtitle = paste0(
          combinations[row]$category,
          " Cases: ", str_replace(combinations[row]$rate.code, ", ", " & "),
          ", with ", combinations[row]$functional.form, " Term"
        )
      ) +
      (
        if (combinations[row]$category == "Forward") {
          scale_color_viridis_d()
        } else {
          scale_color_viridis_d(direction = -1)
        }
      ) +
      (
          if (combinations[row]$category == "Forward") {
            scale_fill_viridis_d()
          } else {
            scale_fill_viridis_d(direction = -1)
          }
      )
  )

  tmp_file.name <- paste(
    "Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods",
    tmp_file.name_functional.form,
    tmp_file.name_model,
    tmp_file.name_rate.code,
    combinations[row]$category,
    sep = "_"
  ) %>% paste0(., ".png")
  tmp_path <- paste(DIR_TO.SAVE_PLOTS, tmp_file.name, sep = "/")
  plot.save(tmp_path, get(tmp_obj.name), width = 30, height = 35, units = "cm")
}


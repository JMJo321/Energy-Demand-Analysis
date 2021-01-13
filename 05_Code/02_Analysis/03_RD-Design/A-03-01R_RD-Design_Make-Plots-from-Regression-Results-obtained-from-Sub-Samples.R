# < Description >
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02R
# #
# > Purpose of the script(s)
# # : To generate a plot based on regression results obtained from sub-samples.

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
FILE_TO.LOAD_RD_LINEAR <-
  "DT_For-Regression_RD-Design_Regression-Results_FELM_By-Rate-Codes.parquet"
PATH_TO.LOAD_RD_LINEAR <- paste(
  PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD_LINEAR,
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
    PATH_TO.LOAD_RD_LINEAR,
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


# --------------------------------------------------
# Generate ggplot object(s)
# --------------------------------------------------
# ------- Set Plot Options -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <-
  list(
    theme_linedraw(),
    facet_grid(model ~ season),
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent),
    scale_color_manual(values = c("#E78070", "#59BAC4")),
    scale_fill_manual(values = c("#E78070", "#59BAC4")),
    #scale_color_viridis_d(),
    #scale_fill_viridis_d(),
    #scale_color_grey(start = 0.6, end = 0.1),
    #scale_fill_grey(start = 0.6, end = 0.1),
    labs(
      x = "Bandwidth",
      y = "Estimated Treatment Effects",
      color = "Rate Codes",
      fill = "Rate Codes"
    ),
    theme(strip.text = element_text(face = "bold"))
  )


# ------- Generate ggplot Object(s)  -------
functional.forms <- c("Linear", "Square", "Cubic")
for (form in functional.forms) {
  tmp_obj.name <- paste0("plot_estimates_", tolower(form))
  if (form == "Linear") {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-0.6, 0.6, by = 0.2),
        limits = c(-0.5, 0.6),
        labels = scales::comma
      )
  } else if (form == "Square") {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-0.8, 0.6, by = 0.2),
        limits = c(-0.7, 0.6),
        labels = scales::comma
      )
  } else {
    range_y.axis <-
      scale_y_continuous(
        breaks = seq(-1.4, 0.6, by = 0.2),
        limits = c(-1.3, 0.6),
        labels = scales::comma
      )
  }

  assign(
    tmp_obj.name,
    ggplot(
      data = dt_for.plot[
        var_independent == "1[Treated]" &
          !is.na(bw_in.percent) &
          functional.form == form
      ]
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_line(
          aes(x = bw_in.percent / 100, y = estimates, color = rate.codes)
      ) +
      geom_ribbon(
          aes(
            x = bw_in.percent / 100,
            ymin = ci_lower,
            ymax = ci_upper,
            fill = rate.codes
        ),
          alpha = 0.3
      ) +
      plot.options +
      range_y.axis
  )
}


# --------------------------------------------------
# Export Plots created
# --------------------------------------------------
# ------- Export Plots in PNG format -------
for (form in functional.forms) {
  tmp_obj.name <- paste0("plot_estimates_", tolower(form))
  tmp_file.name <- paste0(
    "Regression-Results_RD-Design_Treatment-Effect-by-BW-and-Rate-Codes_",
    form,
    ".png"
  )
  tmp_path <-
    paste(DIR_TO.SAVE_PLOTS, tmp_file.name, sep = "/")
  plot.save(tmp_path, get(tmp_obj.name), width = 35, height = 20, units = "cm")
}

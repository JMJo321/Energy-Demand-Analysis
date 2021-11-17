# Description
# : A Header Script for Energy Demand Analysis Projects - CER Trial

# ------------------------------------------------------------------------------
# To load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)

# ------------------------------------------------------------------------------
# To clear the worksapce
# ------------------------------------------------------------------------------
rm(list = setdiff(ls(), "PATH_PROJ"))


# ------------------------------------------------------------------------------
# To set working directory
# ------------------------------------------------------------------------------
# ------- To set working directory -------
# (In each script)


# ------------------------------------------------------------------------------
# To define parameter(s)
# ------------------------------------------------------------------------------
# ------- Define Path-associated Parameters ------
# # 1. Define folder names
# # 1.1. For "03_Note"
PATH_NOTE <- "03_Note"

# # 1.2. For "04_Data"
PATH_DATA <- "04_Data"
PATH_DATA_RAW <- paste(PATH_DATA, "01_Raw-Data", sep = "/")
PATH_DATA_RAW_ORIGINAL <- paste(PATH_DATA_RAW, "01_Original", sep = "/")
PATH_DATA_RAW_USE <- paste(PATH_DATA_RAW, "02_Use", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_Intermediate-Data", sep = "/")
PATH_DATA_ANALYSIS <- paste(PATH_DATA, "03_For-Analysis", sep = "/")

# # 1.3. For "05_Code"
PATH_CODE <- "05_Code"
PATH_CODE_BUILD <- paste(PATH_CODE, "01_Build", sep = "/")
PATH_CODE_ANALYSIS <- paste(PATH_CODE, "02_Analysis", sep = "/")


# ------- Generate the foldes defined above -------
list_path <- c(
  PATH_NOTE,
  PATH_DATA,
  PATH_DATA_RAW,
  PATH_DATA_RAW_ORIGINAL,
  PATH_DATA_RAW_USE,
  PATH_DATA_INTERMEDIATE,
  PATH_DATA_ANALYSIS,
  PATH_CODE,
  PATH_CODE_BUILD,
  PATH_CODE_ANALYSIS
)
for (path in list_path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}


# ------------------------------------------------------------------------------
# To define function(s)
# ------------------------------------------------------------------------------
# ------- Change terms in a formula -------
# ## Note:
# ## Formulas are defined by using `hdd`. But exact variable names are
# ## `hdd_all`, `hdd_extremes`, and `hdd_soil`.
change_terms.in.formula <-
  function (
    formula, is_terms.in.dep.var = FALSE, term_old_in.str, term_new_in.str
  ) {
    formula_in.str <- as.character(formula)
    if (is_terms.in.dep.var == TRUE) {
      formula_in.str[2] <-
        stringr::str_replace(
          formula_in.str[2], term_old_in.str, term_new_in.str
        )
    } else {
      formula_in.str[3] <-
        stringr::str_replace_all(
          formula_in.str[3], term_old_in.str, term_new_in.str
        )
    }
    formula_modified <-
      paste(formula_in.str[2], formula_in.str[3], sep = " ~ ") %>%
        as.formula(.)
    return (formula_modified)
  }


# ------- Export a ggplot object as PNG format -------
export_plot_in.png <- function(
  path.w.file.name, plot.obj, width = NULL, height = NULL, units = "cm"
) {
  ggplot2::ggsave(
    filename = path.w.file.name,
    plot = plot.obj,
    dpi = 320,
    device = "png",
    width = width,
    height = height,
    units = units,
    limitsize = FALSE
  )
}


# ------- Extract estimates from a felm object -------
get_estimates.from.felm <- function (
  felm.object,
  level = 0.95, # The confidence level to use for the confidence interval
  fe = FALSE, # Logical indicating whether or not to include estimates of FEs
  se.type = "robust" # One of "default", "iid", "robust", or "cluster"
) {
  dt_estimates <-
    broom::tidy( # Refer to the document for broom::tidy.felm
      felm.object,
      conf.int = TRUE, conf.level = level,
      fe = fe,
      se.type = se.type
    ) %>%
      data.table::setDT(.)
  return (dt_estimates)
}


# ------- Get formulas for `felm` -------
get_felm.formula <-
  function (
    dep.var,
    indep.var_covariates, indep.var_fes, indep.var_ivs, indep.var_clustered.ses
  ) {
    indep.var <- paste(
      indep.var_covariates,
      indep.var_fes,
      indep.var_ivs,
      indep.var_clustered.ses,
      sep = " | "
    )
    formula_in.str <- paste(dep.var, indep.var, sep = "~ ")
    return(formula(formula_in.str))
}

# ------- Run regressions by using `lapply` function -------
get_reg.result <- function(season.and.rate.period, formula) {
  reg.result <- lfe::felm(
    data = dt_for.reg[
      eval(parse(text = get_condition.in.str(season.and.rate.period)))
    ],
    formula = formula
  )
  return (reg.result)
}


# ------- Label a data.table's columns -------
label_data.fields <- function(
  dt_in.str, data.field_to.label_in.str, list_labels
) {
  tmp_obj.name <- paste(dt_in.str, data.field_to.label_in.str, sep = "$")
  data.table::setattr(
    eval(parse(text = tmp_obj.name)),
    name = "label",
    value = list_labels[[data.field_to.label_in.str]]
  )
}

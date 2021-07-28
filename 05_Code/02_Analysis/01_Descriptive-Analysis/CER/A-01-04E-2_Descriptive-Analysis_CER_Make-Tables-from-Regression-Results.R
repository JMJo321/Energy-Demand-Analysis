# < Description > *
# > Script Group Indicator Number and Name:
# # A-01, Descriptive Analysis
# #
# > Script Number(s):
# # A-01-04E-2
# #
# > Purpose of the script(s):
# # Descriptive Analysis - Make Tables from Regression Results with Hourly Data

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(stargazer)
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
# # 1.1. For Regression Results
DIR_TO.LOAD_CER <- "04_CER"
FILE_TO.LOAD_CER_RESULTS <- "CER_Regression-Results_Rate-Period.RData"
PATH_TO.LOAD_CER_RESULTS <- paste(
  PATH_DATA_ANALYSIS,
  DIR_TO.LOAD_CER,
  FILE_TO.LOAD_CER_RESULTS,
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load Dataset(s) and/or Script(s)
# ------------------------------------------------------------------------------
# ------- Load Dataset(s) -------
load(PATH_TO.LOAD_CER_RESULTS)


# ------------------------------------------------------------------------------
# Create DTs from Regression Results
# ------------------------------------------------------------------------------
# ------- Create DTs from Regression Results with Hourly Data -------
results <- ls()[str_detect(ls(), "^result_")]
rate.periods <- dt_for.reg[, .N, by = .(rate.period)]$rate.period
for (result in results) {
  for (period in rate.periods) {
    obj.name <- match(period, rate.periods) %>% paste0("dt_result_", .)
    reg.exp_base <-
      as.character(period) %>%
        str_replace_all(., "\\(", "\\\\(") %>%
        str_replace_all(., "\\)", "\\\\)") %>%
        str_replace_all(., "\\-", "\\\\-")
    reg.exp <- paste0(
      "(",
      paste0("rate.period", reg.exp_base, ":"),
      ")|(",
      paste0(":rate.period", reg.exp_base),
      ")|(",
      "Intercept",
      ")"
    )

    assign(obj.name, get(result))
    row.names <- get(obj.name)$coefficients %>% rownames(.)
    names_rename <- row.names %>% str_replace(., reg.exp, "")

    coefficients_ <- get(obj.name)$coefficients
    rownames(coefficients_) <- names_rename
    assign(
      obj.name,
      c(
        rlist::list.remove(get(obj.name), "coefficients"),
        list(coefficients = coefficients_)
      )
    )
    beta_ <- get(obj.name)$beta
    rownames(beta_) <- names_rename
    assign(
      obj.name,
      c(
        rlist::list.remove(get(obj.name), "beta"),
        list(beta = beta_)
      )
    )
    se_ <- get(obj.name)$se
    names(se_) <- names_rename
    assign(
      obj.name,
      c(
        rlist::list.remove(get(obj.name), "se"),
        list(se = se_)
      )
    )
  }
  dts <- ls()[str_detect(ls(), "^dt_result_")]
  list_results <- mget(dts) %>% as.list(.)
  for (idx in 1:length(list_results)) {
    element <- list_results[[idx]]
    class(element) <- "felm"
    list_results[[idx]] <- element
  }
  list.name <- paste0("list_", result)
  assign(list.name, copy(list_results))
}
gc(reset = TRUE, full = TRUE)


# ------------------------------------------------------------------------------
# Make Table(s) from Regression Results
# ------------------------------------------------------------------------------
# ------- Make a Table from Regression Results with FEs Model -------
# # 1. Make objects that will be used to make a table
column.labels_base <-
  as.character(rate.periods) %>%
    str_extract(., "\\(.+?\\)") %>%
    str_replace_all(., "(\\()|(\\))", "")
column.labels_by.rate.period <- c(
  paste0(column.labels_base, " in Jul-Oct"),
  paste0(column.labels_base, " in Nov-Dec")
)
covariate.labels_by.rate.period_fes <- c(
  "HDs",
  "(HDs)\\^2",
  "1[Treatment]",
  "1[Post]",
  "1[Treatment and Post]",
  "HDs x 1[Treatment]",
  "(HDs)\\^2 x 1[Treatment]",
  "HDs x 1[Post]",
  "(HDs)\\^2 x 1[Post]",
  "HDs x 1[Treatment and Post]",
  "(HDs)\\^2 x 1[Treatment and Post]"
)
covariate.labels_by.rate.period_ols <- c(
  "(Constant)",
  covariate.labels_by.rate.period_fes
)
add.lines_by.rate.period <- list(
  c("FEs: ID-by-Day of Week", rep("Yes", 8 * 2)),
  c("FEs: Month of Year", rep("Yes", 8 * 2))
)
covariate_keep_by.rate.period <-
  row.names[str_detect(row.names, reg.exp)] %>%
    str_replace(., reg.exp, "") %>%
    str_replace_all(., "\\(", "\\\\(") %>%
    str_replace_all(., "\\)", "\\\\)") %>%
    str_replace_all(., "\\^", "\\\\^") %>%
    str_replace_all(., "\\.", "\\\\.") %>%
    paste0("^", .) %>%
    paste0(., "$") %>%
    c("(Intercept)", .)


# # 2. Print a Table
stargazer(
  list_result_fes_hourly_real.kwh_warm,
  list_result_fes_hourly_real.kwh_cold,
  type = "text",
  column.labels = column.labels_by.rate.period,
  covariate.labels = covariate.labels_by.rate.period_fes,
  dep.var.labels = "Hourly Consumption (kWh per Hour)",
  add.lines = add.lines_by.rate.period,
  keep = covariate_keep_by.rate.period
)


# ------- Make a Table from Regression Results -------
# ## Note:
# ## Only to show coefficients on "treatment.and.post"-related terms.

# # 1. Create objects that will be used to make regression table(s)
list_results_treatment.and.post <- list(
  result_ols_hourly_real.kwh_warm,
  result_ols_hourly_real.kwh_cold,
  result_fes_hourly_real.kwh_warm,
  result_fes_hourly_real.kwh_cold
)
column.labels_treatment.and.post <- c("OLS", "FEs")
covariate.labels_treatment.and.post <- lapply(
  rate.periods,
  function (x) c(
    paste("1[Treatment and Post]", x, sep = " for "),
    paste("HDs x 1[Treatment and Post]", x, sep = " for "),
    paste("(HDs)\\^2 x 1[Treatment and Post]", x, sep = " for ")
  )
) %>%
  unlist(.)
add.lines_treatment.and.post <- list(
  c("Season", c("Jul. - Oct.", "Nov. - Dec.", "Jul. - Oct.", "Nov. - Dec.")),
  c("FEs: ID-by-Day of Week", c(rep("No", 2), rep("Yes", 2))),
  c("FEs: Month of Year", c(rep("No", 2), rep("Yes", 2)))
)
reg.exp_treatment.and.post <- "treatment\\.and\\.post"
reg.exp_order <-
  as.character(rate.periods) %>%
    str_extract(., "\\(.+\\)") %>%
    str_replace_all(., "\\(", "\\\\(") %>%
    str_replace_all(., "\\)", "\\\\)") %>%
    str_replace_all(., "\\-", "\\\\-")
idx_order <- lapply(
  seq(1, 8, by = 1), function (x) seq(x, x  + (8 * 2), by = 8)
) %>%
  unlist(.)


# # 2. Print Table(s)
stargazer(
  list_results_treatment.and.post,
  type = "text",
  column.labels = column.labels_treatment.and.post,
  column.separate = c(2, 2),
  covariate.labels = covariate.labels_treatment.and.post,
  dep.var.labels = "Hourly Consumption (kWh per Hour)",
  add.lines = add.lines_treatment.and.post,
  keep = reg.exp_treatment.and.post,
  order = reg.exp_order
)

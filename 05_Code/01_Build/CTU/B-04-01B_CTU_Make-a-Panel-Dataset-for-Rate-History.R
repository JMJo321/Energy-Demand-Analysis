# < Description > *
# > Script Group Indicator Number and Name
# # : B-04, CTU
# #
# > Script Number(s)
# # : B-04-01B
# #
# > Purpose of the script(s)
# # : Create a Panel Dataset for Residential Rate History Data of City of
# #   Tallahassee Utilities from .RData file generated from B-04-01A.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
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
# # 1. Path(s) for data file(s) that will be imported
# # 1.1. .RData File(s)
DIR_TO.LOAD_HISTORY <- "CTU/Rate-History"
FILE_TO.LOAD_HISTORY <- "CTU_Residential-Rate-History.RData"
PATH_TO.LOAD_HISTORY <- paste(
  PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_HISTORY, FILE_TO.LOAD_HISTORY, sep = "/"
)

# # 1.2. Script(s)
FILE_TO.LOAD_LABELS <- "D-Energy-Demand-Analysis_Data-Dictionary_CTU-only.R"
PATH_TO.LOAD_LABELS <- paste(PATH_CODE, FILE_TO.LOAD_LABELS, sep = "/")


# # 2. Path(s) at which DT(s) will be saved
DIR_TO.SAVE_HISTORY <- DIR_TO.LOAD_HISTORY
FILE_TO.SAVE_HISTORY <- "CTU_Residential-Rate-History_Panel.RData"
PATH_TO.SAVE_HISTORY <- paste(
  PATH_DATA_INTERMEDIATE, DIR_TO.SAVE_HISTORY, FILE_TO.SAVE_HISTORY, sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Label columns in a DT
label.cols <- function(dt, data.dictionary) {
    # # To generate a list containing labels
    tmp_list_labels <- list(NULL)
    for (col in names(dt)) {
        tmp_list_labels[[col]] <- data.dictionary[[col]]
    }
    tmp_list_labels[1] <- NULL

    # # To label columns of a data.table
    mapply(setattr, dt, name= "label", tmp_list_labels, SIMPLIFY= FALSE)
}


# --------------------------------------------------
# Load necessary data
# --------------------------------------------------
# ------- Load Rate History Data -------
load(PATH_TO.LOAD_HISTORY)

# ------- Load Labels for Data Fields -------
source(PATH_TO.LOAD_LABELS)


# --------------------------------------------------
# Make a Panel Dataset for Rate History
# --------------------------------------------------
# ------- Make a Panel Dataset for Rate History -------
# # 1. Make a DT containing panel data
# # 1.1. Create objects that are used later
cols_extract <- names(rate.history)[
  str_detect(names(rate.history), "date", negate = TRUE)
]
cols_common <- c(
  "utility", "rate_category", "rate_type", "rate_item", "base_item"
)
cols_by <- c("date", cols_common)
# ## Note: cols_* are exploited for choosing columns or loopings.
select <- "tmp_date_begin <= date & date <= tmp_date_end"
select_negate <- paste0("!(", select, ")")
# ## Note: select and select_negate are utilized to subset a temporary DT.
date_begin <- min(rate.history$date_from)
date_end <- max(rate.history$date_to)
# ## Note: date_* are used to create a empty panel template.

# # 1.2. Make a temporary panel dataset
n_rows <- rate.history[, .N]
tmp_dt_panel <- setDT(NULL)
for (i in 1:n_rows) {
  # ## Create objects that are used later
  tmp_date_begin <- min(rate.history[i]$date_from)
  tmp_date_end <- max(rate.history[i]$date_to)

  # ## Create an empty panel template
  tmp_dt <- data.table(
    date = seq.Date(from = date_begin, to = date_end, by = "day")
  )

  # ## Update values of columns
  # ### For dates falling in a given range
  for (col in cols_extract) {
    tmp_dt[
      eval(parse(text = select)), (col) := rate.history[i, get(col)]
    ]
  }
  # ### For dates not falling in a given range
  for (col in cols_common) {
    tmp_dt[
      eval(parse(text = select_negate)), (col) := rate.history[i, get(col)]
    ]
  }

  # ## Add the temporary DT created above
  tmp_dt_panel <- rbind(tmp_dt_panel, tmp_dt)
}

# # 1.3. Modify the temporary panel dataset
# # 1.3.1. Drop dupliced rows
tmp_dt_panel_unique <- unique(tmp_dt_panel)
# # 1.3.2. Create a DT by dropping Unnecessary rows
# ## Note: There are, for several dates, rows that have the same information
# ## for cols_common but have no information for remaining columns.
rate.history_panel <- tmp_dt_panel_unique[
  !(is.na(percent) & is.na(rate_in_usd))
]
# # 1.3.3. Reorder columns
cols_reorder <- c(
  "utility", "rate_category", "rate_type", "rate_item", "base_item", "unit",
  "date"
)
setcolorder(rate.history_panel, cols_reorder)


# # 2. Conduct simple tests
rate.history_panel[, .N, by = .(date)][, .N, by = .(N)]
# ## Note: This result implies that several rate items exist only for specific
# ## time period.

# ## 2.1. Check the primary keys of the panel dataset
keys <- c("utility", "rate_category", "rate_item", "base_item", "date")
stopifnot(rate.history_panel[, .N, by = keys][N > 1] == 0)


# # 3. Modify the panel dataset
# # 3.1. Reorder rows
setkeyv(rate.history_panel, keys)

# # 3.2. Label data fields
label.cols(rate.history_panel, labels_ctu)


# --------------------------------------------------
# Save the DT containing Panel Data
# --------------------------------------------------
# ------- Save the DT containing Panel Data in .RData format -------
save(rate.history_panel, file = PATH_TO.SAVE_HISTORY)


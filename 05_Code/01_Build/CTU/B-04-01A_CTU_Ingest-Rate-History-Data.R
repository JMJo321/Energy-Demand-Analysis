# < Description > *
# > Script Group Indicator Number and Name
# # : B-04, CTU
# #
# > Script Number(s)
# # : B-04-01A
# #
# > Purpose of the script(s)
# # : Ingest Residential Rate History Data of City of Tallahassee Utilities

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(readxl)
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
DIR_TO.LOAD_HISTORY <- "CTU/Rate-History"
FILE_TO.LOAD_HISTORY <- "CTU_Summary-of-Rates.xlsx"
PATH_TO.LOAD_HISTORY <- paste(
  PATH_DATA_RAW_USE, DIR_TO.LOAD_HISTORY, FILE_TO.LOAD_HISTORY, sep = "/"
)

# # 2. Path(s) at which DT(s) will be saved
DIR_TO.SAVE_HISTORY <- DIR_TO.LOAD_HISTORY
FILE_TO.SAVE_HISTORY <- "CTU_Residential-Rate-History.RData"
PATH_TO.SAVE_HISTORY <- paste(
  PATH_DATA_INTERMEDIATE, DIR_TO.SAVE_HISTORY, FILE_TO.SAVE_HISTORY, sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Create lists containing types of columns
# # 1.1. For columns in "RATES" sheet
col.types_rates <- list(
  "UTILITY" = "text",
  "RATE_CATEGORY" = "text",
  "RATE_TYPE" = "text",
  "RATE_ITEM" = "text",
  "UNIT" = "text",
  "DATE_FROM" = "date",
  "DATE_TO" = "date",
  "TIER" = "numeric",
  "QTY_LOWER" = "numeric",
  "QTY_UPPER" = "numeric",
  "RATE_IN_USD" = "numeric",
  "DESCRIPTION" = "text"
)

# # 1.2. For columns in "OTHERS" sheet
col.types_others <- list(
  "UTILITY" = "text",
  "RATE_CATEGORY" = "text",
  "RATE_TYPE" = "text",
  "RATE_ITEM" = "text",
  "BASE_ITEM" = "text",
  "DATE_FROM" = "date",
  "DATE_TO" = "date",
  "PERCENT" = "numeric",
  "RATE_IN_USD" = "numeric",
  "DESCRIPTION" = "text"
)

# # 1.3. For columns in "GRT PRICE INDEX" sheet
col.types_index <- list(
  "UTILITY" = "text",
  "RATE_CATEGORY" = "text",
  "RATE_TYPE" = "text",
  "RATE_ITEM" = "text",
  "DATE_FROM" = "date",
  "DATE_TO" = "date",
  "RATE_IN_USD" = "numeric",
  "DESCRIPTION" = "text"
)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load an Excel file
# --------------------------------------------------
# ------- Load an Excel file containing Residential Rate History -------
excel_sheets(PATH_TO.LOAD_HISTORY)
# ## Note: This shows that there are three sheets in the Excel file.

# # 1. Sheet 1: "RATES"
# # 1.1. Create a DT
rates <- read_excel(
  path = PATH_TO.LOAD_HISTORY,
  sheet = "RATES",
  col_types = as.character(col.types_rates),
  na = "NA"
) %>% setDT(.)

# # 1.2. Modify the imported DT
# # 1.2.1. Rename columns
names_old <- names(rates)
names_new <- str_to_lower(names_old)
setnames(rates, names_old, names_new)
# # 1.2.2. Convert types of selected columns
rates[, date_from := as.Date(date_from, format = "%Y-%m-%d")]
rates[, date_to := as.Date(date_to, format = "%Y-%m-%d")]


# # 2. Sheet 2: "OTHERS"
# # 2.1. Create a DT
others <- read_excel(
  path = PATH_TO.LOAD_HISTORY,
  sheet = "OTHERS",
  col_types = as.character(col.types_others),
  na = "NA"
) %>% setDT(.)

# # 2.2. Modify the imported DT
# # 2.2.1. Rename columns
names_old <- names(others)
names_new <- str_to_lower(names_old)
setnames(others, names_old, names_new)
# # 2.2.2. Convert types of selected columns
others[, date_from := as.Date(date_from, format = "%Y-%m-%d")]
others[, date_to := as.Date(date_to, format = "%Y-%m-%d")]


# # 3. Sheet 3: "GRT PRICE INDEX"
# # 3.1. Create a DT
index <- read_excel(
  path = PATH_TO.LOAD_HISTORY,
  sheet = "GRT_PRICE_INDEX",
  col_types = as.character(col.types_index),
  na = "NA"
) %>% setDT(.)

# # 3.2. Modify the imported DT
# # 3.2.1. Rename columns
names_old <- names(index)
names_new <- str_to_lower(names_old)
setnames(index, names_old, names_new)
# # 3.2.2. Convert types of selected columns
index[, date_from := as.Date(date_from, format = "%Y-%m-%d")]
index[, date_to := as.Date(date_to, format = "%Y-%m-%d")]


# --------------------------------------------------
# Combine DTs to create a DT
# --------------------------------------------------
# ------- Combine DTs to create a DT -------
# # 1. Combine three DTs
rate.history <- rbind(rates, others, index, fill = TRUE)

# # 2. Modify the combined DT
# # 2.1. Reorder columns
cols_reorder <- c(
  "utility", "rate_category", "rate_type", "rate_item", "base_item", "unit",
  "date_from", "date_to",
  "tier", "qty_lower", "qty_upper",
  "percent", "rate_in_usd",
  "description"
)
setcolorder(rate.history, cols_reorder)


# --------------------------------------------------
# Save the Combined DT
# --------------------------------------------------
# ------- Save the Combined DT in .RData format -------
save(rate.history, file = PATH_TO.SAVE_HISTORY)


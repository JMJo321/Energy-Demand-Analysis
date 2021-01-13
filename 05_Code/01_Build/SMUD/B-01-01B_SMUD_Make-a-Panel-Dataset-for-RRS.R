# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, SMUD
# #
# > Script Number(s)
# # : B-01-01B
# #
# > Purpose of the script(s)
# # : To make a Panel Dataset for Residential Rate Schedules.
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
# # 1. Path(s) for Data file(s) that will be loaded
DIR_TO.LOAD_RRS <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RRS <- "SMUD_Residential-Rate-Schedules.parquet"
PATH_TO.LOAD_RRS <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RRS, FILE_TO.LOAD_RRS, sep = '/')

# # 2. Path(s) at which DT(s) will be saved
DIR_TO.SAVE_RRS <- DIR_TO.LOAD_RRS
FILE_TO.SAVE_RRS <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.SAVE_RRS <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.SAVE_RRS, FILE_TO.SAVE_RRS, sep = "/"
  )


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load Datasets required
# --------------------------------------------------
# ------- Load datasets required -------
rrs <-
  pq.to.dt(
    PATH_TO.LOAD_RRS,
    reg.ex_date = "(^date)|(^season_)",
    is_drop.index_cols = TRUE
  )
# ## Check primary keys of the DT loaded
stopifnot(rrs[, .N, by = .(date, rate_code, charge_item)][N > 1, .N] == 0)


# --------------------------------------------------
# Make a Panel Dataset for Residential Rate Schedules
# --------------------------------------------------
# ------- Make a panel dataset for Residential Rate Schedules -------
# # 1. Make a DT to which data fields will be copied
cols_extract <-
  c(
    "date", "revision_year", "season", "season_from", "season_to",
    "rate_category", "rate_subcategory", "is_closed", "rate_code"
  )
tmp_panel <-
  rrs[, .SD, .SDcols = cols_extract][, .N, by = cols_extract][, N := NULL]


# # 2. Copy data fields
# # 2.1. Data fields regarding fixed charge
cols_by <- c("date", "rate_subcategory", "rate_code")
cols_merge <- c("charge_item", "charge_in_usd")
tmp_dt_1 <-
  merge(
    tmp_panel,
    rrs[charge_type == "Fixed", .SD, .SDcols = c(cols_by, cols_merge)],
    by = cols_by,
    all.x = TRUE
  )
name_old <- c("charge_item", "charge_in_usd")
name_new <- c("fixed_charge_item", "fixed_charge_in_usd")
setnames(tmp_dt_1, name_old, name_new)

# # 2.2. Data fields regarding variable charges
# # 2.2.1.. Data fields regarding tier 1
cols_merge <- c("charge_item", "charge_in_usd", "tier_1_qty_to")
tmp_dt_2 <-
  merge(
    tmp_dt_1,
    rrs[
      charge_item %like% "(^Tier I )|(^Base )",
      .SD, .SDcols = c(cols_by, cols_merge)
    ],
    by = cols_by,
    all.x = TRUE
  )
name_old <- c("charge_item", "charge_in_usd", "tier_1_qty_to")
name_new <-
  c("tier_1_charge_item", "tier_1_charge_in_usd", "tier_1_qty_upto_in_kwh")
setnames(tmp_dt_2, name_old, name_new)
# # 2.2.2. Data fields regarding tier 2
cols_merge <- c("charge_item", "charge_in_usd", "tier_2_qty_to")
tmp_dt_3 <-
  merge(
    tmp_dt_2,
    rrs[
      charge_item %like% "(^Tier II )|(^Base-)",
      .SD, .SDcols = c(cols_by, cols_merge)
    ],
    by = cols_by,
    all.x = TRUE
  )
name_old <- c("charge_item", "charge_in_usd", "tier_2_qty_to")
name_new <-
  c("tier_2_charge_item", "tier_2_charge_in_usd", "tier_2_qty_upto_in_kwh")
setnames(tmp_dt_3, name_old, name_new)
# # 2.2.3. Data fields regarding tier 3
cols_merge <- c("charge_item", "charge_in_usd", "tier_3_qty_to")
tmp_dt_4 <-
  merge(
    tmp_dt_3,
    rrs[charge_item %like% "^Tier III ", .SD, .SDcols = c(cols_by, cols_merge)],
    by = cols_by,
    all.x = TRUE
  )
name_old <- c("charge_item", "charge_in_usd", "tier_3_qty_to")
name_new <-
  c("tier_3_charge_item", "tier_3_charge_in_usd", "tier_3_qty_upto_in_kwh")
setnames(tmp_dt_4, name_old, name_new)
# # 2.3.4. Data fields regarding surcharges
cols_merge <- c("charge_item", "charge_in_usd")
panel_rrs <-
  merge(
    tmp_dt_4,
    rrs[charge_item %like% "Surcharge$", .SD, .SDcols = c(cols_by, cols_merge)],
    by = cols_by,
    all.x = TRUE
  )
name_old <- c("charge_item", "charge_in_usd")
name_new <- c("surcharge_charge_item", "surcharge_charge_in_usd")
setnames(panel_rrs, name_old, name_new)


# # 3. Change values for `tier_2_qty_upto_in_kwh` and `tier_3_qty_upto_in_kwh`
panel_rrs[tier_2_qty_upto_in_kwh == 10^10, tier_2_qty_upto_in_kwh := NA]
panel_rrs[tier_3_qty_upto_in_kwh == 10^10, tier_3_qty_upto_in_kwh := NA]


# # 4. Add a column showing normalized rate codes
# # 4.1. Add a column by using `rate_code` column, and change values
panel_rrs[, rate_code_normalize := rate_code]
panel_rrs[
  str_detect(rate_code_normalize, "H$", negate = TRUE),
  rate_code_normalize := paste0(rate_code_normalize, "H")
]
# # 4.2. Reorder columns
setcolorder(panel_rrs, c(1:3, 23))


# # 5. Check primary key(s) of the DT generated
cols_keys <- c("date", "rate_code_normalize")
stopifnot(panel_rrs[, .N, by = cols_keys][N > 1, .N] == 0)


# --------------------------------------------------
# Save the Panel Dataset
# --------------------------------------------------
# ------- Save the panel dataset in Parquet format -------
arrow::write_parquet(
  panel_rrs, sink= PATH_TO.SAVE_RRS,
  version= "1.0", compression= "gzip", use_dictionary= TRUE
)

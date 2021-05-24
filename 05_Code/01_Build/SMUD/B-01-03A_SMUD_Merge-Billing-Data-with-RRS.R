# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, SMUD
# #
# > Script Number(s)
# # : B-01-03A
# #
# > Purpose of the script(s)
# # : To merge Billing Data with Residential Rate Schedules.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(parallel)
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
# # 1. Paths of data files that will be loaded
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_BILLING <- "SMUD/Billing-Data"
FILE_TO.LOAD_BILLING <- "SMUD_Billing-Data.parquet"
PATH_TO.LOAD_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_BILLING, FILE_TO.LOAD_BILLING, sep= "/"
  )
# # 1.2. SMUD Residential Rate Schedules
DIR_TO.LOAD_RRS <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RRS <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RRS <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RRS, FILE_TO.LOAD_RRS, sep= "/"
  )

# # 2. Path at which a merged data will be saved
DIR_TO.SAVE_BILLING <- DIR_TO.LOAD_BILLING
FILE_TO.SAVE_BILLING <- "SMUD_Billing-Data_Extended.parquet"
PATH_TO.SAVE_BILLING <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.SAVE_BILLING, FILE_TO.SAVE_BILLING, sep= "/"
  )


# ------- Define parameter(s) -------
# # 1. For "mcmapply" function of "parallel" library
N_cores <- 6
# ## Note: The performance of using 6 cores is better than that of using 8 cores.

# # 2. For sellecting observations that will be included in the sample
# # 2.1. To determining whether a observation is included in the sample with
# #      respect to time period
DATE_LOWER <- as.Date("2004-01-01")
DATE_UPPER <- as.Date("2013-12-31")
# # 2.2. To determining whether a billing has a standard billing period
STANDARD.BILLING.PERIOD_UPPER <- 34
STANDARD.BILLING.PERIOD_LOWER <- 27

# # 3. For nomalized rate codes
rate.codes <- c("RSC", "RSE", "RSG", "RWC", "RWE", "RWG")
RATE.CODES <- c(paste0(rate.codes, "H"))


# ------- Define function(s) -------
# # 1. Calculating the quantity prorated
# # 1.1. Make a list that is used to select a sub-function
list_for.subFunction <-
  list(
    # ## For "is_seasons_overlap"
    "TRUE" =
      list(
        # ## For "is_standard.period"
        "TRUE" = "subFunction_T.T",
        "FALSE" = "subFunction_T.F"
      ),
    "FALSE" =
      list(
        "TRUE" = "subFunction_F.T",
        "FALSE" = "subFunction_F.F"
      )
  )
# # 1.2. Define sub-functions
subFunction_T.T <-
  function(
    quantity_season_1, period.length_season_1,
    quantity_season_2, period.length_season_2
  ) {
    return(
      weighted.mean(
        c(quantity_season_1, quantity_season_2),
        c(period.length_season_1, period.length_season_2) /
          (period.length_season_1 + period.length_season_2)
      )
    )
  }
subFunction_T.F <-
  function(
    quantity_season_1, period.length_season_1,
    quantity_season_2, period.length_season_2
  ) {
    return(
      weighted.mean(
        c(quantity_season_1, quantity_season_2),
        c(period.length_season_1, period.length_season_2) /
          (period.length_season_1 + period.length_season_2)
      ) * (period.length_season_1 + period.length_season_2) / 30
    )
  }
subFunction_F.T <-
  function(
    quantity_season_1, period.length_season_1,
    quantity_season_2, period.length_season_2
  ) {
    return(quantity_season_1)
  }
subFunction_F.F <-
  function(
    quantity_season_1, period.length_season_1,
    quantity_season_2, period.length_season_2
  ) {
    return(quantity_season_1 * period.length_season_1 / 30)
  }
# # 1.3. Define a function calculating the quantity prorated
qty_prorate <-
  function(
    is_seasons.overlap, is_standard.period,
    quantity_season_1, period.length_season_1,
    quantity_season_2, period.length_season_2
  ) {
    if (is.na(is_seasons.overlap) == TRUE | is.na(is_standard.period) == TRUE) {
      quantity_to.apply <- NA
    } else {
      tmp_seasons.overlap <- as.character(is_seasons.overlap)
      tmp_standard.period <- as.character(is_standard.period)
      quantity_to.apply <-
        get(
          list_for.subFunction[[tmp_seasons.overlap]][[tmp_standard.period]]
        )(
          quantity_season_1, period.length_season_1,
          quantity_season_2, period.length_season_2
        )
    }
    return(quantity_to.apply)
  }


# --------------------------------------------------
# Load Datasets required
# --------------------------------------------------
# ------- Load datasets required -------
# # 1. Load datasets
# # 1.1. SMUD Billing Data
billing <-
  pq.to.dt(
    PATH_TO.LOAD_BILLING,
    reg.ex_date = "(^date)|(_from$)|(_to$)",
    is_drop.index_cols = TRUE
  )
# # 1.2. SMUD Residential Rate Schedules
rrs <-
  pq.to.dt(
    PATH_TO.LOAD_RRS,
    reg.ex_date = "(^date)|(^season_)",
    is_drop.index_cols = TRUE
  )
# # 1.3. Do garbage collection
gc(reset = TRUE, full = TRUE)

# # 2. Check primary keys of the DTs
stopifnot(
  billing[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)
stopifnot(rrs[, .N, by = .(date, rate_code_normalize)][N > 1, .N] == 0)


# --------------------------------------------------
# Make a DT by merging the two Datasets loaded
# --------------------------------------------------
# ------- Modify the DTs before merging them -------
# # 1. For Residential Rate Schedules Data
# # 1.1. Add a column indicating the date when quantity (kWh per month) changed
rrs[
  ,
  date_qty.changed := min(season_from, na.rm = TRUE),
  by = .(season_from, season_to)
]


# # 2. For Billing Data
# # 2.1. Add indicator variables
# # 2.1.1. Indicator variable showing whether rate codes in billing data also
# #        exist in residential rate schedule data
# ## Add a column showing normalized rate codes
billing[rate_code %in% RATE.CODES, rate_code_normalize := rate_code]
billing[rate_code %in% rate.codes, rate_code_normalize := rate_code]
# ## Note: "H" will be added later.
billing[
  is.na(rate_code_normalize),
  rate_code_normalize := str_extract(rate_code, ".+(?=_)")
]
billing[
  str_detect(rate_code_normalize, "H$", negate = TRUE),
  rate_code_normalize := paste0(rate_code_normalize, "H")
]
stopifnot(
  billing[, .N, by = .(rate_code_normalize)][, .N] == length(RATE.CODES)
)

billing[!is.na(rate_code_normalize), is_common.code := TRUE]
billing[is.na(is_common.code), is_common.code := FALSE]
# # 2.1.2. Indicator variable showing whether an observation is in a time range
billing[
  DATE_LOWER <= period_from & period_to <= DATE_UPPER, is_in.time.range := TRUE
]
billing[is.na(is_in.time.range), is_in.time.range := FALSE]
# # 2.1.3. Indicator variable showing whether a billing period is overlapped
# #        with others
cols_shift <- c("id_account", "id_premise", "period_to")
cols_tmp <- paste0("tmp_", cols_shift)
billing[
   ,
  (cols_tmp) := shift(.SD),
  .SDcols = cols_shift,
  by = .(id_account, id_premise)
]
billing[is.na(tmp_period_to), is_overlap := FALSE]
billing[
  id_account == tmp_id_account & id_premise == tmp_id_premise &
    tmp_period_to < period_from,
  is_overlap := FALSE
]
billing[
  id_account == tmp_id_account & id_premise == tmp_id_premise &
    tmp_period_to >= period_from,
  is_overlap := TRUE
]

# ## For additional investigation
billing[, tmp_is_overlap := shift(is_overlap, n= -1)]
billing[is_overlap == TRUE | tmp_is_overlap == TRUE]
# ## Note: These two code lines allow me to look at obervations have overlapped
# ## billing period.

result <-
  format(
    billing[is_overlap == TRUE, .N] / billing[, .N] * 100, digits= 3, nsmall= 3
  )
print(paste0("The Share of Observations with Overlapped Period: ", result))
# ## Note: There are very small number of observations with overlapped period.

# # 2.1.4. Indicator variable showing whether a billing period is standard
# #        billing
billing[
  STANDARD.BILLING.PERIOD_LOWER <= period_len &
    period_len <= STANDARD.BILLING.PERIOD_UPPER,
  is_standard.billing.period := TRUE
]
billing[is.na(is_standard.billing.period), is_standard.billing.period := FALSE]
# # 2.1.5. Indicator variable showing whether an observation has negative
# #        values for quantity and charges
billing[
  kwh_total < 0 | kwh_t1 < 0 | kwh_t2 < 0 | kwh_t3 < 0 | charge_fixed < 0 |
    charge_variable < 0 | charge_total < 0 | charge_variable_t1 < 0 |
    charge_variable_t2 < 0 | charge_variable_t3 < 0,
  is_negative.value := TRUE
]
billing[is.na(is_negative.value), is_negative.value := FALSE]
# # 2.1.6. Indicator variable showing whether an observation is included in the
# #        sample
billing[
  is_standard.billing.period == TRUE & is_in.time.range == TRUE &
    is_overlap == FALSE & is_common.code == TRUE & is_negative.value == FALSE,
  is_in.sample := TRUE
]
billing[is.na(is_in.sample), is_in.sample := FALSE]

# # 2.2. Drop temporary columns
cols_drop <- names(billing)[str_detect(names(billing), "^tmp_")]
billing[, (cols_drop) := NULL]


# # 3. Merge the two DTs
# ## Note: In a day, tier-related information is identical across observations.

# # 3.1. Merge the two DTS
setnames(rrs, "rate_code", "rate_code_rrs")
cols_by.y <- c("date", "rate_code_normalize")
cols_extract <-
c(
  cols_by.y,
    c(
      "rate_code_rrs", "season", "date_qty.changed", "fixed_charge_in_usd",
      "tier_1_charge_in_usd", "tier_1_qty_upto_in_kwh",
      "tier_2_charge_in_usd", "tier_2_qty_upto_in_kwh",
      "tier_3_charge_in_usd", "tier_3_qty_upto_in_kwh",
      "surcharge_charge_in_usd"
    )
)
suffixes <- c("_before", "_after")
tmp_DT <-
  merge(
    billing,
    rrs[, .SD, .SDcols = cols_extract],
    by.x = c("period_from", "rate_code_normalize"),
    by.y = cols_by.y,
    all.x = TRUE,
    suffixes = suffixes
  )
billing_ext <-
  merge(
    tmp_DT,
    rrs[, .SD, .SDcols = cols_extract],
    by.x = c("period_to", "rate_code_normalize"),
    by.y = cols_by.y,
    all.x = TRUE,
    suffixes = suffixes
  )

# ## Clean memory up
rm(billing, tmp_DT)
invisible(capture.output(gc(verbose= FALSE, full= TRUE)))


# # 4. Add additional columns
# # 4.1. Add a column showing whether quantity changed within a billing period
# # 4.1.1. Define conditions
condition_na <-
  paste0(
    "( ",
     paste(
       "is.na(tier_1_charge_in_usd_before)",
          "is.na(tier_1_charge_in_usd_after)",
          "is.na(tier_1_qty_upto_in_kwh_before)",
          "is.na(tier_1_qty_upto_in_kwh_after)",
      sep= " | "
    ),
       " )"
  )
condition_qty.not.changed <-
  paste(
    paste0("!", condition_na),
      paste0(
        "( ",
        paste(
          "tier_1_charge_in_usd_before == tier_1_charge_in_usd_after",
            "tier_1_qty_upto_in_kwh_before == tier_1_qty_upto_in_kwh_after",
          sep= " & "
        ),
        " )"
      ),
    sep= " & "
  )
condition_qty.changed <-
  paste(
    paste0("!", condition_na),
      paste0(
        "( ",
        paste(
          "tier_1_charge_in_usd_before != tier_1_charge_in_usd_after",
            "tier_1_qty_upto_in_kwh_before != tier_1_qty_upto_in_kwh_after",
          sep= " | "
        ),
        " )"
      ),
    sep= " & "
  )
# # 4.1.2. Add an indicator variable by using the conditions
billing_ext[
  eval(parse(text= condition_qty.not.changed)), is_qty.changed := FALSE
]
billing_ext[eval(parse(text= condition_qty.changed)), is_qty.changed := TRUE]

# # 4.2. Add columns showing prorated quantities
# # 4.2.1. Add temporary columns for calculating prorated quantities
billing_ext[
  is_qty.changed == FALSE,
  `:=` (tmp_before = period_len, tmp_after = 0)
]
billing_ext[
  is_qty.changed == TRUE,
  `:=` (
    tmp_before =
      as.numeric(date_qty.changed_after - period_from, units = "days"),
    tmp_after =
      as.numeric(period_to - date_qty.changed_after + 1, units = "days")
  )
]
# # 4.2.2. Add columns indicating prorated quantities
billing_ext[
  ,
  tier_1_qty_prorate := mcmapply(
    qty_prorate, is_qty.changed, is_standard.billing.period,
    tier_1_qty_upto_in_kwh_before, tmp_before,
    tier_1_qty_upto_in_kwh_after, tmp_after,
    mc.cores = N_cores
  )
]
gc(reset = TRUE, full = TRUE)
billing_ext[
  ,
  tier_2_qty_prorate := mcmapply(
    qty_prorate, is_qty.changed, is_standard.billing.period,
    tier_2_qty_upto_in_kwh_before, tmp_before,
    tier_2_qty_upto_in_kwh_after, tmp_after,
    mc.cores = N_cores
  )
]
gc(reset = TRUE, full = TRUE)

# # 4.3. Add columns showing total kWh consumed in percentage (relative to
# #      quantity)
# # 4.3.1. For the first threshold
billing_ext[, kwh_total_in.percent_t1 := kwh_total / tier_1_qty_prorate * 100]
# # 4.3.2. For the second threshold
billing_ext[, kwh_total_in.percent_t2 := kwh_total / tier_2_qty_prorate * 100]


# # 5. Make the DT clean
# # 5.1. Rename a column
setnames(billing_ext, "date_qty.changed_after", "date_qty.changed")

# # 5.2. Drop unnecessary columns
cols_drop <-
  names(billing_ext)[
    str_detect(names(billing_ext), "(^tmp_)|(date_qty.changed_before)")
  ]
billing_ext[, (cols_drop) := NULL]

# # 5.3. Reorder columns
cols_reorder <-
  c(
      "id_account", "id_premise", "id_bu_part", "rate_code_normalize",
      "rate_code", "period_from", "period_to", "period_len",
      "charge_fixed","charge_variable", "charge_total", "kwh_total", "kwh_t1",
      "charge_variable_t1", "kwh_t2", "charge_variable_t2", "kwh_t3",
      "charge_variable_t3", "is_pv", "is_common.code", "is_in.time.range",
      "is_overlap", "is_standard.billing.period", "is_negative.value",
      "is_in.sample", "is_qty.changed", "date_qty.changed",
      "tier_1_qty_prorate", "tier_2_qty_prorate", "kwh_total_in.percent_t1",
      "kwh_total_in.percent_t2", "rate_code_rrs_before", "season_before",
      "fixed_charge_in_usd_before", "tier_1_charge_in_usd_before",
      "tier_2_charge_in_usd_before", "tier_3_charge_in_usd_before",
      "surcharge_charge_in_usd_before", "tier_1_qty_upto_in_kwh_before",
      "tier_2_qty_upto_in_kwh_before", "tier_3_qty_upto_in_kwh_before",
      "rate_code_rrs_after", "season_after", "fixed_charge_in_usd_after",
      "tier_1_charge_in_usd_after", "tier_2_charge_in_usd_after",
      "tier_3_charge_in_usd_after", "surcharge_charge_in_usd_after",
      "tier_1_qty_upto_in_kwh_after", "tier_2_qty_upto_in_kwh_after",
      "tier_3_qty_upto_in_kwh_after"
  )
setcolorder(billing_ext, cols_reorder)

# # 5.4. Reorder rows
cols_keys <- c("id_account", "id_premise", "period_from", "period_to")
setkeyv(billing_ext, cols_keys)


# --------------------------------------------------
# Save the DT generated
# --------------------------------------------------
# ------- Save the DT generated in Parquet format -------
arrow::write_parquet(
  billing_ext,
  sink = PATH_TO.SAVE_BILLING,
  version = "1.0",
  compression = "snappy",
  use_dictionary = TRUE
)
# ## Note: Version 2.0 is not supported as of September 6, 2020.

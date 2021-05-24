# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-01C
# #
# > Purpose of the script(s)
# # : To

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
library(parallel)
library(ggplot2)
library(data.table)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) for Data file(s)
# # 1.1. SMUD Billing Data for RD Design
DIR_TO.LOAD_GN <- "02_Greenergy-Program"
FILE_TO.LOAD_GN_BILLING <- "DT_Greenergy-Program_Billing-Data.RData"
PATH_TO.LOAD_GN_BILLING <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_BILLING, sep = "/")
# # 1.2. Data for Changes in Greenergy Program Participation and Residential
# #      Rate
FILE_TO.LOAD_GN_CHANGES <- "DT_Greenergy-Program_Changes.RData"
PATH_TO.LOAD_GN_CHANGES <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_CHANGES, sep = "/")
# # 1.3. SMUD Residential Rate Panel
DIR_TO.LOAD_RR <- "SMUD/Residential-Rate-Schedules"
FILE_TO.LOAD_RR <- "SMUD_Residential-Rate-Schedules_Panel.parquet"
PATH_TO.LOAD_RR <-
  paste(PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_RR, FILE_TO.LOAD_RR, sep = "/")

# # # 2. Path at which the DT created will be saved
# DIR_TO.SAVE <- DIR_TO.LOAD_GN
# FILE_TO.SAVE <- "DT_Greenergy-Program_Changes_Extended.RData"
# PATH_TO.SAVE <- paste(PATH_DATA_ANALYSIS, DIR_TO.SAVE, FILE_TO.SAVE, sep = '/')


# ------- Define parameter(s) -------
# (NOT Applicable)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load Datasets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_BILLING)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Residential Rates Panel Data -------
# # 1. Load a parquet file
dt_rr <- pq.to.dt(
  PATH_TO.LOAD_RR,
  reg.ex_date = "(^date)|(_from$)|(_to$)",
  is_drop.index_cols = TRUE
)
gc(reset = TRUE, full = TRUE)

# # 2. Check Primary Keys of the DT
stopifnot(dt_rr[, .N, by = .(date, rate_code_normalize)][N > 1, .N] == 0)


# ------- Load Data for Changes in rate code and Greenergy Program Participation -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_CHANGES)

# # 2. Check primary keys of the DT
stopifnot(dt_rate.change[, .N, by = .(ids)][N > 1, .N] == 0)
stopifnot(dt_suffix.change[, .N, by = .(ids)][N > 1, .N] == 0)


# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------
# #
dt_billing[billing.month_mid %in% c(5:10), season_modified := "Summer"]
dt_billing[billing.month_mid %in% c(1:4, 11:12), season_modified := "Winter"]


# # Make a DT including required columns only
dt_subset <- dt_billing[
  # category_greenergy == "Greenergy to Non-Greenergy" &
  category_greenergy == "Greenergy to Non-Greenergy" &
    rate_code_normalize %in% c("RSGH", "RSCH", "RSEH"),
  .(
    ids, id_account, id_premise, category_greenergy, rate_code,
    rate_code_normalize, suffix_greenergy, period_from, period_to,
    first.date_suffix.change_by.ids.and.suffix,
    billing.ym_mid, billing.year_mid, billing.month_mid, season_before,
    season_after, season_modified, kwh_total, charge_fixed, charge_variable,
    charge_total
  )
]
dt_subset[, .N, by = .(suffix_greenergy)]


# #
compute_surcharge <- function(
  DT_rate.codes , normalized.rate.code,
  billing.period_first.date, billing.period_last.date, surcharge.item_in.str
) {
  tmp_N <- DT_rate.codes[
    rate_code_normalize == normalized.rate.code &
      date %in% c(billing.period_first.date:billing.period_last.date),
    .N
  ]

  return(
    DT_rate.codes[
      rate_code_normalize == normalized.rate.code &
        date %in% c(billing.period_first.date:billing.period_last.date),
      lapply(.SD, sum, na.rm = TRUE), .SDcols = c(surcharge.item_in.str)
    ][, get(surcharge.item_in.str)] / tmp_N
  )
}


dt_subset[
  ,
  surcharge.rate_energy_per.kwh := mcmapply(
    FUN = compute_surcharge,
    normalized.rate.code = rate_code_normalize,
    billing.period_first.date = period_from,
    billing.period_last.date = period_to,
    MoreArgs = list(
      DT_rate.codes = dt_rr,
      surcharge.item_in.str = "surcharge_energy_charge.per.kwh_in.usd"
    ),
    mc.cores = 6
  )
]
dt_subset[
  ,
  surcharge.rate_solar_per.kwh := mcmapply(
    FUN = compute_surcharge,
    normalized.rate.code = rate_code_normalize,
    billing.period_first.date = period_from,
    billing.period_last.date = period_to,
    MoreArgs = list(
      DT_rate.codes = dt_rr,
      surcharge.item_in.str = "surcharge_solar_charge.per.kwh_in.usd"
    ),
    mc.cores = 6
  )
]

save(dt_subset, file = "/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/04_Data/03_For-Analysis/02_Greenergy-Program/Temporary_N-to-G.RData")


load("/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/04_Data/03_For-Analysis/02_Greenergy-Program/Temporary_N-to-G.RData")





# #
dt_subset[, charge_others := charge_total - (charge_fixed + charge_variable)]

# #
dt_subset[
  billing.year_mid < 2011,
  charge_surcharge_state := round(kwh_total * 0.00022, 2)
]
dt_subset[
  billing.year_mid >= 2011,
  charge_surcharge_state := round(kwh_total * 0.00029, 2)
]

# #
dt_subset[
  ,
  charge_surcharge_energy := round(kwh_total * surcharge.rate_energy_per.kwh, 2)
]


# #
dt_subset[
  ,
  charge_surcharge_solar := round(kwh_total * surcharge.rate_solar_per.kwh, 2)
]



# #
dt_subset[
  suffix_greenergy == 1,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.01, 2)
  )
]
dt_subset[
  suffix_greenergy == 2,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.005, 2)
  )
]
dt_subset[
  suffix_greenergy == 3,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.010, 2)
  )
]
dt_subset[
  suffix_greenergy == 4,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.015, 2)
  )
]
dt_subset[
  suffix_greenergy == 5,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.020, 2)
  )
]
dt_subset[
  suffix_greenergy == 6,
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = round(kwh_total * 0.015, 2)
  )
]
dt_subset[
  suffix_greenergy == 11,
  `:=` (
    charge_greenergy_fixed = 6,
    charge_greenergy_variable = 0
  )
]
dt_subset[
  suffix_greenergy == 12,
  `:=` (
    charge_greenergy_fixed = 3,
    charge_greenergy_variable = 0
  )
]
dt_subset[
  suffix_greenergy == 13,
  `:=` (
    charge_greenergy_fixed = 6,
    charge_greenergy_variable = round(kwh_total * 0.010, 2)
  )
]
dt_subset[
  suffix_greenergy == 14,
  `:=` (
    charge_greenergy_fixed = 3,
    charge_greenergy_variable = round(kwh_total * 0.010, 2)
  )
]
dt_subset[
  is.na(suffix_greenergy),
  `:=` (
    charge_greenergy_fixed = 0,
    charge_greenergy_variable = 0
  )
]



cols_to.sum <- c("charge_greenergy_fixed", "charge_greenergy_variable")
dt_subset[
  , charge_greenergy_total := rowSums(.SD, na.rm = TRUE), .SDcols = cols_to.sum
]


# #
cols_to.sum <- c(
  "charge_surcharge_state", "charge_surcharge_energy",
  "charge_surcharge_solar", "charge_greenergy_total"
)
dt_subset[, charge_unknown := charge_others - rowSums(.SD, na.rm = TRUE), .SDcols = cols_to.sum]




cols_to.sum <- c(
  "charge_fixed", "charge_variable", "charge_greenergy_total",
  "charge_surcharge_energy", "charge_surcharge_solar"
)
dt_subset[
  ,
  share_unknown := (charge_unknown / rowSums(.SD, na.rm = TRUE)) %>% round(., digits = 5),
  .SDcols = cols_to.sum
]
# ## Note: State Surcharge is not considered when Sacramento City Tax is
# ## computed.



dt_subset[charge_unknown < 0, .N, by = .(ids)][, .N]
dt_subset[charge_unknown < 0]$charge_unknown %>% summary(.)
# ## Note: There are negative values for column "charge_unknown". These might
# ## be occured due to some credits.

cols_to.sum <- c("charge_fixed", "charge_greenergy_fixed")
dt_subset[, usd.per.kwh_variable.only := ((charge_total - rowSums(.SD, na.rm = TRUE)) / kwh_total / (charge_variable / kwh_total)) %>% round(., digits = 5), .SDcols = cols_to.sum]
stopifnot(dt_subset[is.nan(usd.per.kwh_variable.only)][kwh_total != 0, .N] == 0)
stopifnot(dt_subset[is.infinite(usd.per.kwh_variable.only)][!(kwh_total == 0 | charge_variable == 0), .N] == 0)
# ## There are zero consumption billing cycles that causes NaN values
dt_subset[is.nan(usd.per.kwh_variable.only) | is.infinite(usd.per.kwh_variable.only), usd.per.kwh_variable.only := NA]



# -------  -------
# #
tmp_dt <- dt_subset[!is.na(suffix_greenergy), .(ids, rate_code_normalize, period_from, period_to, suffix_greenergy, share_unknown)]
tmp_dt[, .N, by = .(ids, suffix_greenergy)][, .N, by = .(ids)][N > 2, .N]
tmp_dt[, mean_share := mean(share_unknown, na.rm = TRUE), by = .(ids)]
tmp_dt[, sd_share := sd(share_unknown, na.rm = TRUE), by = .(ids)]
dt_subset <- tmp_dt[, .N, by = .(ids, mean_share, sd_share)][, N := NULL][dt_subset, on = .(ids)]

dt_subset[is.na(sd_share) & !is.na(suffix_greenergy), .N, by = .(ids)][N > 1]
# ## Note: For several observations, the number of observation with "is.na(suffix_greenergy) == FALSE" is just one.


# #
tmp_dt <- dt_billing[, .(ids, rate_code_normalize, period_from, period_to, season_modified, kwh_total)]
tmp_dt[, mean_kwh := mean(kwh_total, na.rm = TRUE), by = .(ids, season_modified)]
tmp_dt[, sd_kwh := sd(kwh_total, na.rm = TRUE), by = .(ids, season_modified)]
dt_subset <- tmp_dt[, .N, by = .(ids, season_modified, mean_kwh, sd_kwh)][, N := NULL][dt_subset, on = .(ids, season_modified)]

# #
tmp_dt <- dt_subset[!is.na(suffix_greenergy), .(ids, rate_code_normalize, period_from, period_to, usd.per.kwh_variable.only)]
tmp_dt[, mean_usd.per.kwh := mean(usd.per.kwh_variable.only, na.rm = TRUE), by = .(ids)]
tmp_dt[, sd_usd.per.kwh := sd(usd.per.kwh_variable.only, na.rm = TRUE), by = .(ids)]
dt_subset <- tmp_dt[, .N, by = .(ids, mean_usd.per.kwh, sd_usd.per.kwh)][, N := NULL][dt_subset, on = .(ids)]


# Tests
dt_subset[, critical.value_share := qnorm(0.975, mean = mean_share, sd = sd_share) %>% round(., digits = 5)]
dt_subset[, critical.value_kwh_upper := qnorm(0.975, mean = mean_kwh, sd = sd_kwh) %>% round(., digits = 5)]
dt_subset[, critical.value_kwh_lower := qnorm(0.025, mean = mean_kwh, sd = sd_kwh) %>% round(., digits = 5)]
dt_subset[, critical.value_usd.per.kwh_upper := qnorm(0.975, mean = mean_usd.per.kwh, sd = sd_usd.per.kwh) %>% round(., digits = 5)]
dt_subset[, critical.value_usd.per.kwh_lower := qnorm(0.025, mean = mean_usd.per.kwh, sd = sd_usd.per.kwh) %>% round(., digits = 5)]

dt_subset[, is_significant_share := critical.value_share < share_unknown]
dt_subset[, is_significant_kwh_too.small := kwh_total < critical.value_kwh_lower]
dt_subset[, is_significant_kwh_too.large := critical.value_kwh_upper < kwh_total]
dt_subset[, is_significant_usd.per.kwh_too.small := usd.per.kwh_variable.only < critical.value_usd.per.kwh_lower]
dt_subset[, is_significant_usd.per.kwh_too.large := critical.value_usd.per.kwh_upper < usd.per.kwh_variable.only]

dt_subset[is.na(suffix_greenergy), .N, keyby = .(is_significant_share, is_significant_kwh_too.small, is_significant_kwh_too.large, is_significant_usd.per.kwh_too.small, is_significant_usd.per.kwh_too.large)]


dt_subset[
  is_significant_share == FALSE & is_significant_kwh_too.small == FALSE &
    is_significant_kwh_too.large == FALSE &
    is_significant_usd.per.kwh_too.small == FALSE &
    is_significant_usd.per.kwh_too.large == TRUE
]


# modified

ids_NA.for.share <- dt_subset[is.na(is_significant_share), .N, by = .(ids)]$ids
dt_subset[ids %in% ids_NA.for.share, is_significant_infer := NA]
dt_subset[
  !ids %in% ids_NA.for.share & is_significant_share == FALSE,
  is_significant_infer := FALSE
]
dt_subset[
  !ids %in% ids_NA.for.share & is_significant_share == TRUE,
  is_significant_infer := TRUE
]
dt_subset[!ids %in% ids_NA.for.share & !is.na(suffix_greenergy), is_significant_infer := FALSE]



dt_subset[, is_significant_infer_lead := shift(is_significant_infer, 1), by = .(ids)]

dt_to.show <- dt_subset[!is.na(is_significant_infer_lead) & is_significant_infer_lead != is_significant_infer, .N, by = .(ids)][, .N, keyby = .(N)]
names(dt_to.show) <- c("changes", "households")
dt_to.show[, cumumlative := cumsum(households) / lapply(.SD, sum, na.rm = TRUE)$households * 100, .SDcols = "households"]
dt_to.show

dt_subset[!is.na(is_significant_infer_lead) & is_significant_infer_lead != is_significant_infer, .N, by = .(ids)][N == 2]


dt_subset[
  ids %in%  dt_subset[!is.na(is_significant_infer_lead) & is_significant_infer_lead != is_significant_infer, .N, by = .(ids)][N == 2]$ids &
    is_significant_infer == FALSE & is_significant_infer_lead == TRUE,
  .N, keyby = .(billing.ym_mid)
]



dt_subset[, is_significant_kwh := is_significant_kwh_too.large == TRUE | is_significant_kwh_too.small == TRUE]
dt_subset[, is_significant_usd.per.kwh := is_significant_usd.per.kwh_too.large == TRUE | is_significant_usd.per.kwh_too.small == TRUE]

dt_subset[
  is_significant_share == TRUE & is_significant_usd.per.kwh == TRUE,
  category_share.and.usd.per.kwh := "Both"
]
dt_subset[
  is_significant_share == TRUE & is_significant_usd.per.kwh == FALSE,
  category_share.and.usd.per.kwh := "Share only"
]
dt_subset[
  is_significant_share == FALSE & is_significant_usd.per.kwh == TRUE,
  category_share.and.usd.per.kwh := "USD per kWh only"
]
dt_subset[
  is_significant_share == FALSE & is_significant_usd.per.kwh == FALSE,
  category_share.and.usd.per.kwh := "Neither"
]

dt_subset[
  is_significant_share == TRUE & is_significant_kwh == TRUE,
  category_share.and.kwh := "Both"
]
dt_subset[
  is_significant_share == TRUE & is_significant_kwh == FALSE,
  category_share.and.kwh := "Share only"
]
dt_subset[
  is_significant_share == FALSE & is_significant_kwh == TRUE,
  category_share.and.kwh := "USD per kWh only"
]
dt_subset[
  is_significant_share == FALSE & is_significant_kwh == FALSE,
  category_share.and.kwh := "Neither"
]


dt_subset[, share_unknown_normal := share_unknown - mean_share]
dt_subset[, usd.per.kwh_normal := usd.per.kwh_variable.only - mean_usd.per.kwh]
dt_subset[, category_suffix := !is.na(suffix_greenergy)]


plot_test <-
  ggplot(
    data = dt_subset[category_share.and.usd.per.kwh != "Neither"],
    aes(
      x = share_unknown_normal,
      y = usd.per.kwh_normal,
      # color = category_suffix
      color = category_share.and.usd.per.kwh
    )
  ) +
    geom_jitter(alpha = 0.2) +
    scale_x_continuous(limits = c(-0.5, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_linedraw()






# dt_subset[, .N, by = .(ids, is_out.of.upper.bound)][, .N, by = .(ids)][N == 1]
# dt_subset[, .N, by = .(ids, is_out.of.upper.bound)][, .N, by = .(ids)][N > 1]
# dt_subset[, .N, by = .(ids, is_out.of.upper.bound)][, .N, by = .(ids)][N > 2]
#
# dt_subset[, .N, by = .(ids, is_out.of.upper.bound)][, .N, by = .(ids)][N == 1][, .N, by = .(ids)][, .N] / dt_subset[, .N, by = .(ids)][, .N] * 100
# dt_subset[, .N, by = .(ids, is_out.of.upper.bound)][, .N, by = .(ids)][N > 1][, .N, by = .(ids)][, .N] / dt_subset[, .N, by = .(ids)][, .N] * 100
# # ## Note: These results highly seem that the information about Greenergy program participation, which can be inferred from each billing cycle's rate code, is not reliable.
#
#
# dt_subset[, lead_is_out.of.upper.bound := shift(is_out.of.upper.bound, 1), by = .(ids)]
# dt_subset[!is.na(lead_is_out.of.upper.bound) & lead_is_out.of.upper.bound != is_out.of.upper.bound, .N, by = .(ids)][N > 1]
# dt_subset[!is.na(lead_is_out.of.upper.bound) & lead_is_out.of.upper.bound != is_out.of.upper.bound, .N, by = .(ids)][N > 1]$N %>% summary(.)
#
#
# N_hh <- dt_subset[!is.na(lead_is_out.of.upper.bound) & lead_is_out.of.upper.bound != is_out.of.upper.bound, .N, by = .(ids)][, .N]
# dt_to.show <- dt_subset[!is.na(lead_is_out.of.upper.bound) & lead_is_out.of.upper.bound != is_out.of.upper.bound, .N, by = .(ids)][, .N, keyby = .(N)]
# names(dt_to.show) <- c("changes", "households")
# dt_to.show[, share := households / N_hh * 100]
# dt_to.show[, cum := cumsum(share)]


# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------
#
# dt_subset[suffix_greenergy %in% c(), total.charge.per.kwh := ((charge_total - charge_fixed) / kwh_total) / (charge_variable / kwh_total)]
#
# tmp_dt <- dt_subset[!is.na(suffix_greenergy), .(ids, period_from, period_to, suffix_greenergy, total.charge.per.kwh)]
# tmp_dt[, mean_kwh_by.ids.and.suffix := mean(total.charge.per.kwh, na.rm = TRUE), by = .(ids)]
# tmp_dt[, sd_kwh_by.ids.and.suffix := sd(total.charge.per.kwh, na.rm = TRUE), by = .(ids)]
# dt_subset <- tmp_dt[, .N, by = .(ids, mean_kwh_by.ids.and.suffix, sd_kwh_by.ids.and.suffix)][, N := NULL][dt_subset, on = .(ids)]
#
#
# dt_subset[, z.value_kwh_upper := qnorm(0.975, mean = mean_kwh_by.ids.and.suffix, sd = sd_kwh_by.ids.and.suffix)]
# dt_subset[, is_out.of.upper.bound_kwh := z.value_kwh_upper < total.charge.per.kwh]
# dt_subset[z.value_kwh_upper < total.charge.per.kwh, is_out.of.upper.bound_kwh := TRUE]
# dt_subset[is.na(is_out.of.upper.bound_kwh), is_out.of.upper.bound_kwh := FALSE]
#
# dt_subset[is_out.of.upper.bound == TRUE & is_out.of.upper.bound_kwh == TRUE][, .N, by = .(ids)]
#


# --------------------------------------------------
#
# --------------------------------------------------
# -------  -------










# # --------------------------------------------------
# # Save the DT created
# # --------------------------------------------------
# # ------- Save the DT created in .RData format -------
# save(dt_pv.and.greenergy, file = PATH_TO.SAVE)

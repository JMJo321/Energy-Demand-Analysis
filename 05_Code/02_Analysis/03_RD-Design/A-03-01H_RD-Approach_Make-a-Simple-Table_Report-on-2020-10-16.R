# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-02G
# #
# > Purpose of the script(s)
# # : Generate a simple table.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(huxtable)
library(stringr)
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
# # 1.1. SMUD Billing Data
DIR_TO.LOAD_RD <- "RD-Approach"
FILE_TO.LOAD_RD <- "DT_For-Regression_RD-Approach.parquet"
PATH_TO.LOAD_RD <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_RD, FILE_TO.LOAD_RD, sep= "/")


## ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load SMUD Billing Data
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load SMUD billing data
dt_for.reg <-
  pq.to.dt(
    PATH_TO.LOAD_RD,
    reg.ex_date= "(^date)|(_from$)|(_to$)",
    is_drop.index_cols= TRUE
  )

# # 2. Check primary keys of the DTs
stopifnot(
  dt_for.reg[
    , .N, by = .(id_account, id_premise, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# --------------------------------------------------
# Create a simple table
# --------------------------------------------------
# ------- Create a simple table -------
# # 1. Generate objects that will be utilized to create a DT
for (bw in c(0, 5, 10, 15, seq(20, 90, by = 10))) {
  tmp_name_below <- paste0("n_bw", bw, "_below")
  tmp_name_above <- paste0("n_bw", bw, "_above")
  tmp_name_hh <- paste0("n_bw", bw, "_hh")

  if (bw == 0) {
    tmp_col <- "is_balanced.ids_bw_na"

    assign(
      tmp_name_below,
      dt_for.reg[
        kwh_total_in.percent_normalize_period0 <= 0 &
          get(tmp_col) == TRUE,
        .N
      ]
    )

    assign(
      tmp_name_above,
      dt_for.reg[
        0 < kwh_total_in.percent_normalize_period0 &
          get(tmp_col) == TRUE,
        .N
      ]
    )

    assign(
      tmp_name_hh,
      dt_for.reg[get(tmp_col) == TRUE, .N, by = .(id_account, id_premise)][, .N]
    )
  } else {
    tmp_col <- paste0("is_balanced.ids_bw_", bw)

    assign(
      tmp_name_below,
      dt_for.reg[
        (bw * -1 <= kwh_total_in.percent_normalize_period0 &
          kwh_total_in.percent_normalize_period0 <= 0) &
          get(tmp_col) == TRUE,
        .N
      ]
    )

    assign(
      tmp_name_above,
      dt_for.reg[
        (0 < kwh_total_in.percent_normalize_period0 &
          kwh_total_in.percent_normalize_period0 <= bw) &
          get(tmp_col) == TRUE,
        .N
      ]
    )

    assign(
      tmp_name_hh,
      dt_for.reg[
        (bw * -1 <= kwh_total_in.percent_normalize_period0 &
          kwh_total_in.percent_normalize_period0 <= bw) &
          get(tmp_col) == TRUE,
        .N,
        by = .(id_account, id_premise)
      ][, .N]
    )
  }
}

# # 2. Generate a DT that will be used to generate a simple table
dt_by.bw <-
  data.table(
    "Bandwidth" = c(paste0(c(5, 10, 15, seq(20, 90, by = 10)), "%"), "N/A"),
    "Households" =
      c(
        n_bw5_hh, n_bw10_hh, n_bw15_hh, n_bw20_hh, n_bw30_hh, n_bw40_hh,
        n_bw50_hh, n_bw60_hh, n_bw70_hh, n_bw80_hh, n_bw90_hh, n_bw0_hh
      ),
    "Control" =
      c(
        n_bw5_below, n_bw10_below, n_bw15_below, n_bw20_below, n_bw30_below,
        n_bw40_below, n_bw50_below, n_bw60_below, n_bw70_below, n_bw80_below,
        n_bw90_below, n_bw0_below
      ),
    "Treatment" =
      c(
        n_bw5_above, n_bw10_above, n_bw15_above, n_bw20_above, n_bw30_above,
        n_bw40_above, n_bw50_above, n_bw60_above, n_bw70_above, n_bw80_above,
        n_bw90_above, n_bw0_above
      )
  )
dt_by.bw[, Total := Control + Treatment]

# # 3. Create a simple table showing both the number of households and
# #    the distribution of observations by bandwidth
hux.table_by.bw <- as_hux(dt_by.bw)
number_format(hux.table_by.bw)[2:13, 2:5] <- fmt_pretty()
bottom_border(hux.table_by.bw)[1,] <- 1
hux.table_by.bw <-
  insert_row(
    hux.table_by.bw, "", "Households", "Observations", "", "", after = 0
  ) %>%
    merge_cells(., 1, 3:5) %>%
    set_align(., 1:2, everywhere, "center")

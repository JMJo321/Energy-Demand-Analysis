# < Description >
# > Script Group Indicator Number and Name
# # : A-04, Greenergy Program
# #
# > Script Number(s)
# # : A-04-02B
# #
# > Purpose of the script(s)
# # : To make plots related to PV adoption and Greenergy Program participation.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(zoo)
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
DIR_TO.LOAD_GN <- "02_Greenergy-Program"
FILE_TO.LOAD_GN_BILLING <- "DT_Greenergy-Program_Billing-Data.RData"
PATH_TO.LOAD_GN_BILLING <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_BILLING, sep = "/")

# # 1.2. PV-Adoption and Greenergy-Program-Participation Data
FILE_TO.LOAD_GN_CHANGES <- "DT_Greenergy-Program_Changes_Extended.RData"
PATH_TO.LOAD_GN_CHANGES <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_CHANGES, sep = "/")


# # 2. Paths at which Output will be saved
DIR_TO.SAVE_FIGURE <- paste(
  PATH_NOTE, "04_Greenergy-Program/02_Figures", sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Levels and Labels for Suffixes of Greenergy Program
levels_suffixes <- c(
  1, 2, 3,
  4, 5, 6, 7,
  11, 12,
  13, 14
)
labels_suffixes <- c(
  "100% Green (1)", "50% Green (2)", "Community Solar (3)",
  "Options 1 & 2 (4)", "Options 1 & 3 (5)", "Options 2 & 3 (6)",
  "Options 1, 2 & 3 (7)",
  "100% Green Flat Fee (11)", "50% Green Flat Fee (12)",
  "Options 3 & 11 (13)", "Options 3 & 12 (14)"
)

# # 2. Labels for Deciles w.r.t. Consumption
labels_deciles <- c(
  "(0%,10%]", "(10%,20%]", "(20%,30%]", "(30%,40%]", "(40%,50%]",
  "(50%,60%]", "(60%,70%]", "(70%,80%]", "(80%,90%]", "(90%,100%]"
)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Datasets
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


# ------- Load SMUD Data w.r.t. Changes in Greenergy Program, etc. -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_CHANGES)

# 2. Check primary keys of the DT
stopifnot(dt_pv.and.greenergy[, .N, by = .(ids)][N > 1, .N] == 0)


# --------------------------------------------------
# Create DTs to make figures
# --------------------------------------------------
# ------- DTs w.r.t. PV Adoption  -------
# # 1. DT for the number of household adopting PV system by Greenergy Program
# #    participation
# # 1.1. Create objects that will be used later
cols_to.subset <- c(
  "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy", "Non-Greenergy"
)
tmp_year.month_base <- dt_pv.and.greenergy[
  !is.na(year.month_pv.adoption_by.ids),
  .N, keyby = .(year.month_pv.adoption_by.ids)
]$year.month_pv.adoption_by.ids
tmp_year.month <- seq(
  min(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  max(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  by = "month"
) %>% as.yearmon(.)

# # 1.2. Make a temporary DT
tmp_dt_pv.adoption <- data.table(
  year.month_pv.adoption_by.ids = rep(
    tmp_year.month, each = length(cols_to.subset)
  ),
  category_greenergy = rep(cols_to.subset, times = length(tmp_year.month))
)
tmp_dt_pv.adoption[
  , year.month_pv.adoption_by.ids := as.yearmon(year.month_pv.adoption_by.ids)
]

# # 1.3. Create a DT from the temporary DT
dt_pv.adoption <- dt_pv.and.greenergy[
  category_greenergy %in% cols_to.subset
][
  !is.na(year.month_pv.adoption_by.ids),
  .N, keyby = .(year.month_pv.adoption_by.ids, category_greenergy)
][
  tmp_dt_pv.adoption, on = .(year.month_pv.adoption_by.ids, category_greenergy)
]
dt_pv.adoption[is.na(N), N := 0]


# # 2. DT for before-after consumption comparision w.r.t. PV Adoption
# # 2.1. Make a temporary DT
tmp_dt_qty_pv <- dt_billing[
  is_pv.adoption == TRUE &
    season_before == "Summer" & season_before == season_after,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
  by = .(ids, is_pv, category_greenergy)
][
  , tmp_col := .N, by = .(ids, category_greenergy)
][
  tmp_col == 2
  # ## Note: To exclude both always-adopter and never-adopter.
][
  , tmp_col := NULL
]

# # 2.2. Decast the temporary DT
dt_qty_pv <- dcast(tmp_dt_qty_pv, ids + category_greenergy ~ is_pv)

# # 2.3. Rename data fields
names_old <- c("FALSE", "TRUE")
names_new <- c("mean.consumption_before", "mean.consumption_after")
setnames(dt_qty_pv, names_old, names_new)


# ------- Make a DT to add straight lines -------
dt_for.abline <- data.table(
  "category_greenergy" = rep(
    c("Non-Greenergy to Greenergy", "Greenergy to Non-Greenergy"),
    times = 2
  ),
  "slope" = rep(c(1), times = 4),
  "intercept" = c(0, 0, -2, 5)
)


# ------- DTs for Greenergy Program Participation -------
# # 1. Make a DT showing Greenergy Program Participation over time
# # 1.1. Create objects that will be used later
cols_to.subset <- c(
  "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy",
  "Greenergy to Greenergy"
)
tmp_year.month_base <- dt_pv.and.greenergy[
  category_greenergy %in% cols_to.subset,
  .N, keyby = .(year.month_final_suffix_greenergy)
]$year.month_final_suffix_greenergy
tmp_year.month <- seq(
  min(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  max(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  by = "month"
) %>% as.yearmon(.)

# # 1.2. Make a temporary DT
tmp_dt_greenergy.participation <- data.table(
  year.month_final_suffix_greenergy =
    rep(tmp_year.month, each = length(cols_to.subset)),
  category_greenergy = rep(cols_to.subset, times = length(tmp_year.month))
)
tmp_dt_greenergy.participation[
  ,
  year.month_final_suffix_greenergy :=
    as.yearmon(year.month_final_suffix_greenergy)
]

# # 1.3. Create a DT from the temporary DT
dt_greenergy.participation <- dt_pv.and.greenergy[
  category_greenergy %in% cols_to.subset
][
  ,
  .N, keyby = .(year.month_final_suffix_greenergy, category_greenergy)
][
  tmp_dt_greenergy.participation,
  on = .(year.month_final_suffix_greenergy, category_greenergy)
]
dt_greenergy.participation[is.na(N), N := 0]


# 2. Create a DT to do before-after comparision w.r.t. electricity consumption
# # 2.1. Create a temporary DT
tmp_dt_qty_greenergy <- dt_billing[
  is_pv.adoption == FALSE,
  .(ids, suffix_greenergy, period_from, season_before, season_after, kwh_total)
][
  dt_pv.and.greenergy[
    !is.na(first.date_final_suffix_greenergy),
    .(
      ids, first.date_final_suffix_greenergy,
      initial_suffix_greenergy, final_suffix_greenergy
    )
  ],
  on = .(ids)
]

# # 2.2. Add an indicator variable showing whether each billing cycle is
# #      in Greenergy Program or not
tmp_dt_qty_greenergy[
  , is_greenergy := FALSE
][
  period_from >= first.date_final_suffix_greenergy,
  is_greenergy := TRUE
]

# # 2.3. Create a DT from the temporary
dt_qty_greenergy <-
  tmp_dt_qty_greenergy[
    season_before == "Summer" & season_before ==  season_after,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
    by = .(ids, initial_suffix_greenergy, final_suffix_greenergy, is_greenergy)
  ] %>%
    dcast(
      ., ids + initial_suffix_greenergy + final_suffix_greenergy ~ is_greenergy
    )

# # 2.4. Modify the DT created
# # 2.4.1. Rename columns
names_old <- c("FALSE", "TRUE")
names_new <- c("mean.consumption_before", "mean.consumption_after")
setnames(dt_qty_greenergy, names_old, names_new)
# # 2.4.2. Add data fields
# ## A data field showing the change in average consumptions
dt_qty_greenergy[
  , change_consumption := mean.consumption_after - mean.consumption_before
]
# ## An indicator variable showing whether average consumption increase or not
dt_qty_greenergy[change_consumption >= 0, is_increase := TRUE]
dt_qty_greenergy[change_consumption < 0, is_increase := FALSE]
# # 2.4.3. Conver columns' data type from integer to factor
dt_qty_greenergy[
  ,
  `:=` (
    suffix_greenergy_before = factor(
      initial_suffix_greenergy,
      levels = levels_suffixes, labels = labels_suffixes
    ),
    suffix_greenergy_after = factor(
      final_suffix_greenergy,
      levels = levels_suffixes, labels = labels_suffixes
    )
  )
]


# # 3. Create DTs that will be used to make figures
# # 3.1. Create a DT including info. about changes in Greenergy-Program-related
# #      suffixes
# # 3.1.1. Make a temporary DT
categories_to.select <- c(
  "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy",
  "Greenergy to Greenergy"
)
tmp_dt_suffix.change <- dt_pv.and.greenergy[
  is_pv.adoption == FALSE & category_greenergy %in% categories_to.select,
  # ## Note: PV adopters are exlcuded because their consumption is net.
  .(ids, first.date_final_suffix_greenergy)
][
  dt_billing[
    , .(ids, period_from, suffix_greenergy, billing.ym_mid, kwh_total)
  ],
  on = .(ids)
]
# # 3.1.2. Add data fields to the temporary DT
tmp_dt_suffix.change[
  period_from < first.date_final_suffix_greenergy,
  sn_bcs_before := .N:1, by = .(ids)
]
tmp_dt_suffix.change <- tmp_dt_suffix.change[sn_bcs_before <= 12]
# ## Note: Only care about 12 billing cycles before changing Greenergy
# ## Program participation.

tmp_dt_suffix.change[
  ,
  decile_by.mid := cut(
    kwh_total,
    breaks = quantile(kwh_total, probs = seq(0, 1, by = 0.1)),
    labels = c(1:10)
  ) %>% as.integer(.),
  by = .(billing.ym_mid)
]
# ## Note: Determine which decile each household is included in.

tmp_dt_suffix.change[
  ,
  mean.decile_by.ids := lapply(.SD, mean, na.rm = TRUE),
  .SDcols = c("decile_by.mid"),
  by = .(ids)
]
tmp_dt_suffix.change[
  , mean.decile_by.ids := round(mean.decile_by.ids, digits = 0) %>% factor(.)
]
# ## Note: Compute each household's average decile.

dt_suffix.change <- dt_pv.and.greenergy[
  tmp_dt_suffix.change[, .N, by = .(ids, mean.decile_by.ids)][, N := NULL],
  on = .(ids)
]
dt_suffix.change[
  ,
  `:=` (
    initial_suffix_greenergy = factor(
      initial_suffix_greenergy,
      levels = levels_suffixes, labels = labels_suffixes
    ),
    final_suffix_greenergy = factor(
      final_suffix_greenergy,
      levels = levels_suffixes, labels = labels_suffixes
    ),
    mean.decile_by.ids = factor(
      mean.decile_by.ids,
      levels = c(1:10), labels = labels_deciles
    )
  )
]

# # 3.2. Create DTs to make figures from the DT created above
# # 3.2.1. For entire time range
dt_suffix.change_to.plot <- dt_suffix.change[
  mean.decile_by.ids %in% labels_deciles,
  .N,
  by = .(
    category_greenergy, initial_suffix_greenergy, final_suffix_greenergy,
    mean.decile_by.ids
  )
]
dt_suffix.change_to.plot[
  ,
  N_by.initial := lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N"),
  by = .(initial_suffix_greenergy, mean.decile_by.ids)
]
dt_suffix.change_to.plot[
  ,
  N_by.final := lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N"),
  by = .(final_suffix_greenergy, mean.decile_by.ids)
]
dt_suffix.change_to.plot[
  ,
  `:=` (
    ratio_initial = round(N / N_by.initial * 100, digits = 1),
    ratio_final = round(N / N_by.final * 100, digits = 1)
  )
]
dt_suffix.change_to.plot[
  ,
  `:=` (
    ratio_initial_in.str = paste0(
      format(N, big.mark = ",", justify = "none"), "\n(", ratio_initial, "%)"
    ),
    ratio_final_in.str = paste0(
      format(N, big.mark = ",", justify = "none"), "\n(", ratio_final, "%)"
    )
  )
]
# # 3.2.2. Only for Early of 2010
# ## Note: Code in 3.2.1. is re-used.
dt_suffix.change_to.plot_early.2010 <- dt_suffix.change[
  mean.decile_by.ids %in% labels_deciles &
    year.month_final_suffix_greenergy %in%
      as.yearmon(c("Jan 2010", "Feb 2010", "Mar 2010", "Apr 2010")),
  .N,
  by = .(
    category_greenergy, initial_suffix_greenergy, final_suffix_greenergy,
    mean.decile_by.ids
  )
]
dt_suffix.change_to.plot_early.2010[
  ,
  N_by.initial := lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N"),
  by = .(initial_suffix_greenergy, mean.decile_by.ids)
]
dt_suffix.change_to.plot_early.2010[
  ,
  N_by.final := lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N"),
  by = .(final_suffix_greenergy, mean.decile_by.ids)
]
dt_suffix.change_to.plot_early.2010[
  ,
  `:=` (
    ratio_initial = round(N / N_by.initial * 100, digits = 1),
    ratio_final = round(N / N_by.final * 100, digits = 1)
  )
]
dt_suffix.change_to.plot_early.2010[
  ,
  `:=` (
    ratio_initial_in.str = paste0(
      format(N, big.mark = ",", justify = "none"), "\n(", ratio_initial, "%)"
    ),
    ratio_final_in.str = paste0(
      format(N, big.mark = ",", justify = "none"), "\n(", ratio_final, "%)"
    )
  )
]


# ------- DTs for Residential Rate Changes -------
# # 1. Make a DT for Residential Rate Changes
# # 1.1. Create objects that will be used later
cols_to.subset <- dt_pv.and.greenergy[
  str_detect(category_rate.change, "(RW)|(Stay)", negate = TRUE),
  .N, by = .(category_rate.change)
]$category_rate.change
tmp_year.month_base <- dt_pv.and.greenergy[
  category_rate.change %in% cols_to.subset,
  .N, keyby = .(year.month_final_rate.code, category_rate.change)
]$year.month_final_rate.code

tmp_year.month <- seq(
  min(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  max(tmp_year.month_base, na.rm = TRUE) %>% as.yearmon(.) %>% as.Date(.),
  by = "month"
) %>% as.yearmon(.)
# ## Note: To avoid missing some year-months.

# # 1.2. Make a temporary DT
tmp_dt_rate.change <- data.table(
  year.month_final_rate.code =
    rep(tmp_year.month, each = length(cols_to.subset)),
  category_rate.change = rep(cols_to.subset, times = length(tmp_year.month))
)
tmp_dt_rate.change[
  ,
  year.month_final_rate.code :=
    as.yearmon(year.month_final_rate.code)
]

# # 1.3. Create a DT from the temporary DT
dt_rate.change <- dt_pv.and.greenergy[
  ,
  .N, keyby = .(year.month_final_rate.code, category_rate.change)
][
  tmp_dt_rate.change,
  on = .(year.month_final_rate.code, category_rate.change)
]
dt_rate.change[is.na(N), N := 0]


# # 1.4. Modify the DT created
# # 1.4.1. Add a column showing normalized code regarding rate changes
dt_rate.change[
  category_rate.change %in% c("RSCH to RSEH", "RSEH to RSCH"),
  category_rate.change_normalize := "RSCH/RSEH to RSCH/RSEH"
]
dt_rate.change[
  category_rate.change %in% c("RSCH to RSGH", "RSEH to RSGH"),
  category_rate.change_normalize := "RSCH/RSEH to RSGH"
]
dt_rate.change[
  category_rate.change %in% c("RSGH to RSCH", "RSGH to RSEH"),
  category_rate.change_normalize := "RSCH/RSEH to RSGH"
]
# # 1.4.2. Convert a column's data type from string to factor
dt_rate.change[
  ,
  category_rate.change_normalize := factor(
    category_rate.change_normalize,
    levels = c("RSCH/RSEH to RSGH", "RSCH/RSEH to RSCH/RSEH")
  )
]


# --------------------------------------------------
# Create ggplot objects
# --------------------------------------------------
# ------- Make a ggplot object: PV Adoption -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot_pv.adoption <-
  ggplot(
    data = dt_pv.adoption,
    aes(x = year.month_pv.adoption_by.ids, y = N, fill = category_greenergy)
  ) +
    geom_area(alpha = 0.5) +
    scale_x_yearmon(format = "%Y", n = 10) +
    scale_y_continuous(breaks = seq(0, 350, by = 50)) +
    labs(
      x = "",
      y = "Number of Households adopting PV System",
      fill = "Greenergy Program\nParticipation"
    ) +
    scale_fill_manual(values = color.palette) +
    theme_linedraw() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
# ## Note: At 2005, there were exceptionally many installation.


# ------- Make a ggplot object: For before-after consumption comparision w.r.t. PV Installation -------
plot_qty.comparision_pv <-
  ggplot(
    data = dt_qty_pv,
    aes(
      x = mean.consumption_before, y = mean.consumption_after,
      color = category_greenergy
    )
  ) +
    geom_point(size = 1, alpha = 0.3) +
    geom_abline(
      slope = 1,
      color = "grey70", linetype = "dashed", alpha = 0.7
    ) +
    labs(
      x = "Summer-Season Average Consumption before Installing PV System (kWh/Month)",
      y = "Summer-Season Average Consumption after Installing PV System (kWh/Month)",
      color = "Greenergy\nProgram\nParticipation"
    ) +
    scale_x_continuous(
      labels = scales::comma,
      breaks = seq(0, 4000, by = 1000),
      limits = c(0, 4000)
    ) +
    scale_y_continuous(
      labels = scales::comma,
      breaks = seq(0, 4000, by = 1000),
      limits = c(0, 4000)
    ) +
    scale_color_brewer(palette = "Spectral") +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"), legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2,byrow = TRUE))
# ## Note: It is highly likely that values of "kwh_total" are net consumption.


# ------- Make a ggplot object: Months from PV Installation to Change in Participation -------
cols_to.subset <- c("Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy")
plot_diff.in.month <-
  ggplot(
    data = dt_pv.and.greenergy[
      !is.na(first.date_pv.adoption_by.ids) &
        !is.na(first.date_final_suffix_greenergy) &
        n_billing.cycle_suffix.change_before > 0,
        # ## Note: To exclude households staying on the same rate code.
      .(
        category_greenergy,
        first.date_pv.adoption_by.ids, first.date_final_suffix_greenergy
      )
    ][
      , diff.in.month :=
          as.numeric(
            first.date_final_suffix_greenergy - first.date_pv.adoption_by.ids
          ) / (365 / 12)
    ]
  ) +
    geom_histogram(
      aes(x = diff.in.month, y = ..density..),
      binwidth = 1, na.rm = TRUE,
      color = "grey70", fill = "white", alpha = 0.5
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed", alpha = 0.7
    ) +
    geom_density(
      aes(x = diff.in.month),
      adjust = 1/10, na.rm = TRUE,
      color = color.palette[1], alpha = 0.7
    ) +
    facet_wrap(. ~ category_greenergy, ncol = 1) +
    scale_x_continuous(breaks = seq(-12 * 8, 12 * 5.5, by = 12)) +
    labs(
      x = "Months from PV System Installation to Change in Greenergy Program Participation",
      y = "Density"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))
# ## Note: For "Greenergy to Non-Greenergy", the surge at 60 is caused by
# ## the large number of PV System Installation at 2005.


# ------- Make a ggplot object: Dates of PV Installation and Change in Greenergy Program Participation -------
plot_year.month_pv.and.greenergy <-
  ggplot(
    data = dt_pv.and.greenergy[
      !is.na(year.month_pv.adoption_by.ids) &
        !is.na(year.month_final_suffix_greenergy) &
        n_billing.cycle_suffix.change_before > 0,
        # ## Note: To exclude households staying on the same rate code.
      .N,
      by = .(
        category_greenergy,
        year.month_pv.adoption_by.ids,year.month_final_suffix_greenergy
      )
    ],
    aes(
      x = year.month_pv.adoption_by.ids,
      y = year.month_final_suffix_greenergy,
      alpha = N, size = N
    )
  ) +
    geom_point() +
    geom_abline(
      data = dt_for.abline, aes(slope = slope, intercept = intercept),
      linetype = "dashed", color = "grey70", alpha = 0.7
    ) +
    facet_wrap(. ~ category_greenergy, ncol = 1) +
    scale_x_yearmon(format = "%Y", n = 11) +
    scale_y_yearmon(format = "%Y", n = 11) +
    labs(
      x = "Year-Month of PV System Installation",
      y = "Year-Month of Change in Greenergy Program Participation"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold")) +
    guides(alpha = "none", size = "none")


# ------- Make a ggplot object: Changes in Greenergy Program Participation -------
plot_greenergy.participation <-
  ggplot(
    data = dt_greenergy.participation[
      ,
      category_greenergy := factor(
        category_greenergy,
        levels = c(
          "Non-Greenergy to Greenergy", "Greenergy to Non-Greenergy",
          "Greenergy to Greenergy"
        )
      )
    ],
    aes(
      x = year.month_final_suffix_greenergy, y = N,
      color = category_greenergy, shape = category_greenergy
    )
  ) +
    geom_point(size = 1.5) +
    geom_line() +
    scale_x_yearmon(format = "%Y", n = 10) +
    scale_y_continuous(
      labels = scales::comma_format(accuracy = 1), trans = "log10"
    ) +
    scale_color_manual(values = color.palette) +
    scale_shape_manual(values = c(16, 15, 18)) +
    labs(
      x = "",
      y = "Number of Households changing Greenergy Program Participation",
      color = "Greenergy Program\nParticipation",
      shape = "Greenergy Program\nParticipation"
    ) +
    theme_linedraw() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )


# ------- Make a ggplot object: Before-After Consumption Comparision w.r.t. Changes in Participation -------
suffixes_to.select <- c(
  "100% Green (1)", "50% Green (2)",
  "100% Green Flat Fee (11)", "50% Green Flat Fee (12)"
)
# ## Note: Other suffixes have very small number of observations.
plot_qty.comparision_greenergy <-
  ggplot(
    data = dt_qty_greenergy[
      (
        suffix_greenergy_before %in% suffixes_to.select |
          is.na(suffix_greenergy_before)
      ) & (
        suffix_greenergy_after %in% suffixes_to.select[3:4] |
          is.na(suffix_greenergy_after)
      )
    ],
    aes(x = mean.consumption_before, y = mean.consumption_after)
  ) +
    geom_point(size = 1, alpha = 0.1) +
    facet_grid(
      suffix_greenergy_after ~ suffix_greenergy_before, switch = "both"
    ) +
    scale_x_continuous(
      labels = scales::comma,
      breaks = seq(0, 3000, by = 1000), limits = c(0, 3000)
    ) +
    scale_y_continuous(
      labels = scales::comma,
      breaks = seq(0, 3000, by = 1000), limits = c(0, 3000)
    ) +
    geom_abline(
      slope = 1, color = "grey70", linetype = "dashed", alpha = 0.7
    ) +
    labs(
      x = "Summer-Season Average Consumption before changing Greenergy Program Participation (kWh/Month)",
      y = "Summer-Season Average Consumption after changing Greenergy Program Participation (kWh/Month)"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))


# ------- Make a ggplot object: Greenergy Program Suffix Changes -------


plot_suffix.change_initial <-
  ggplot(data = dt_suffix.change_to.plot[ratio_initial != 0 | ratio_final != 0]) +
    geom_label(
      aes(
        x = initial_suffix_greenergy, y = final_suffix_greenergy,
        label = ratio_initial_in.str,
        alpha = ratio_initial, fill = category_greenergy
      ),
      fontface = "bold", size = 2.5, color = "black"
    ) +
    labs(
      x = "Initial Greenergy Program Participation",
      y = "Final Greenergy Program Participation",
      fill = "Greenergy Program\nParticipation"
    ) +
    facet_wrap(. ~ mean.decile_by.ids, ncol = 2) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = color.palette) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    guides(size = "none", alpha = "none")


plot_suffix.change_final <-
  ggplot(data = dt_suffix.change_to.plot[ratio_initial != 0 | ratio_final != 0]) +
    geom_label(
      aes(
        x = initial_suffix_greenergy, y = final_suffix_greenergy,
        label = ratio_final_in.str,
        alpha = ratio_final, fill = category_greenergy
      ),
      fontface = "bold", size = 2.5, color = "black"
    ) +
    labs(
      x = "Initial Greenergy Program Participation",
      y = "Final Greenergy Program Participation",
      fill = "Greenergy Program\nParticipation"
    ) +
    facet_wrap(. ~ mean.decile_by.ids, ncol = 2) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = color.palette) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    guides(size = "none", alpha = "none")


# # 2.


plot_suffix.change_initial_early.2010 <-
  ggplot(data = dt_suffix.change_to.plot_early.2010[ratio_initial != 0 | ratio_final != 0]) +
    geom_label(
      aes(
        x = initial_suffix_greenergy, y = final_suffix_greenergy,
        label = ratio_initial_in.str,
        alpha = ratio_initial, fill = category_greenergy
      ),
      fontface = "bold", size = 2.5, color = "black"
    ) +
    labs(
      x = "Initial Greenergy Program Participation",
      y = "Final Greenergy Program Participation",
      fill = "Greenergy Program\nParticipation"
    ) +
    facet_wrap(. ~ mean.decile_by.ids, ncol = 2) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = color.palette) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    guides(size = "none", alpha = "none")


plot_suffix.change_final_early.2010 <-
  ggplot(data = dt_suffix.change_to.plot_early.2010[ratio_initial != 0 | ratio_final != 0]) +
    geom_label(
      aes(
        x = initial_suffix_greenergy, y = final_suffix_greenergy,
        label = ratio_final_in.str,
        alpha = ratio_final, fill = category_greenergy
      ),
      fontface = "bold", size = 2.5, color = "black"
    ) +
    labs(
      x = "Initial Greenergy Program Participation",
      y = "Final Greenergy Program Participation",
      fill = "Greenergy Program\nParticipation"
    ) +
    facet_wrap(. ~ mean.decile_by.ids, ncol = 2) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = color.palette) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    guides(size = "none", alpha = "none")


# ------- Make a ggplot object: Residential Rate Code Changes -------
plot_rate.change <-
  ggplot(
    data = dt_rate.change[
      ,
      lapply(.SD, sum, na.rm = TRUE), .SDcols = c("N"),
      by = .(year.month_final_rate.code, category_rate.change_normalize)
    ],
    aes(
      x = year.month_final_rate.code, y = N,
      fill = category_rate.change_normalize
    )
  ) +
    geom_area(color = "white", alpha = 0.6) +
    scale_x_yearmon(format = "%Y", n = 10) +
    scale_y_continuous(breaks = seq(0, 70, by = 10)) +
    scale_fill_manual(values = color.palette) +
    labs(x = "", y = "Number of Households changing Rate Code", fill = "") +
    theme_linedraw() +
    theme(legend.position = "bottom")


# --------------------------------------------------
# Save ggplot objects in PNG format
# --------------------------------------------------
# ------- Export ggplot objects in PNG format -------
# # 1. The ggplot object for PV Adoption
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE, "Greenergy-Program_PV-Installation.png", sep = "/"
  ),
  plot_pv.adoption,
  width = 30, height = 17, units = "cm"
)

# # 2. The ggplot object for before-after consumption comparision w.r.t.
# #    PV Installation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Before-After-Comparision_PV-Installation.png",
    sep = "/"
  ),
  plot_qty.comparision_pv,
  width = 30, height = 20, units = "cm"
)

# # 3. The ggplot object for Months from PV Installation to Change in
# #    Participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Months-from-PV-Installation-to-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_diff.in.month,
  width = 25, height = 17, units = "cm"
)

# # 4. The ggplot object for Dates of PV Installation and Change in Greenergy
# #    Program Participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Dates-of-PV-Installation-and-Change-in-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_year.month_pv.and.greenergy,
  width = 25, height = 17, units = "cm"
)

# # 5. The ggplot object for Changes in Greenergy Program Participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Changes-in-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_greenergy.participation,
  width = 30, height = 20, units = "cm"
)

# # 6. The ggplot object for Greenergy Program Suffix Changes
# # 6.1.
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Greenergy-Program-Suffix-Changes_Initial.png",
    sep = "/"
  ),
  plot_suffix.change_initial,
  width = 40, height = 45, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Greenergy-Program-Suffix-Changes_Final.png",
    sep = "/"
  ),
  plot_suffix.change_final,
  width = 40, height = 45, units = "cm"
)
# # 6.2.
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Greenergy-Program-Suffix-Changes_Initial_Early-2010.png",
    sep = "/"
  ),
  plot_suffix.change_initial_early.2010,
  width = 40, height = 45, units = "cm"
)
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Greenergy-Program-Suffix-Changes_Final_Early-2010.png",
    sep = "/"
  ),
  plot_suffix.change_final_early.2010,
  width = 40, height = 45, units = "cm"
)

# # 7. The ggplot object for before-after consumption comparision w.r.t.
# #    changes in participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Before-After-Comparision_Changes-in-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_qty.comparision_greenergy,
  width = 27, height = 35, units = "cm"
)

# # 8. The ggplot object for residential rate code changes
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Changes-in-Residential-Rate-Code.png",
    sep = "/"
  ),
  plot_rate.change,
  width = 30, height = 20, units = "cm"
)

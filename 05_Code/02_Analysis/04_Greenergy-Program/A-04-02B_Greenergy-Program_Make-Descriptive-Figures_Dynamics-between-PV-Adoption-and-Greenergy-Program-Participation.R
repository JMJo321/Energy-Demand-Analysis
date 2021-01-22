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
FILE_TO.LOAD_GN <- "DT_Greenergy-Program.RData"
PATH_TO.LOAD_GN <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN, sep= "/")

# # 1.2. PV-Adoption and Greenergy-Program-Participation Data
FILE_TO.LOAD_GN_PV <- "DT_Greenergy-Program_With-PV-Adoption.RData"
PATH_TO.LOAD_GN_PV <-
  paste(PATH_DATA_ANALYSIS, DIR_TO.LOAD_GN, FILE_TO.LOAD_GN_PV, sep= "/")


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
  "100% Green", "50% Green", "Community Solar",
  "Options 1 & 2", "Options 1 & 3", "Options 2 & 3", "Options 1, 2 & 3",
  "100% Green Flat Fee", "50% Green Flat Fee",
  "Options 3 & 11", "Options 3 & 12"
)


# ------- Define function(s) -------
# (NOT Applicable)


# --------------------------------------------------
# Load SMUD Datasets
# --------------------------------------------------
# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN)

# # 2. Check primary keys of the DT
stopifnot(
  dt_billing[
    , .N, by = .(ids, period_from, period_to, rate_code)
  ][
    N > 1, .N
  ] == 0
)


# ------- Load SMUD Billing Data -------
# # 1. Load a .RData file
load(file = PATH_TO.LOAD_GN_PV)

# 2. Check primary keys of the DT
stopifnot(dt_pv.and.greenergy[, .N, by = .(ids)][N > 1, .N] == 0)


# --------------------------------------------------
# Create DTs to make figures
# --------------------------------------------------
# ------- DT for before-after consumption comparision w.r.t. PV Installation  -------
# # 1. Make a temporary DT
tmp_dt_qty_pv <- dt_billing[
  is_pv.install == TRUE &
    season_before == "Summer" & season_before == season_after,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
  by = .(ids, is_pv, category_greenergy)
][
  , tmp_col := .N, by = .(ids, category_greenergy)
][
  tmp_col == 2 & !is.na(category_greenergy)
][
  , tmp_col := NULL
]

# # 2. Decast the temporary DT
dt_qty_pv <- dcast(tmp_dt_qty_pv, ids + category_greenergy ~ is_pv)

# # 3. Rename data fields
names_old <- c("FALSE", "TRUE")
names_new <- c("mean.consumption_before", "mean.consumption_after")
setnames(dt_qty_pv, names_old, names_new)


# ------- DT for Changes in Greenergy Program Suffixes -------
# # 1. Add a column showing consumption-based intervals
# # 1.1. Make a DT including calculate consumption by "ids"
dt_qty_quantile <- dt_billing[
  category_greenergy %in%
    c(
      "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy",
      "Greenergy to Greenergy"
    ),
  .(ids, season_before, season_after, kwh_total)
][
  season_before == "Summer" & season_before == season_after,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
  by = .(ids)
]

# # 1.2. Add a data field showing consumption-based intervals
# ## Calculate quantiles
quantiles <-
  dt_qty_quantile$kwh_total %>%
    quantile(., probs = seq(0, 1, by = 0.25)) %>%
    round(., digits = 0)
# ## Add a data field
dt_qty_quantile[
  ,
  quantile := cut(kwh_total, breaks = quantiles, dig.lab = 5)
][
  ,
  quantile :=
    str_replace(quantile, "\\(38,571\\]", "(0%, 25%]") %>%
      str_replace(., "\\(571,767\\]", "(25%, 50%]") %>%
      str_replace(., "\\(767,1016\\]", "(50%, 75%]") %>%
      str_replace(., "\\(1016,5848\\]", "(75%, 100%]")
]


# # 2. Make a DT including both changes in suffixes and consumption-based
# #    intervals
# # 2.1. Make a temporary DT
tmp_dt_suffix.change <- dt_pv.and.greenergy[
  category_greenergy %in%
    c(
      "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy",
      "Greenergy to Greenergy"
    )
]

# # 2.2. Merge the temporary DT with the DT incl. consumption-based intervals
dt_suffix.change <- dt_qty_quantile[tmp_dt_suffix.change, on = .(ids)]

# # 2.3. Convert columns' types from integer to factor
dt_suffix.change[
  ,
  `:=` (
    suffix_greenergy_before = factor(
      suffix_greenergy_before,
      labels = labels_suffixes, levels = levels_suffixes, ordered = TRUE
    ),
    suffix_greenergy_after = factor(
      suffix_greenergy_after,
      labels = labels_suffixes, levels = levels_suffixes, ordered = TRUE
    )
  )
]


# ------- Make a DT for before-after consumption comparision w.r.t. changes in participation -------
# # 1. Make a DT
# # 1.1. Create a temporary DT
tmp_dt_qty_greenergy <- dt_billing[
  is_pv.install == FALSE,
  .(ids, suffix_greenergy, period_from, season_before, season_after, kwh_total)
][
  dt_pv.and.greenergy[
    !is.na(date_greenergy.suffix.change),
    .(
      ids, date_greenergy.suffix.change,
      suffix_greenergy_before, suffix_greenergy_after
    )
  ],
  on = .(ids)
]

# # 1.2. Add an indicator variable showing whether each billing cycle is
# #      in Greenergy Program or not
tmp_dt_qty_greenergy[
  , is_greenergy := FALSE
][
  period_from >= date_greenergy.suffix.change,
  is_greenergy := TRUE
]

# # 1.3. Create a DT from the temporary
dt_qty_greenergy <-
  tmp_dt_qty_greenergy[
    season_before == "Summer" & season_before ==  season_after,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = c("kwh_total"),
    by = .(ids, suffix_greenergy_before, suffix_greenergy_after, is_greenergy)
  ] %>%
    dcast(
      ., ids + suffix_greenergy_before + suffix_greenergy_after ~ is_greenergy
    )


# # 2. Modify the DT created
# # 2.1. Rename columns
names_old <- c("FALSE", "TRUE")
names_new <- c("mean.consumption_before", "mean.consumption_after")
setnames(dt_qty_greenergy, names_old, names_new)

# # 2.2. Add data fields
# # 2.2.1. A data field showing the change in average consumptions
dt_qty_greenergy[
  , change_consumption := mean.consumption_after - mean.consumption_before
]
# # 2.2.2. An indicator variable showing whether average consumption increased
# #        or not
dt_qty_greenergy[change_consumption >= 0, is_increase := TRUE]
dt_qty_greenergy[change_consumption < 0, is_increase := FALSE]

# # 2.3. Conver columns' data type from integer to factor
dt_qty_greenergy[
  ,
  `:=` (
    suffix_greenergy_before = factor(
      suffix_greenergy_before,
      levels = levels_suffixes, labels = labels_suffixes
    ),
    suffix_greenergy_after = factor(
      suffix_greenergy_after,
      levels = levels_suffixes, labels = labels_suffixes
    )
  )
]


# ------- Make a DT to add straight lines -------
dt_for.abline <- data.table(
  "category_greenergy" = rep(
    c("Non-Greenergy to Greenergy", "Greenergy to Non-Greenergy"),
    times = 2
  ),
  "slope" = rep(c(1), times = 4),
  "intercept" = c(0, 0, -2, 5)
)


# --------------------------------------------------
# Create ggplot objects
# --------------------------------------------------
# ------- Make a ggplot object: PV Adoption -------
plot_pv.adoption <-
  ggplot(
    data = dt_pv.and.greenergy[
      category_greenergy %in%
        c(
          "Greenergy to Non-Greenergy", "Non-Greenergy to Greenergy",
          "Non-Greenergy"
        )
    ][
      !is.na(category_greenergy) & !is.na(year.month_pv.install),
      .N,
      keyby = .(year.month_pv.install, category_greenergy)
    ]
  ) +
    geom_point(aes(x = year.month_pv.install, y = N), size = 1) +
    facet_wrap(. ~ category_greenergy, ncol = 1) +
    scale_x_yearmon(format = "%Y", n = 10) +
    scale_y_continuous(
      trans = "log10", limits = c(1, 500), breaks = c(1, 10, 100, 300)
    ) +
    labs(x = "", y = "Number of Households adopting PV System") +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))
# ## Note: At 2005, there were exceptionally many installation.


# ------- Make a ggplot object: For before-after consumption comparision w.r.t. PV Installation -------
plot_qty_pv <-
  ggplot(
    data = dt_qty_pv,
    aes(x = mean.consumption_before, y = mean.consumption_after)
  ) +
    geom_point(size = 1, alpha = 0.1) +
    geom_abline(
      slope = 1,
      color = "grey70", linetype = "dashed", alpha = 0.7
    ) +
    labs(
      x = "Summer-Season Average Consumption before Installing PV System (kWh/Month)",
      y = "Summer-Season Average Consumption after Installing PV System (kWh/Month)"
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
    facet_wrap(. ~ category_greenergy, ncol = 2) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))
# ## Note: It is highly likely that values of "kwh_total" are net consumption.


# ------- Make a ggplot object: Months from PV Installation to Change in Participation -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot_diff.in.month <-
  ggplot(
    data = dt_pv.and.greenergy[
      !is.na(date_pv.install) & !is.na(date_greenergy.suffix.change),
      .(category_greenergy, date_pv.install, date_greenergy.suffix.change)
    ][
      , diff.in.month :=
          as.numeric(date_greenergy.suffix.change - date_pv.install) /
            (365 / 12)
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
plot_pv.and.greenergy <-
  ggplot(
    data = dt_pv.and.greenergy[
      !is.na(year.month_pv.install) &
        !is.na(year.month_greenergy.suffix.change),
      .N,
      by = .(
        category_greenergy,
        year.month_pv.install, year.month_greenergy.suffix.change
      )
    ],
    aes(
      x = year.month_pv.install,
      y = year.month_greenergy.suffix.change,
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
      x = "Date of PV System Installation",
      y = "Date of Change in Greenergy Program Participation",
      alpha = "Counts", size = "Counts"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))


# ------- Make a ggplot object: Changes in Greenergy Program Participation -------
plot_greenergy <-
  ggplot(
    data = dt_pv.and.greenergy[
      !is.na(year.month_greenergy.suffix.change),
      .N,
      by = .(category_greenergy, year.month_greenergy.suffix.change)
    ]
  ) +
    geom_point(
      aes(x = year.month_greenergy.suffix.change, y = N),
      size = 1
    ) +
    facet_wrap(. ~ category_greenergy, ncol = 1) +
    scale_x_yearmon(format = "%Y", n = 10) +
    scale_y_continuous(
      labels = scales::comma_format(accuracy = 1), trans = "log10"
    ) +
    labs(
      x = "",
      y = "Number of Households changing Greenergy Program Participation"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))


# ------- Make a ggplot object: Greenergy Program Suffix Changes -------
plot_suffix.change <-
  ggplot(data = dt_suffix.change[!is.na(quantile)]) +
    geom_jitter(
      aes(x = suffix_greenergy_before, y = suffix_greenergy_after),
      size = 1, alpha = 0.1
    ) +
    labs(
      x = "Initial Greenergy Program Participation",
      y = "Final Greenergy Program Participation"
    ) +
    facet_wrap(. ~ quantile, ncol = 2) +
    scale_y_discrete(limits = rev) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(face = "bold")
    )


# ------- Make a ggplot object: Before-After Consumption Comparision w.r.t. Changes in Participation -------
suffixes_select <- c(
  "100% Green", "50% Green", "100% Green Flat Fee", "50% Green Flat Fee"
)
# ## Note: Other suffixes have very small number of observations.
plot_qty_greenergy <-
  ggplot(
    data = dt_qty_greenergy[
      (
        is.na(suffix_greenergy_before) |
          suffix_greenergy_before %in% suffixes_select
      ) & (
        is.na(suffix_greenergy_after) |
          suffix_greenergy_after %in% suffixes_select[3:4]
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
  width = 20, height = 17, units = "cm"
)

# # 2. The ggplot object for before-after consumption comparision w.r.t.
# #    PV Installation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Before-After-Comparision_PV-Installation.png",
    sep = "/"
  ),
  plot_qty_pv,
  width = 25, height = 17, units = "cm"
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
  plot_pv.and.greenergy,
  width = 25, height = 17, units = "cm"
)

# # 5. The ggplot object for Changes in Greenergy Program Participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Changes-in-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_greenergy,
  width = 20, height = 17, units = "cm"
)

# # 6. The ggplot object for Greenergy Program Suffix Changes
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Greenergy-Program-Suffix-Changes.png",
    sep = "/"
  ),
  plot_suffix.change,
  width = 25, height = 20, units = "cm"
)

# # 7. The ggplot object for before-after consumption comparision w.r.t.
# #    changes in participation
plot.save(
  paste(
    DIR_TO.SAVE_FIGURE,
    "Greenergy-Program_Before-After-Comparision_Changes-in-Greenergy-Program-Participation.png",
    sep = "/"
  ),
  plot_qty_greenergy,
  width = 30, height = 25, units = "cm"
)

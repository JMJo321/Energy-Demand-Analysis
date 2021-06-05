# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-03A
# #
# > Purpose of the script(s)
# # : Make plots with respect to CTU Rate History.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(unikn)
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
# # 1. Path(s) for data file(s)
DIR_TO.LOAD_HISTORY <- "CTU/Rate-History"
FILE_TO.LOAD_HISTORY <- "CTU_Residential-Rate-History_Panel.RData"
PATH_TO.LOAD_HISTORY <-
  paste(
    PATH_DATA_INTERMEDIATE, DIR_TO.LOAD_HISTORY, FILE_TO.LOAD_HISTORY,
    sep = "/"
  )

# # 2. Path(s) at which plots will be saved
DIR_TO.SAVE_PLOTS <- paste(PATH_NOTE, "06_CTU-Rate-History", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Load a Data File required
# --------------------------------------------------
# ------- Load a data file required -------
load(PATH_TO.LOAD_HISTORY)

# ## Check primary keys of the DT loaded
keys <- c("utility", "rate_category", "rate_item", "base_item", "date")
stopifnot(rate.history_panel[, .N, by = keys][N > 1, .N] == 0)


# --------------------------------------------------
# Set Common Plot Options
# --------------------------------------------------
# ------- Set common plot options -------
plot.options <- list(
  theme_linedraw(),
  scale_x_date(date_labels = "%Y", date_breaks = "1 year"),
  theme(strip.text = element_text(face = "bold"))
)


# ------- Set color palette -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)


# --------------------------------------------------
# Make Plots: for Electricity
# --------------------------------------------------
# ------- Make Plots: for Electricity - Base Rates -------
# # 1. Modify the DT loaded
# # 1.1. For customer charges
# # 1.1.1. Add a temporary column
rate.history_panel[
  utility == "Electricity" & rate_type == "Base Rate" &
    str_detect(rate_item, "^Customer Charge"),
  tmp_customer.charge := str_replace(rate_item, ": ", ":\n")
]
# # 1.1.2. Convert column's data type from character to factor
rate.history_panel[
  ,
  tmp_customer.charge := factor(
    tmp_customer.charge,
    levels = c(
      "Customer Charge:\nThree-Phase", "Customer Charge:\nSingle-Phase"
    )
  )
]

# # 1.2. For customer charges
# # 1.2.1. Add a temporary column
rate.history_panel[
  utility == "Electricity" & rate_type == "Base Rate" &
    str_detect(rate_item, "Energy Charge"),
  tmp_energy.charge := rate_item
]
# # 1.2.2. Convert column's data type from character to factor
rate.history_panel[
  ,
  tmp_energy.charge := factor(
    tmp_energy.charge,
    levels = c(
      "Nights & Weekends Energy Charge On-Peak",
      "Non-Fuel Energy Charge",
      "Nights & Weekends Energy Charge Off-Peak"
    )
  )
]


# # 2. Create ggplot objects
# # 2.1. Customer charges
plot_electricity_base.rates_customer.charges <-
  ggplot() +
    geom_line(
      data = rate.history_panel[
        utility == "Electricity" & rate_type == "Base Rate" &
          str_detect(rate_item, "^Customer Charge")
      ],
      aes(x = date, y = rate_in_usd, color = tmp_customer.charge)
    ) +
    plot.options +
    facet_grid(tmp_customer.charge ~ ., scale = "free_y") +
    theme(
      strip.text.y = element_blank(),
      legend.text = element_text(margin = margin(t = 7))
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$ per Month", color = "Rate Item(s)")

# # 2.2. Energy charges
plot_electricity_base.rates_energy.charges <-
  ggplot() +
    geom_line(
      data = rate.history_panel[
        utility == "Electricity" & rate_type == "Base Rate" &
          str_detect(rate_item, "Energy Charge")
      ],
      aes(x = date, y = rate_in_usd, color = tmp_energy.charge)
    ) +
    plot.options +
    facet_grid(tmp_energy.charge ~ ., scale = "free_y") +
    theme(strip.text.y = element_blank()) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$/kWh", color = "Rate Item(s)")


# ------- Make Plots: for Electricity - ECRC Rates -------
# # 1. Modify the DT loaded
# # 1.1. Add a temporary column
rate.history_panel[
  utility == "Electricity" & rate_type == "ECRC",
  tmp_ecrc := rate_item
]
# # 1.2. Convert column's data type from character to factor
rate.history_panel[
  ,
  tmp_ecrc := factor(
    tmp_ecrc,
    levels = c(
      "ECRC: Nights & Weekends On-Peak",
      "ECRC",
      "ECRC: Nights & Weekends Off-Peak"
    )
  )
]

# # 2. Create ggplot an object
plot_electricity_ecrc <-
  ggplot() +
    geom_line(
      data = rate.history_panel[utility == "Electricity" & rate_type == "ECRC"],
      aes(x = date, y = rate_in_usd, color = tmp_ecrc)
    ) +
    plot.options +
    scale_y_continuous(
      limits = c(-0.001, 0.08), breaks = seq(0, 0.08, by = 0.02),
      labels = scales::comma
    ) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$/kWh", color = "Rate Item(s)")


# --------------------------------------------------
# Make Plots: for Natural Gas
# --------------------------------------------------
# ------- Make Plots: for Natural Gas - Base Rates -------
# # 1. Modify the DT loaded
# # 1.1. Add a temporary column
rate.history_panel[
  utility == "Natural Gas" & rate_type == "Base Rate" &
    str_detect(rate_item, "Energy"),
  tmp_gas.charge := rate_item
]

# # 2. Create ggplot objects
# # 2.1. For customer charge
plot_gas_base.rates_customer.charge <-
  ggplot() +
    geom_line(
      data = rate.history_panel[
        utility == "Natural Gas" & rate_type == "Base Rate" &
          str_detect(rate_item, "Customer")
      ],
      aes(x = date, y = rate_in_usd, color = rate_item)
    ) +
    plot.options +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$ per Month", color = "") +
    guides(color = "none")
# # 2.2. For energy charges
plot_gas_base.rates_energy.charge <-
  ggplot() +
    geom_line(
      data = rate.history_panel[
        utility == "Natural Gas" & rate_type == "Base Rate" &
          str_detect(rate_item, "Energy")
      ],
      aes(x = date, y = rate_in_usd, color = rate_item)
    ) +
    plot.options +
    facet_grid(tmp_gas.charge ~ ., scale = "free_y") +
    theme(strip.text.y = element_blank()) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$/CCF", color = "Rate Item(s)")


# ------- Make Plots: for Natural Gas - PGRC Rate -------
# # 1. Create ggplot an object
plot_gas_pgrc <-
  ggplot() +
    geom_line(
      data = rate.history_panel[utility == "Natural Gas" & rate_type == "PGRC"],
      aes(x = date, y = rate_in_usd, color = rate_item)
    ) +
    plot.options +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$/CCF", color = "") +
    guides(color = "none")


# --------------------------------------------------
# Make Plots: for Water
# --------------------------------------------------
# ------- Make Plots: for Water - Base Rates -------
# # 1. Modify the DT loaded
# # 1.1. Add a temporary column
rate.history_panel[
  utility == "Water" & rate_type == "Base Rate",
  tmp_gallonage.charge := str_extract(rate_item, "[0-9]")
]
# # 1.2. Convert column's data type from character to factor
rate.history_panel[
  ,
  tmp_gallonage.charge := factor(
    tmp_gallonage.charge, levels = c("3", "2", "1")
  )
]

# # 2. Create ggplot objects
# ## Note: There is no fixed charge with respect to water
# # 2.1. For Gallonage Charge
plot_water_gallonage.charge <-
  ggplot() +
    geom_line(
      data = rate.history_panel[utility == "Water" & rate_type == "Base Rate"],
      aes(x = date, y = rate_in_usd, color = tmp_gallonage.charge)
    ) +
    plot.options +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "US$ per 100 Gallons", color = "Tiers")

# # 2.2. For Upper Bounds of Tiers
plot_water_tier.qty <-
  ggplot(
    data = rate.history_panel[
      utility == "Water" & str_detect(rate_item, "Gallonage"),
      .(date, tier, qty_upper)
    ][tier != 3][, tier := factor(tier, levels = c(2, 1))]
  ) +
    geom_line(aes(x = date, y = qty_upper, color = tier)) +
    plot.options +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = color.palette) +
    labs(x = "", y = "Tier's Upper Bound (100 Gallons)", color = "Tiers")


# --------------------------------------------------
# Save Plots
# --------------------------------------------------
# ------- Save plots in PNG format -------
# # 1. Plots for Electricity
# # 1.1. For customer charges
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Electricity_Fixed-Charge.png",
    sep = "/"
  ),
  plot_electricity_base.rates_customer.charges,
  width = 35, height = 25, units = "cm"
)
# # 1.2. For energy charges
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Electricity_Variable-Charge.png",
    sep = "/"
  ),
  plot_electricity_base.rates_energy.charges,
  width = 35, height = 25, units = "cm"
)
# # 1.3. For ECRCs
plot.save(
  paste(DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Electricity_ECRC.png", sep = "/"),
  plot_electricity_ecrc,
  width = 35, height = 20, units = "cm"
)

# # 2. Plots for Natural Gas
# # 2.1. For customer charge
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Natural-Gas_Fixed-Charge.png", sep = "/"
  ),
  plot_gas_base.rates_customer.charge,
  width = 35, height = 17, units = "cm"
)
# # 2.2. For energy charges
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Natural-Gas_Variable-Charge.png",
    sep = "/"
  ),
  plot_gas_base.rates_energy.charge,
  width = 35, height = 20, units = "cm"
)
# # 2.3. For PGRC
plot.save(
  paste(DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Natural-Gas_PGRC.png", sep = "/"),
  plot_gas_pgrc,
  width = 35, height = 17, units = "cm"
)

# # 3. Plots for Water
# # 3.1. For gallonage charge
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Water_Variable-Charge.png", sep = "/"
  ),
  plot_water_gallonage.charge,
  width = 35, height = 20, units = "cm"
)
# # 3.2. For upper bounds of tiers
plot.save(
  paste(
    DIR_TO.SAVE_PLOTS, "CTU-Rate-History_Water_Upper-Bounds-of-Tiers.png", sep = "/"
  ),
  plot_water_tier.qty,
  width = 35, height = 20, units = "cm"
)
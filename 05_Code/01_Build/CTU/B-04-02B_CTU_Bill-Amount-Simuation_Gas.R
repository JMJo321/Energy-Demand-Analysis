# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, CTU
# #
# > Script Number(s)
# # : B-04-02B
# #
# > Purpose of the script(s)
# # : Simulate CTU Bill Amount for Natural Gas

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
library(stringr)
library(parallel)
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
    paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Data File(s) is(are) loaded
# # 1.1. CTU Rate History
FILE_TO.LOAD_CTU_RATE.HISTORY <- "CTU_Residential-Rate-History_Panel.RData"
DIR_TO.LOAD_CTU_RATE.HISTORY <- "CTU/Rate-History"
PATH_TO.LOAD_CTU_RATE.HISTORY <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.LOAD_CTU_RATE.HISTORY,
  PATH_TO.LOAD_CTU_RATE.HISTORY,
  sep = "/"
)

# # 2. Path(s) to which Plot(s) is(are) saved
DIR_TO.SAVE_CTU_PLOT <- "06_CTU-Rate-History"
PATH_TO.SAVE_CTU_PLOT <- paste(
  PATH_NOTE, DIR_TO.SAVE_CTU_PLOT, sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Create a list of rate items
list_rate.item.info <- list(
  `Electricity` = list(
    `Customer Charge: Single-Phase` = c("customer_single", "fixed"),
    `Customer Charge: Three-Phase` = c("customer_three", "fixed"),
    `Non-Fuel Energy Charge` = c("energy", "variable"),
    `ECRC` = c("ecrc", "vairable"),
    `FL Gross Receipts Tax` = c("grt_electricity", "tax"),
    `City Public Service Tax (Inside City)` =
      c("public.service_electricity_inside", "tax"),
    `Leon County Public Service Tax (Outside City)` =
      c("public.service_electricity_leon", "tax"),
    `Wakulla County Public Service Tax (Outside City)` =
      c("public.service_electricity_wakulla", "tax")
  ),
  `Natural Gas` = list(
    `Customer Charge` = c("customer", "fixed"),
    `Non-Fuel Energy Charge` = c("energy", "variable"),
    `Space Cooling Non-Fuel Energy Charge` = c("space.cooling", "variable"),
    `PGRC` = c("pgrc", "variable"),
    `FL Gross Receipts Tax` = c("grt_gas", "tax"),
    `GRT Index` = c("grt.index", "index"),
    `City Public Service Tax (Inside City)` =
      c("public.service_gas_inside", "tax"),
    `Leon County Public Service Tax (Outside City)` =
      c("public.service_gas_leon", "tax"),
    `Wakulla County Public Service Tax (Outside City)` =
      c("public.service_gas_wakulla", "tax")
  )
)

# # 2. Number of Cores that will be used for `mcapply`
N_CORES <- 6

# # 3. Parameters to compute bill amount
# # 3.1. Natural gas Consumption Level
QTY_FOR.BILL.AMOUNT <- 30


# ------- Define function(s) -------
# # 1. Functions to compute bill amount for a given amount of consumption level
# # 1.1. Helper functions
# # 1.1.1. Function to compute fixed amount
bill.amount_gas_fixed <- function (rate.item, date_) {
  rate <- rate.history_panel[
    utility == "Natural Gas" & rate_item == rate.item &
      date == date_
  ]$rate_in_usd
  rate.code <- list_rate.item.info[["Natural Gas"]][[rate.item]][1]
  rate.class <- list_rate.item.info[["Natural Gas"]][[rate.item]][2]
  amount <- rate
  output <- list(
    `utility` = "Natural Gas",
    `rate.code` = rate.code,
    `rate.class` = rate.class,
    `rate` = rate,
    `amount` = amount
  )
  return (output)
}
# # 1.1.2. Function to compute variable amount
bill.amount_gas_variable <- function (rate.item, date_, qty_in_ccf) {
  rate <- rate.history_panel[
    utility == "Natural Gas" & rate_item == rate.item &
      date == date_
  ]$rate_in_usd
  rate.code <- list_rate.item.info[["Natural Gas"]][[rate.item]][1]
  rate.class <- list_rate.item.info[["Natural Gas"]][[rate.item]][2]
  amount <- rate * qty_in_ccf
  output <- list(
    `utility` = "Natural Gas",
    `rate.code` = rate.code,
    `rate.class` = rate.class,
    `rate` = rate,
    `amount` = amount
  )
  return (output)
}
# # 1.1.3. Function to compute FL Gross Receipts Tax (GRT)
bill.amount_gas_others_grt <- function(date_, qty_in_ccf) {
  percent_ccf <- rate.history_panel[
    utility == "Natural Gas" & rate_item == "FL Gross Receipts Tax" &
      base_item == "Billed CCF" & date == date_
  ]$percent
  rate_grt.index <- rate.history_panel[
    utility == "Natural Gas" & rate_item == "GRT Index" & date == date_ &
      rate_category == "Residential"
  ]$rate_in_usd
  rate.code <-
    list_rate.item.info[["Natural Gas"]][["FL Gross Receipts Tax"]][1]
  rate.class <-
    list_rate.item.info[["Natural Gas"]][["FL Gross Receipts Tax"]][2]
  amount <- percent_ccf * rate_grt.index * qty_in_ccf
  output <- list(
    `utility` = "Natural Gas",
    `rate.code` = rate.code,
    `rate.class` = rate.class,
    `rate` = list(
      `base.rate` = percent_ccf,
      `grt.index` = rate_grt.index
    ),
    `amount` = amount
  )
  return (output)
}
# # 1.1.4. Function to compute Public Service Charge
bill.amount_gas_others_public.service <-
  function(
    item_customer.charge, item_energy.charge, date_, qty_in_ccf, county = NULL
  ) {
    if(!is.null(county)) {
      if (str_detect(county, "(Leon)|(leon)")) {
        percent_base <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Leon County Public Service Tax (Outside City)" &
            base_item == "Base Rate" &
            date == date_
        ]$percent
        percent_ccf <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Leon County Public Service Tax (Outside City)" &
            base_item == "Billed CCF" &
            date == date_
        ]$percent
        rate_ccf <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Leon County Public Service Tax (Outside City)" &
            base_item == "Billed CCF" &
            date == date_
        ]$rate_in_usd
        percent_grt <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Leon County Public Service Tax (Outside City)" &
            base_item == "FL Gross Receipts Tax" &
            date == date_
        ]$percent
        # TODO Check how to compute GRT for surcharge item(s)
        rate.code <-
          list_rate.item.info[["Natural Gas"]][["Leon County Public Service Tax (Outside City)"]][1]
        rate.class <-
          list_rate.item.info[["Natural Gas"]][["Leon County Public Service Tax (Outside City)"]][2]
      } else if (str_detect(county, "(Wakulla)|(wakulla)")) {
        percent_base <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Wakulla County Public Service Tax (Outside City)" &
            base_item == "Base Rate" &
            date == date_
        ]$percent
        percent_ccf <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Wakulla County Public Service Tax (Outside City)" &
            base_item == "Billed CCF" &
            date == date_
        ]$percent
        rate_ccf <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Wakulla County Public Service Tax (Outside City)" &
            base_item == "Billed CCF" &
            date == date_
        ]$rate_in_usd
        percent_grt <- rate.history_panel[
          utility == "Natural Gas" &
            rate_item == "Wakulla County Public Service Tax (Outside City)" &
            base_item == "FL Gross Receipts Tax" &
            date == date_
        ]$percent
        # TODO Check how to compute GRT for surcharge item(s)
        rate.code <-
          list_rate.item.info[["Natural Gas"]][["Wakulla County Public Service Tax (Outside City)"]][1]
        rate.class <-
          list_rate.item.info[["Natural Gas"]][["Wakulla County Public Service Tax (Outside City)"]][2]
      }
    } else {
      percent_base <- rate.history_panel[
        utility == "Natural Gas" &
          rate_item == "City Public Service Tax (Inside City)" &
          base_item == "Base Rate" &
          date == date_
      ]$percent
      percent_ccf <- rate.history_panel[
        utility == "Natural Gas" &
          rate_item == "City Public Service Tax (Inside City)" &
          base_item == "Billed CCF" &
          date == date_
      ]$percent
      rate_ccf <- rate.history_panel[
        utility == "Natural Gas" &
          rate_item == "City Public Service Tax (Inside City)" &
          base_item == "Billed CCF" &
          date == date_
      ]$rate_in_usd
      percent_grt <- rate.history_panel[
        utility == "Natural Gas" &
          rate_item == "City Public Service Tax (Inside City)" &
          base_item == "FL Gross Receipts Tax" &
          date == date_
      ]$percent
      # ## Note: NOT imposed on surcharges.
      rate.code <-
        list_rate.item.info[["Natural Gas"]][["City Public Service Tax (Inside City)"]][1]
      rate.class <-
        list_rate.item.info[["Natural Gas"]][["City Public Service Tax (Inside City)"]][2]
    }
    customer.charge <- bill.amount_gas_fixed(item_customer.charge, date_)
    energy.charge <- bill.amount_gas_variable(
      item_energy.charge, date_, qty_in_ccf
    )
    grt <- bill.amount_gas_others_grt(date_, qty_in_ccf)
    amount <-
      (customer.charge[["amount"]] + energy.charge[["amount"]]) * percent_base +
        (qty_in_ccf * rate_ccf) * percent_ccf +
        grt[["amount"]] * percent_grt
    output <- list(
      `utility` = "Natural Gas",
      `rate.code` = rate.code,
      `rate.class` = rate.class,
      `rate` = list(
        `base.rate` = percent_base,
        `grt` = percent_grt,
        `ccf` = percent_ccf
      ),
      `amount` = amount
    )
    return (output)
  }

# # 1.2. Function to compute the bill amount for a given gas consumption
bill.amount_gas <-
  function(
    item_customer.charge, item_energy.charge, item_pgrc, date_, qty_in_ccf,
    county = NULL
  ) {
    customer.charge <- bill.amount_gas_fixed(
      item_customer.charge, date_
    )
    energy.charge <- bill.amount_gas_variable(
      item_energy.charge, date_, qty_in_ccf
    )
    pgrc <- bill.amount_gas_variable(
      item_pgrc, date_, qty_in_ccf
    )
    # TODO Check about Space Cooling Non-Fuel Energy Charge
    grt <- bill.amount_gas_others_grt(date_, qty_in_ccf)
    public.service <- bill.amount_gas_others_public.service(
      item_customer.charge, item_energy.charge, date_, qty_in_ccf,
      county
    )
    total_fixed <- customer.charge[["amount"]]
    total_variable <- energy.charge[["amount"]] + pgrc[["amount"]]
    total_tax <- grt[["amount"]] + public.service[["amount"]]
    total_grand <- total_fixed + total_variable + total_tax
    output <- list(
      `utility` = "Natural Gas",
      `rate.class` = "total",
      `amount.by.class` = list(
        `fixed` = list(
          `customer.charge` = customer.charge[["amount"]],
          `total_fixed` =  total_fixed
        ),
        `variable` = list(
          `energy.charge` = energy.charge[["amount"]],
          `pgrc` = pgrc[["amount"]],
          `total_variable` = total_variable
        ),
        `tax` = list(
          `grt` = grt[["amount"]],
          `public.service` = public.service[["amount"]],
          `total_tax` = total_tax
        )
      ),
      `amount` = total_grand
    )
    return (output)
}

# # 1.3. Extract pieces of infromation from result obtained
# #      from `bill.amount_gas`
extract_bill.amount_gas <- function(date_, qty_in_ccf) {
  result <- bill.amount_gas(
    item_customer.charge = "Customer Charge",
    item_energy.charge = "Non-Fuel Energy Charge",
    item_pgrc = "PGRC",
    date = date_,
    qty_in_ccf = qty_in_ccf,
    county = NULL
  )
  total_fixed <- result[["amount.by.class"]][["fixed"]][["total_fixed"]]
  total_variable <- result[["amount.by.class"]][["variable"]][["total_variable"]]
  total_tax <- result[["amount.by.class"]][["tax"]][["total_tax"]]
  total_grand <- result[["amount"]]
  output <- list(
    `bill.amount_fixed` = total_fixed,
    `bill.amount_variable` = total_variable,
    `bill.amount_tax` = total_tax,
    `bill.amount_total` = total_grand
  )
  return(output)
}


# --------------------------------------------------
# Generate a DT for Simulated Bill Amount
# ------- Load CTU Bill History -------
load(PATH_TO.LOAD_CTU_RATE.HISTORY)


# ------- Create a DT to make plot(s) -------
# # 1. Make an empty DT
date_begin <- rate.history_panel[utility == "Natural Gas" & rate_item == "Customer Charge"]$date %>% min(.)
date_end <- rate.history_panel[utility == "Natural Gas" & rate_item == "Customer Charge"]$date %>% max(.)

dt_bill.amount <- data.table(
  date = seq.Date(from = date_begin, to = date_end, by = "day")
)

# # 2. Compute Bill Amount
dt_bill.amount <- dt_bill.amount[
  ,
  mclapply(
    date,
    function (x) extract_bill.amount_gas(x, QTY_FOR.BILL.AMOUNT),
    mc.cores = N_CORES
  ) %>% rbindlist(.)
] %>% cbind(dt_bill.amount, .)

# # 3. Modify the DT created above
# # 3.1. Melt the DT
dt_for.plot <- melt(
  data = dt_bill.amount,
  id.vars = "date",
  variable.name = "category",
  value.name = "bill.amount"
)
# # 3.2. Convert data type from character to factor
dt_for.plot[
  ,
  category := (
    str_extract(category, "(?<=_).+$") %>%
      str_to_title(.) %>%
      factor(., levels = c("Fixed", "Variable", "Tax"))
  )
]


# ------------------------------------------------------------------------------
# Make a Plot that illustrate Simulated Bill Amount
# ------------------------------------------------------------------------------
# ------- Set common plot options -------
color.palette <- unikn::usecol(pal = pal_signal, n = 3)
plot.options <- list(
  theme_linedraw(),
  scale_x_date(date_labels = "%Y", date_breaks = "1 year"),
  scale_y_continuous(
    breaks = seq(0, 150, by = 10),
    labels = seq(0, 150, by = 10)
  ),
  scale_fill_manual(values = color.palette),
  labs(
    x = "",
    y = "Bill Amount (US$)",
    fill = "",
    caption = paste(
      "Note: This figure is based on the consumption of 50 CCF/Month.",
      "Space Cooling Non-Fuel Energy Charge is NOT included.",
      sep = " "
    )
  )
)


# ------- Create ggplot object(s) -------
plot <-
  ggplot(data = dt_for.plot[category != "Total"]) +
    geom_area(aes(x = date, y = bill.amount, fill = category), alpha = 0.5) +
    plot.options


# ------- Save plot(s) created -------
plot.save(
  paste(
    PATH_TO.SAVE_CTU_PLOT,
    "CTU-Rate-History_Simulated-Bill-Amount_Gas.png",
    sep = "/"
  ),
  plot,
  width = 45, height = 25, units = "cm"
)

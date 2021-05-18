# < Description > *
# > Script Group Indicator Number and Name
# # : D
# #
# > Script Number(s)
# # : D-Energy-Demand-Analysis
# #
# > Purpose of the script(s)
# # : Make a list including labels for data fields in CTU datasets.

# --------------------------------------------------
# Load required libraries
# --------------------------------------------------
# (Not Applicable)


# --------------------------------------------------
# Set working directory, and run header script
# --------------------------------------------------
# ------- Set project name -------
# (Not Applicable)


# ------- Set working directory -------
# (Not Applicable)


# ------- Run the header script -------
# (Not Applicable)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# --------------------------------------------------
# Create a List containing Labels for Data Fields
# --------------------------------------------------
# ------- Create a List containing Labels for Data Fields -------
labels_ctu <- list(
  date = "Date",
  utility = "Utility, One among Electricity, Natural Gas, Storm Water, Garbage, Water, Sewer, and Fire Service",
  rate_category = "Category of Rate Item, One among Residential, Commercial, Industrial, and Common",
  rate_type = "Type of Rate Item, One among Base Rate, ECRC, Surcharge, PGRC, Tax, and Index",
  rate_item = "Rate Item",
  base_item = "Calculation Base for Tax or Surcharge",
  unit = "Unit of Measurement",
  tier = "Tier for Rate Item, if any",
  qty_lower = "Tier's Lower Limit in Qty",
  qty_upper = "Tier's Upper Limit in Qty",
  percent = "Percent for Tax/Surcharge Calculation",
  rate_in_usd = "Rate in US Dollar",
  description = "Description of Rate Item"
)


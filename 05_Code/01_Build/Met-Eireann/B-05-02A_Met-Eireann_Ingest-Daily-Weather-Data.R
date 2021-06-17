# < Description > *
# > Script Group Indicator Number and Name
# # : B-05, Met-Eireann
# #
# > Script Number(s)
# # : B-05-02A
# #
# > Purpose of the script(s)
# # : Ingest Met Eireann's Daily-Level Weather Data

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Energy-Demand-Analysis"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("05_Code/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Daily Weather Data files will be read
DIR_TO.LOAD_WEATHER <-
  paste(PATH_DATA_RAW_USE, "Met-Eireann/Daily-Data", sep = "/")

# # 2. Path(s) to which Output will be saved
DIR_TO.SAVE_WEATHER <- "Met-Eireann"
FILE_TO.SAVE_WEATHER <- "Met-Eireann_Weather-Data_Daily.parquet"
PATH_TO.SAVE_WEATHER <- paste(
  PATH_DATA_INTERMEDIATE,
  DIR_TO.SAVE_WEATHER,
  FILE_TO.SAVE_WEATHER,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Range of Years to subset Weather Data
RANGE_YEARS <- c(2000:2020)

# # 2. List of Station Name and Code
stations <- list(
  `1875` = "Athenry",
  `675` = "Ballyhaise",
  `2375` = "Belmullet",
  `3723` = "Casement Aerodrome",
  `2175` = "Claremorris",
  `2437` = "Clones",
  `3904` = "Cork Airport",
  `532` = "Dublin Airport",
  `1375` = "Dunsany",
  `2075` = "Finner",
  `1475` = "Gurteen",
  `1775` = "Johnstown",
  `275` = "Mace",
  `1575` = "Malin",
  `1275` = "Markree Castle",
  `575` = "Moore Park",
  `1975` = "Mount Dillon",
  `875` = "Mullingar",
  `1175` = "Newport Furnace",
  `375` = "Oak Park",
  `175` = "Phoenix Park",
  `1075` = "Roches Point SWS",
  `518` = "Shannon Airport",
  `775` = "Sherkin Island"
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT from Met Eireann's Hourly Weather Data
# ------------------------------------------------------------------------------
# ------- Create a DT by importing CSV files -------
# # 1. Make a list of CSV files
files <- list.files(DIR_TO.LOAD_WEATHER)
csv.files <- files[str_detect(files, ".csv$")]

# # 2. Create DTs by importing CSV files
for (file in csv.files) {
  tmp_name <- paste0(
    "dt_", (str_replace(file, ".csv", "") %>% str_replace(., "hly", ""))
  )
  tmp_path <- paste(DIR_TO.LOAD_WEATHER, file, sep = "/")
  tmp_code <- str_extract(file, "[0-9]+") %>% as.integer(.)

  assign(tmp_name, fread(file = tmp_path))
  get(tmp_name)[, station_code := tmp_code]
}

# # 3. Create a DT by combining DTs made above
dts <- ls()[str_detect(ls(), "^dt_")]
dt_weather_daily <-
  rbindlist(mget(dts) %>% as.list(.), use.names = TRUE, fill = TRUE)


# ------- Modify the DT created -------
# # 1. Rename columns
names_old <- c(
  "date", "ind", "maxtp", "ind", "mintp", "igmin", "gmin",
  "ind", "rain", "cbl", "wdsp", "ind", "hm", "ind", "ddhm", "ind", "hg",
  "sun", "dos", "g_rad", "soil", "pe", "evap", "smd_wd", "smd_md", "smd_pd",
  "station_code"
)
names_new <- c(
  "date", "imax", "maxtp_c", "imin", "mintp_c", "igmin", "gmin_c",
  "irain", "rain", "cbl", "wdsp", "ihm", "hm", "iddhm", "ddhm", "ihg", "hg",
  "sun", "dos", "g_rad", "soil_c", "pe", "evap", "smd_wd", "smd_md", "smd_pd",
  "station_code"
)
names(dt_weather_daily) <- names_new
# ## Note:
# ## There are columns that have the same name. They can be distinguished
# ## based on Met Eireann's data dictionary.


# # 2. Drop unnecessary columns
cols_keep <- c(
  "date", "imax", "maxtp_c", "imin", "mintp_c", "igmin", "gmin_c", "soil_c",
  "irain", "rain", "station_code"
)
names_keep <- names(dt_weather_daily)[names(dt_weather_daily) %in% cols_keep]
dt_weather_daily <- dt_weather_daily[, .SD, .SDcols = names_keep]


# # 3. Add columns
# # 3.1. Add columns that are related to date and time
# # 3.1.1. Add a column showing datetime
dt_weather_daily[
  ,
  date := as.Date(date, format = "%d-%b-%Y")
]
# # 3.1.2. Add columns showing year, month and day
dt_weather_daily[
  ,
  `:=` (
    year = year(date),
    month = month(date),
    day = lubridate::mday(date)
  )
]

# # 3.2. Add a column showing station names
for (code in names(stations)) {
  dt_weather_daily[
    station_code == as.integer(code),
    station := stations[[code]]
  ]
}

# # 3.3. Add a column that includes temperature in Fahrenheit
cols_convert <-
  names(dt_weather_daily)[str_detect(names(dt_weather_daily), "_c$")]
for (col in cols_convert) {
  tmp_col <- str_replace(col, "_c", "_f")
  dt_weather_daily[, eval(tmp_col) := get(col) * (9 / 5) + 32]
}


# # 4. Drop unnecessary observations
dt_weather_daily <- dt_weather_daily[year %in% RANGE_YEARS]


# # 5. Reorder columns
cols_reorder <- c(
  "station", "station_code", "date", "year", "month", "day", "irain", "rain",
  "imax", "maxtp_c", "maxtp_f", "imin", "mintp_c", "mintp_f",
  "igmin", "gmin_c", "gmin_f", "soil_c", "soil_f"
)
setcolorder(dt_weather_daily, cols_reorder)


# ------- Save the DT in Parquet Format -------
arrow::write_parquet(
  dt_weather_daily,
  sink = PATH_TO.SAVE_WEATHER,
  compression = "snappy",
  use_dictionary = TRUE
)

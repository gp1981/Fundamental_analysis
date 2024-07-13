# 01 - API, Packages and libraries -------------------------------------------------

packages <- c("httr","jsonlite","tidyverse", "openxlsx", "lubridate","tidyquant",
              "ggthemes","ggplot2","openxlsx","dplyr","zoo","ggpubr","foreach", 
              "progress", "fmpcloudr", "ggplot2", "kableExtra", "openxlsx", "knitr", 
              "zoo")

for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
  
  # Load the package
  library(package, character.only = TRUE)
}

# 02 - Utility functions -------------------------------------------------------

## API ---------------------------------------------------------------------

# Load API Key
API_Key = rstudioapi::askForSecret("API_FMP_KEY")
fmpc_set_token(API_Key, noBulkWarn = TRUE)

## Dates ---------------------------------------------------------------------

# Get today's date
today_date <- as.Date(Sys.Date())

# Calculate the historical dates
historical_dates <- data.frame(
  date_25Y = (today_date - years(25)),
  date_20Y = (today_date - years(20)),
  date_10Y = (today_date - years(10)),
  date_5Y = (today_date - years(5)),
  date_3Y = (today_date - years(3)),
  date_1Y = (today_date - years(1)),
  date_6M = (today_date - months(6)),
  date_1M = (today_date - months(1)),
  date_1W = (today_date - weeks(1))
)
# 
# # Files to source ----
source('script/data_retrieval.R')
source('script/analysis.R')
source('script/utils.R')
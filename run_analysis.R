# Source data and libraries -----------------------------------------------
source('Functions/setup.R')


# Retrieve data -----------------------------------------------------------

# Input: company ticker and competitors
company_ticker <- 'PHM'
competitors_tickers <- c('MSFT', 'GOOGL')
indexes = c('^SP500TR','JPYUSD','BTCUSD','GCUSD')


# Retrieve financial data
financial_data <- get_financial_data(company_ticker, competitors_tickers, indexes, historical_dates)

# Analysis ------------------------------------------------------------
ratio_analysis <- ratio_analysis_chart(financial_data)

# To do:
#   1) Working capital ratios
#   2) NOPLAT, and earnings
#   3) quarto web posting
#   4) median from industry for benchmark
# Source data and libraries -----------------------------------------------
source('script/setup.R')

# Retrieve data -----------------------------------------------------------

# Input: company ticker and competitors
company_ticker <- 'PHM'
competitors_tickers <- c('MSFT', 'GOOGL')
indexes = c('^SP500TR','JPYUSD','BTCUSD','GCUSD')


# Retrieve financial data
financial_data <- get_financial_data(company_ticker, competitors_tickers, indexes, historical_dates)

# Analysis ------------------------------------------------------------
ratio_analysis_plot <- ratio_analysis_chart(financial_data)

# Charts
ratio_analysis_plot$current_assets_plot

ratio_analysis_plot$cash_conversion_plot

ratio_analysis_plot$debt_ratios_plot

ratio_analysis_plot$debt_coverage_plot
# Source data and libraries -----------------------------------------------
source('script/setup.R')

# Retrieve data -----------------------------------------------------------

# Input: company ticker and competitors
company_ticker <- 'PHM'
competitors_tickers <- c('NVR', 'TOL', 'DHI', 'LEN')
indexes = c('^SP500TR','JPYUSD','BTCUSD','GCUSD')


# Retrieve financial data
financial_data_list <- get_financial_data(company_ticker, competitors_tickers, indexes, historical_dates)
financial_data_df <- financial_data_list$financial_data_df
price_history_data <- financial_data_list$price_history_data


# Ratio Analysis ------------------------------------------------------------
ratio_analysis_plot <- ratio_analysis_chart(financial_data_df)

# Charts
ratio_analysis_plot$current_assets_plot

ratio_analysis_plot$cash_conversion_plot

ratio_analysis_plot$debt_ratios_plot

ratio_analysis_plot$debt_coverage_plot

# Historical Total Shareholder Equity value ------------------------------------------------------------

# Reverse order and compute cumulative sums
financial_data_df1 <- financial_data_df %>%
  arrange(desc(row_number())) %>% # Reverse order
  mutate(
    rev_dividendsPaid = cumsum(dividendsPaid),
    rev_commonStockIssued = cumsum(commonStockIssued),
    rev_commonStockRepurchased = cumsum(commonStockRepurchased)
  ) %>%
  arrange(row_number()) %>% # Restore original order
  mutate(
    Historical_Equity_Value = (totalStockholdersEquity + 
      rev_dividendsPaid + rev_commonStockIssued - rev_commonStockRepurchased) / numberOfShares
  ) %>%
  select(-rev_dividendsPaid, -rev_commonStockIssued, -rev_commonStockRepurchased) %>%  # Remove temporary columns
  select(1:8, Historical_Equity_Value, everything())



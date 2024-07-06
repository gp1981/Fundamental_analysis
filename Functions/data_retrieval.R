
get_financial_data <- function(company_ticker, competitors_ticker, indexes, historical_dates) {

  symbols <- c(company_ticker, competitors_tickers)
  
# Price data --------------------------------------------------------------
  date_25Y <- historical_dates$date_25Y
  
  # Get historical prices
  price_history_data <- fmpc_price_history(symbols,
                                           startDate = date_25Y, endDate = today_date)
  

# Fundamentals data ----------------------------------------------------------
  
  # Balance Sheet
  BS = fmpc_financial_bs_is_cf(symbols, statement = 'balance', quarterly = TRUE)
  # BalG = fmpc_financial_bs_is_cf(symbols,statement = 'balance', growth = TRUE)
  
  # Income statements
  IS = fmpc_financial_bs_is_cf(symbols, statement = 'income', quarterly = TRUE, )
  # ISa = fmpc_financial_bs_is_cf(symbols,statement = 'income', quarterly = FALSE)
  
  # Cash Flow Statements
  CF = fmpc_financial_bs_is_cf(symbols, statement = 'cashflow', quarterly = TRUE)
  # cfsec = fmpc_financial_bs_is_cf('AAPL',statement = 'cashflow', SECReported  = TRUE)
  
  ### Other financial metrics
  # Financial Ratios
  ratio = fmpc_financial_metrics(symbols, metric = 'ratios', quarterly = TRUE)
  
  # Key Metrics
  keym = fmpc_financial_metrics(symbols, metric = 'key', quarterly = FALSE)
  
  # Enterprise Value
  ev = fmpc_financial_metrics(symbols, metric = 'ev', quarterly = TRUE)
  
  financial_data <- list(
    BS = BS, 
    IS = IS, 
    CF = CF, 
    Ratio = ratio, 
    KeyMetrics = keym, 
    Enterprise_Values = ev, 
    Price_History = price_history_data
  )
  
  return(financial_data)
}

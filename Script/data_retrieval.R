
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
  BS <- BS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  # Income statements
  IS = fmpc_financial_bs_is_cf(symbols, statement = 'income', quarterly = TRUE, )
  # ISa = fmpc_financial_bs_is_cf(symbols,statement = 'income', quarterly = FALSE)
  IS <- IS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  # Cash Flow Statements
  CF = fmpc_financial_bs_is_cf(symbols, statement = 'cashflow', quarterly = TRUE)
  # cfsec = fmpc_financial_bs_is_cf('AAPL',statement = 'cashflow', SECReported  = TRUE)
  CF <- CF %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  ### Other financial metrics
  # Financial Ratios
  ratio = fmpc_financial_metrics(symbols, metric = 'ratios', quarterly = TRUE)
  ratio <- ratio %>% 
    mutate_at(vars(date), as.Date) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  # Key Metrics
  keym = fmpc_financial_metrics(symbols, metric = 'key', quarterly = FALSE)
  keym <- keym %>% 
    mutate_at(vars(date), as.Date) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  # Enterprise Value
  ev = fmpc_financial_metrics(symbols, metric = 'ev', quarterly = TRUE)
  ev <- ev %>% 
    mutate_at(vars(date), as.Date)
  
  financial_data_df <- BS %>% 
    left_join(IS) %>% 
    left_join(CF) %>%
    left_join(ratio) %>% 
    left_join(keym) %>% 
    left_join(ev)
  
  financial_data_list <- list(
    financial_data_df = financial_data_df,
    price_history_data = price_history_data
  )
  
  return(financial_data_list)
}

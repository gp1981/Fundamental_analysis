

# Data processing ---------------------------------------------------------

calculate_cash_ratio <- function(financial_data) {
  cash_ratio <- financial_data %>%
    filter(statement == 'balance-sheet-statement') %>%
    select(date, cash_and_cash_equivalents, total_current_liabilities) %>%
    mutate(cash_ratio = cash_and_cash_equivalents / total_current_liabilities)
  return(cash_ratio)
}

benchmark_ratios <- function(company_data, competitors_data) {
  # Logic to benchmark ratios with competitors
  benchmarked_data <- bind_rows(company_data, competitors_data)
  return(benchmarked_data)
}

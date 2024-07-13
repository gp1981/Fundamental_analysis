

ratio_analysis_chart <- function(financial_data_df){
  
  # Data processing ---------------------------------------------------------
  
  # Function to calculate IQR limits
  calculate_iqr_limits <- function(df, financial_data_df_column) {
    Q1 <- quantile(df[[financial_data_df_column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[financial_data_df_column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_limit <- Q1 - 1.5 * IQR
    upper_limit <- Q3 + 1.5 * IQR
    return(c(lower_limit, upper_limit))
  }
  
  # List of ratio columns to process
  financial_data_df_columns <- c("currentRatio", "quickRatio", "cashRatio", 
                                 "daysOfSalesOutstanding", "daysOfInventoryOutstanding", 
                                 "daysOfPayablesOutstanding", "operatingCycle", 
                                 "cashConversionCycle", "debtEquityRatio", 
                                 "totalDebtToCapitalization", "longTermDebtToCapitalization", 
                                 "shortTermCoverageRatios", "cashFlowToDebtRatio")
  
  # Reshape the data to long format
  current_assets_ratio_data_long <- financial_data_df %>%
    select(symbol, date, currentRatio, quickRatio, cashRatio) %>%
    pivot_longer(cols = c(currentRatio, quickRatio, cashRatio),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  cash_conversion_ratio_data_long <- financial_data_df %>%
    select(symbol, date, daysOfSalesOutstanding, daysOfInventoryOutstanding, 
           daysOfPayablesOutstanding, operatingCycle, cashConversionCycle) %>%
    pivot_longer(cols = c(daysOfSalesOutstanding, daysOfInventoryOutstanding, 
                          daysOfPayablesOutstanding, operatingCycle, cashConversionCycle),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  debt_ratio_data_long <- financial_data_df %>%
    select(symbol, date, debtEquityRatio, totalDebtToCapitalization, longTermDebtToCapitalization) %>%
    pivot_longer(cols = c(debtEquityRatio, totalDebtToCapitalization, longTermDebtToCapitalization),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  debt_coverage_data_long <- financial_data_df %>%
    select(symbol, date, shortTermCoverageRatios, cashFlowToDebtRatio) %>%
    pivot_longer(cols = c(shortTermCoverageRatios, cashFlowToDebtRatio),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  
  # Calculate IQR limits for each combination of symbol and ratio_type
  current_assets_iqr_limits <- current_assets_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  cash_conversion_iqr_limits <- cash_conversion_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_ratio_iqr_limits <- debt_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_coverage_iqr_limits <- debt_coverage_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  # Convert date to Date format if it's not already
  current_assets_ratio_data_long$date <- as.Date(current_assets_ratio_data_long$date)
  cash_conversion_ratio_data_long$date <- as.Date(cash_conversion_ratio_data_long$date)
  debt_ratio_data_long$date <- as.Date(debt_ratio_data_long$date)
  debt_coverage_data_long$date <- as.Date(debt_coverage_data_long$date)
  
  # Plotting ----------------------------------------------------------------
  
  
  
  ## 01 - Current asset ratios ----------------------------------------------------
  
  # Plotting current asset ratios
  current_assets_plot <- ggplot(current_assets_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Current, Quick, and Cash Ratios by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("currentRatio" = "#0072B2", 
                                  "quickRatio" = "#009E73", 
                                  "cashRatio" = "#D55E00"),
                       labels = c("currentRatio" = "Current Ratio", 
                                  "quickRatio" = "Quick Ratio", 
                                  "cashRatio" = "Cash Ratio")) +
    scale_y_continuous(limits = c(min(current_assets_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(current_assets_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold"))
  
  ## 02 - Cash conversion ratios ----------------------------------------------------
  
  # Plotting cash conversion ratios
  cash_conversion_plot <- ggplot(cash_conversion_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Cash conversion ratio by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("daysOfSalesOutstanding" = "#0072B2",
                                  "daysOfInventoryOutstanding" = "#009E73",
                                  "daysOfPayablesOutstanding" = "#D55E00",
                                  "operatingCycle" = "#CC79A7",
                                  "cashConversionCycle" = "#E69F00"),
                       labels = c("daysOfSalesOutstanding" = "Days of Sales Outstanding (DSO)", 
                                  "daysOfInventoryOutstanding" = "Days of Inventory Outstanding (DIO)", 
                                  "daysOfPayablesOutstanding" = "Days of Payable Outstanding (DPO)",
                                  "operatingCycle" = "Operating Cycle (DSO + DIO)",
                                  "cashConversionCycle" = "Cash Conversion Cycle (DSO + DIO + DPO)")) +
    scale_y_continuous(limits = c(min(cash_conversion_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(cash_conversion_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold"))
  
  
  ## 03 - Debt ratios ----------------------------------------------------
  
  # Plotting debt ratios
  debt_ratios_plot <- ggplot(debt_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Debt ratios by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("debtEquityRatio" = "#0072B2",
                                  "totalDebtToCapitalization" = "#009E73",
                                  "longTermDebtToCapitalization" = "#D55E00"),
                       labels = c("debtEquityRatio" = "Debt to Equity (Total Liabilities / Total Equity)",
                                  "totalDebtToCapitalization" = "Total Debt to Capitalization (Total Debt / (Total Debt + Total Equity))", 
                                  "longTermDebtToCapitalization" = "Long Term Capitalization (Long Term Debt / (Long Term Debt + Total Equity))")) + 
    scale_y_continuous(limits = c(min(debt_ratio_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(debt_ratio_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold")
    )
  
  # Plotting debt coverage
  debt_coverage_plot <- ggplot(debt_coverage_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Debt Coverage Over Time",
         subtitle = "Debt coverage by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("shortTermCoverageRatios" = "#0072B2", 
                                  "cashFlowToDebtRatio" = "#E69F00"),
                       labels = c("shortTermCoverageRatios" = "Short Term Coverage Ratio (Operating Cash Flow / Short Term Debt)",
                                  "cashFlowToDebtRatio" =  "Cash Flow to Debt Ratio (Operating Cash Flow / Total Debt)")) + 
    scale_y_continuous(limits = c(min(debt_coverage_iqr_limits$ymin, na.rm = TRUE) * 0.3, 
                                  max(debt_coverage_iqr_limits$ymax, na.rm = TRUE) * 0.3)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold")
    )
  
  plot_ratio_analysis <- list(
    current_assets_plot = current_assets_plot, 
    cash_conversion_plot = cash_conversion_plot, 
    debt_ratios_plot = debt_ratios_plot, 
    debt_coverage_plot = debt_coverage_plot)
  
  return(plot_ratio_analysis)
}

total_shareholders_value <- function(financial_data_df){
  # Assuming financial_data_df is your input data frame
  # Historical Total Shareholder Equity value ------------------------------------------------------------
  
  # Check for non-zero values in dividends paid, stock issued, and repurchased
  has_dividends_paid <- any(financial_data_df$dividendsPaid != 0, na.rm = TRUE)
  has_stock_issued <- any(financial_data_df$commonStockIssued != 0, na.rm = TRUE)
  has_stock_repurchased <- any(financial_data_df$commonStockRepurchased != 0, na.rm = TRUE)
  
  # Print messages based on the presence of non-zero values
  if (has_dividends_paid) {
    print("There are dividends paid.")
  } else {
    print("No dividends paid.")
  }
  
  if (has_stock_issued) {
    print("There are common stock issued.")
  } else {
    print("No common stock issued.")
  }
  
  if (has_stock_repurchased) {
    print("There are common stock repurchased.")
  } else {
    print("No common stock repurchased.")
  }
  
  # Reverse order and compute cumulative sums
  financial_data_df1 <- financial_data_df %>%
    arrange(desc(row_number())) %>%  # Reverse order
    mutate(
      rev_dividendsPaid = cumsum(replace_na(dividendsPaid, 0)),  # Handle NA values
      rev_commonStockIssued = cumsum(replace_na(commonStockIssued, 0)),  # Handle NA values
      rev_commonStockRepurchased = cumsum(replace_na(commonStockRepurchased, 0))  # Handle NA values
    ) %>%
    arrange(row_number()) %>%  # Restore original order
    mutate(
      Historical_Equity_Value = (totalStockholdersEquity + 
                                   rev_dividendsPaid + rev_commonStockIssued - rev_commonStockRepurchased) / numberOfShares
    ) %>%
    select(-rev_dividendsPaid, -rev_commonStockIssued, -rev_commonStockRepurchased) %>%  # Remove temporary columns
    select(1:8, Historical_Equity_Value, everything())
  
  # View the resulting data frame
  print(financial_data_df1)
  
}

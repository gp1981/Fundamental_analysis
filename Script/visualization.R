
ratio_analysis_chart <- function(financial_data){

  # Extract data and symbols from financial_data
  cash_ratio_data <- financial_data$Ratio
  
  # Convert date to Date format if it's not already
  cash_ratio_data$date <- as.Date(cash_ratio_data$date)
  
  # Assuming financial_data$Ratio is your data frame containing the ratios
  
  # Reshape the data to long format
  ratio_data_long <- financial_data$Ratio %>%
    select(symbol, date, currentRatio, quickRatio, cashRatio) %>%
    pivot_longer(cols = c(currentRatio, quickRatio, cashRatio),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  # Convert date to Date format if it's not already
  ratio_data_long$date <- as.Date(ratio_data_long$date)
  
  # Plotting with ggplot2
  ggplot(ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Current, Quick, and Cash Ratios by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("currentRatio" = "#0072B2", "quickRatio" = "#009E73", "cashRatio" = "#D55E00"),
                       labels = c("currentRatio" = "Current Ratio", "quickRatio" = "Quick Ratio", "cashRatio" = "Cash Ratio")) +
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
}



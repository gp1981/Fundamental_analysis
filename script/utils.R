
export_excel_data <- function(DF1) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  
  
  # Write DF1 and DF2 to worksheet if provided
  
  writeDataTable(
    wb,
    "Data",
    x = as.data.frame(DF1),
    colNames = TRUE,
    tableStyle = "TableStyleLight9",
    tableName = "Data_US_Stocks"
  )
  
 
  # Save workbook
  saveWorkbook(wb,
               file = paste0("data/dataset", ".xlsx"),
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}


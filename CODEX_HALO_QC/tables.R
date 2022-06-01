output$full_dataset <- renderDataTable(
  uploaded_file(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(df))), 
    scrollX = TRUE)
)

output$classification_dataset <- renderDataTable(
  classification_col_filter(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(df))), 
    scrollX = TRUE)
)

output$intensity_dataset <- renderDataTable(
  intensity_col_filter(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(df))), 
    scrollX = TRUE)
)



output$full_dataset <- renderDataTable(
    uploaded_file(),
    options = list(
        columnDefs = list(list(visible = T, targets = 1:length(df))), 
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

output$double_positive_data <- renderDataTable(
    double_positives(),
    options = list(
        columnDefs = list(list(visible = T, targets = 1:length(df))), 
        scrollX = TRUE, sScrollY = '300px')
)

output$double_positive_percentages <- renderDataTable(
    (double_positives() %>% 
         mutate_if(is.numeric, ~ round(.x/total_cells()*100,3))),
    options = list(
        columnDefs = list(list(visible = T, targets = 1:length(df))), 
        scrollX = TRUE, sScrollY = '300px')
)
output$download_dp_total <- downloadHandler(
    filename = 'double_positives.csv',
    content = function(file) {
        write_csv(double_positives(), file)
    }
)

output$download_dp_perc <- downloadHandler(
    filename = 'double_positive_perc.csv',
    content = function(file) {
        write_csv((double_positives() %>% mutate_if(is.numeric, ~ round(.x/total_cells()*100,3))), file)
    }
)

output$download_classification_cols <- downloadHandler(
    filename = 'classification_cols.csv',
    content = function(file) {
        write_csv(classification_col_filter(), file)
    }
)

output$download_intensity_cols <- downloadHandler(
    filename = 'intensity_cols.csv',
    content = function(file) {
        write_csv(intensity_col_filter(), file)
    }
)
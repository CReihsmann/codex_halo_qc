uploaded_file <- reactive({
    
    req(input$file)
    
    df <- read_csv(input$file$datapath)
})

classification_col_filter <- reactive({
    uploaded_file() %>% 
        select(`Object Id`,contains('Positive Classification'))%>%
        rename_with(~ gsub(" Positive Classification", "", .x, fixed = T))
})

intensity_col_filter <- reactive({
    uploaded_file() %>%
        select(`Object Id`, contains('Intensity')) %>%
        rename_with(~ gsub(" Intensity", "", .x, fixed = T))
})

total_cells <- reactive({
    as.numeric((nrow(classification_col_filter())))
})

doughnut_totals <- reactive({
    total_cells <- total_cells()
    
    classification_col_filter() %>% 
        summarise_at(2:ncol(classification_col_filter()), sum) %>% 
        select(matches(comp_markers())) %>% 
        mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
        mutate(other_cells = total_cells - total_of_subset) %>% 
        relocate(c(total_of_subset, other_cells), .before = 1)
})

comp_markers <- eventReactive(input$comp_reset, {
    input$barChart_input
})

intensity_markers_y <- eventReactive(input$intensity_reset, {
    input$y_input
})
intensity_markers_x <- eventReactive(input$intensity_reset, {
    input$x_input
})
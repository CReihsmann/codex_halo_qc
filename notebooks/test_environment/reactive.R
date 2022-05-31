uploaded_file <- reactive({
  
  withProgress(message = 'Converting Data',style = 'notification', value = 0, {
    req(input$file)
    
    df <- read_csv(input$file$datapath)})
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

observeEvent(input$file, {
  
  marker_names <- classification_col_filter() %>%
    select(!`Object Id`) %>%
    colnames()
  
  marker_names_intensity <- intensity_col_filter() %>%
    select(!`Object Id`) %>%
    colnames()
  
  markers_bars = c()
  
  for(i in marker_names){
    print(i)
    i = str_replace(i, '([.])', "-")
    markers_bars = append(marker_names, i)
  }
  
  markers_intensity = c()
  
  for(i in marker_names_intensity){
    print(i)
    i = str_replace(i, '([.])', "-")
    markers_intensity = append(marker_names_intensity, i)
  }
  
  markers_dict <- list()
  for(i in 1:length(marker_names)) {
    markers_dict[markers_bars[i]] <- marker_names[i]
  }
  
  markers_intensity_dict <- list()
  for(i in 1:length(marker_names_intensity)) {
    markers_intensity_dict[markers_intensity[i]] <- marker_names_intensity[i]
  }
  
  updateSelectInput(session,
                    'barChart_input',
                    choices = markers_dict)
  
  updateSelectInput(session,
                    'y_input',
                    choices = markers_intensity_dict)
  
  updateSelectInput(session,
                    'x_input',
                    choices = markers_intensity_dict)
  
})

total_cells <- reactive({
  as.numeric((nrow(classification_col_filter())))
})

totals_barCharts <- reactive({
  classification_col_filter() %>% 
    summarise_at(2:ncol(classification_cols), sum) %>% 
    select(matches(input$barChart_input))
})
totals_doughnut <- reactive({
  classification_col_filter() %>% 
    summarise_at(2:ncol(classification_cols), sum) %>% 
    select(matches(input$barChart_input)) %>% 
    mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
    relocate(total_of_subset, .before = 1)
})

doughnut_totals <- reactive({
  total_cells <- as.numeric(nrow(classification_col_filter()))
  
  classification_col_filter() %>% 
    summarise_at(2:ncol(classification_col_filter()), sum) %>% 
    select(matches(input$barChart_input)) %>% 
    mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
    mutate(other_cells = total_cells - total_of_subset) %>% 
    relocate(c(total_of_subset, other_cells), .before = 1)
})
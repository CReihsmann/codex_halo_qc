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

double_positives <- reactive({
  added_constant <- classification_col_filter() %>% 
    mutate(ID = row_number()) %>%
    pivot_longer(-ID) %>%
    filter(value != 0)
  
  double_pos_occurances <- merge(added_constant, added_constant, by = 'ID', all = T) %>% 
    filter(name.x != name.y) %>% 
    group_by(name.x, name.y) %>% 
    summarise(val=n()) %>% 
    pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
    rename(markers = name.x)  
})
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

bar_chart_base <- reactive({
  classification_cols <- classification_col_filter()
  
  total_cells <- as.numeric(nrow(classification_cols))
  
  
  totals <- classification_cols %>% 
    summarise_at(2:ncol(classification_cols), sum) %>% 
    select(matches(input$barChart_input))
  
  
  added_constant <- classification_cols %>%
    mutate(ID = row_number()) %>%
    pivot_longer(-ID) %>%
    filter(value != 0)
  
  double_pos_occurances <- merge(added_constant, added_constant, by = 'ID', all = T) %>% 
    filter(name.x != name.y) %>% 
    group_by(name.x, name.y) %>% 
    summarise(val=n()) %>% 
    pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
    rename(markers = name.x)
  
  totals_2 <- totals %>% 
    pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'non_dp')
  
  filtered_markers <- double_pos_occurances %>% 
    select(matches(input$barChart_input)) %>% 
    ungroup()%>% 
    filter(markers %in% input$barChart_input) %>% 
    mutate(sums = rowSums(across(where(is.numeric))))
})
library(shiny)


options(shiny.maxRequestSize=500*1024^2) 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  uploaded_file <- reactive({
    
    withProgress(message = 'Converting Data',style = 'notification', value = 0, {
      req(input$file)
      
      df <- read.csv(input$file$datapath, check.names = T)})
  })
  
  classification_col_filter <- reactive({
    classification_cols <- uploaded_file() %>% 
      select(Object.Id,contains('Positive.Classification'))%>%
      rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T))
  })
  
  observeEvent(input$file, {
    
    marker_names <- classification_col_filter() %>%
      select(!Object.Id) %>%
      colnames()
    
    markers = c()
    
    for(i in marker_names){
      print(i)
      i = str_replace(i, '([.])', "-")
      markers = append(markers, i)
    }
    
    markers_dict <- list()
    for(i in 1:length(markers)) {
      markers_dict[markers[i]] <- marker_names[i]
    }
    updateSelectInput(session,
                      'barChart_input',
                      choices = markers_dict)
    
  })
  output$barChart <- renderPlot({
    
    classification_cols <- classification_col_filter()
    
    total_cells <- as.numeric(nrow(classification_cols))
    
    totals <- classification_cols %>% 
      summarise_at(2:ncol(classification_cols), sum)
    
    percentages <- totals %>% 
      pivot_longer(cols = 1:ncol(totals), names_to = "markers", values_to = "total_positive") %>% 
      mutate(percentage_of_total = round((total_positive/total_cells)*100, 2)) %>% 
      select(markers, percentage_of_total) %>% 
      pivot_wider(names_from = markers, values_from = percentage_of_total)
    
    
    test3 <- classification_cols %>%
      mutate(ID = row_number()) %>%
      pivot_longer(-ID) %>%
      filter(value != 0)
    
    double_pos_occurances <- merge(test3, test3, by = 'ID', all = T) %>% 
      filter(name.x != name.y) %>% 
      group_by(name.x, name.y) %>% 
      summarise(val=n()) %>% 
      pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
      rename(markers = name.x)
    
    test <- double_pos_occurances %>% 
      select(!!as.name(input$barChart_input)) %>% 
      ungroup()%>% 
      filter(markers %in% input$barChart_input) %>% 
      mutate(sums = rowSums(across(where(is.numeric))))
    
    totals_2 <- totals %>% 
      pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'non_dp')
    
    test_b <- test %>% 
      left_join(totals_2) %>% 
      mutate(non_dp = non_dp - sums) %>% 
      select(!sums) 
    
    test_b %>% 
      pivot_longer(2:ncol(test_b), names_to = 'dp_markers', values_to = 'dp_rates') %>%
      mutate(percentage_of_total = round((dp_rates/total_cells)*100, 2)) %>% 
      select(markers, dp_markers, percentage_of_total) %>%
      arrange(percentage_of_total) %>% 
      ggplot(aes(x=markers, y = percentage_of_total, fill = reorder(dp_markers, percentage_of_total)))+
      geom_col()
    
    
  })
  
})
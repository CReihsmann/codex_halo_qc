#Intensity Plots

output$intensityChart <- renderPlot({
  
  x_marker <- word(input$x_input, 1)
  y_marker <- word(input$y_input, 1)
  dataset <- uploaded_file()
  
  intensity_class_x <- dataset %>%
    select(`Object Id`, contains(x_marker))
  intensity_class_y <- dataset %>%
    select(`Object Id`, contains(y_marker))
  intensity_all <- intensity_class_x %>%
    merge(intensity_class_y)
  class_sums <- intensity_all %>%
    select(`Object Id`, contains('Positive Classification')) %>% 
    mutate(sums = rowSums(.[2:3])) %>%
    select(`Object Id`, sums)
  
  intensity_with_sums <- intensity_all %>%
    select(`Object Id`, contains('Intensity')) %>%
    merge(class_sums) %>%
    rename_with(~ gsub(" Intensity", "", .x, fixed = T)) %>%
    select(sums, matches(input$x_input), matches(input$y_input))
  
  if (input$log_trans == 'yes'){
    intensity_with_sums <- intensity_with_sums %>%
      mutate_at(vars(2:3), log)
  }
  else {
    intensity_with_sums
  }
  
  intensity_no_pos <- intensity_with_sums %>%
    filter(sums == 0)
  intensity_ind_pos <- intensity_with_sums %>%
    filter(sums == 1)
  intensity_double_pos <- intensity_with_sums %>%
    filter(sums == 2)
  
  if (input$intensity_choices == 'all'){
    intensity_no_pos %>% 
      bind_rows(intensity_ind_pos) %>% 
      bind_rows(intensity_double_pos) %>% 
      ggplot(aes(x=!!as.name(input$x_input), y=!!as.name(input$y_input)))+
      geom_hex(bins = 200, alpha = 1.0)
  }
  else if (input$intensity_choices == 'positive') {
    intensity_ind_pos %>% 
      bind_rows(intensity_double_pos) %>% 
      ggplot(aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input)))+
      geom_hex(bins = 200, alpha = 1.0)+
      geom_hex(data = intensity_no_pos, alpha = 0.4, bins = 200, aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input))) 
  }
  else if (input$intensity_choices == 'double positive') {
    intensity_no_pos %>% 
      bind_rows(intensity_ind_pos) %>% 
      ggplot(aes(x=!!as.name(input$x_input), y=!!as.name(input$y_input)))+
      geom_hex(bins = 200, alpha = 0.4) +
      geom_hex(data = intensity_double_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input)))
  }
  else if (input$intensity_choices == 'positive (no double positive)') {
    intensity_double_pos %>% 
      bind_rows(intensity_no_pos) %>% 
      ggplot(aes(x=!!as.name(input$x_input), y=!!as.name(input$y_input)))+
      geom_hex(bins = 200, alpha = 0.4)+
      geom_hex(data = intensity_ind_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input)))
  }
  else if(input$intensity_choices == 'negative') {
    intensity_double_pos %>% 
      bind_rows(intensity_ind_pos) %>% 
      ggplot(aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input)))+
      geom_hex(bins = 200, alpha = 0.4)+
      geom_hex(data = intensity_no_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(input$x_input), y = !!as.name(input$y_input)))
  }
  
})

#--Bar Charts
output$barChart <- renderPlotly({
  
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

  if (input$radio_bar == '% of whole') {
    final_df <- filtered_markers %>% 
      left_join(totals_2) %>% 
      mutate(non_dp = non_dp - sums) %>% 
      select(!sums)%>% 
      pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>%
      mutate(percentage_of_total = round((dp_rates/total_cells)*100, 2)) %>% 
      select(markers, dp_markers, percentage_of_total) %>%
      arrange(percentage_of_total) 
    
    ggplotly(final_df %>% 
               ggplot(aes(x=markers, y = percentage_of_total, fill = reorder(dp_markers, percentage_of_total)))+
               geom_col())
  }
  else {
    total_of_subset_prep <- totals %>% 
      pivot_longer(1:last_col(), names_to = 'markers', values_to = 'totals_cell')
    
    total_of_subset <- as.numeric(sum(total_of_subset_prep$totals_cell))
    
    final_df <- filtered_markers %>% 
      left_join(totals_2) %>% 
      mutate(non_dp = non_dp - sums) %>% 
      select(!sums) %>% 
      mutate_if(is.numeric, ~ round(.x/total_of_subset*100,2))%>% 
      pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>% 
      select(markers, dp_markers, dp_rates)
    
    ggplotly(final_df%>%
               ggplot(aes(x=markers, y = dp_rates, fill = reorder(dp_markers, dp_rates)))+
               geom_col())
  }
  
})

#--Doughnut Charts

output$doughnutChart <- renderPlot({
  
  totals <- doughnut_totals()
  
  if (input$radio_donut == '% of subset') {
    
    subset_totals <- totals %>% 
      pivot_longer(3:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
      mutate(percent_of_subset = round((total_marker_subset/total_of_subset),3)) %>% 
      mutate(ymax = cumsum(percent_of_subset)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(label = paste0(selected_markers, ' \n', (percent_of_subset*100), "%"),
             labelPosition = (ymax + ymin)/2) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax = 4, xmin=3, fill=selected_markers))+
      geom_rect()+
      geom_text(x=3.5, aes(y=labelPosition, label = label), size=5)+
      geom_text(x = 2, aes(y=0, label = '% of subset'), size = 6)+
      scale_fill_brewer(palette = 4)+
      scale_color_brewer(palette=3) +
      coord_polar(theta="y")+
      xlim(c(2, 4))+
      theme_void() +
      theme(legend.position = "none")
    
    subset_totals
  }
  else {
    total_cells <- as.numeric(nrow(classification_col_filter()))
    
    subset_of_all <- totals %>% 
      pivot_longer(2:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
      mutate(percent_of_all = round((total_marker_subset/total_cells),3)) %>%  
      mutate(ymax = cumsum(percent_of_all)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(label = paste0(selected_markers, ' \n', (percent_of_all*100), "%"),
             labelPosition = (ymax + ymin)/2) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax = 4, xmin=3, fill=selected_markers))+
      geom_rect()+
      geom_text(x=2.80, aes(y=labelPosition, label = label), size=4)+
      geom_text(x = 2, aes(y=0, label = '% of all'), size = 6)+
      scale_fill_brewer(palette = 4)+
      scale_color_brewer(palette=3) +
      coord_polar(theta="y")+
      xlim(c(2, 4))+
      theme_void() +
      theme(legend.position = "none")
    
    subset_of_all
  }
})
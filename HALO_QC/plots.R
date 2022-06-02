output$barChart <- renderPlot({
    
    classification_cols <- classification_col_filter()
    
    total_cells <- total_cells()
    
    
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
    
    if (input$subset_bar == '% of whole') {
        final_df <- filtered_markers %>% 
            left_join(totals_2) %>%  
            select(!sums)%>% 
            pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>%
            mutate(percentage_of_total = round((dp_rates/total_cells)*100, 2)) %>% 
            select(markers, dp_markers, percentage_of_total) %>%
            arrange(percentage_of_total) 
        
        final_df %>% 
            ggplot(aes(x=markers, y = percentage_of_total, fill = reorder(dp_markers, percentage_of_total)))+
            geom_col(position = 'dodge')
    }
    else {
        total_of_subset_prep <- totals %>% 
            pivot_longer(1:last_col(), names_to = 'markers', values_to = 'totals_cell')
        
        total_of_subset <- as.numeric(sum(total_of_subset_prep$totals_cell))
        
        final_df <- filtered_markers %>% 
            left_join(totals_2) %>%  
            select(!sums) %>% 
            mutate_if(is.numeric, ~ round(.x/total_of_subset*100,2))%>% 
            pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>% 
            select(markers, dp_markers, dp_rates)
        
        final_df%>%
            ggplot(aes(x=markers, y = dp_rates, fill = reorder(dp_markers, dp_rates)))+
            geom_col(position = 'dodge')
    }
})

output$doughnutChart <- renderPlotly({
    
    totals <- doughnut_totals()
    
    if (input$subset_pie == '% of subset') {
        
        subset_totals <- totals %>% 
            pivot_longer(3:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
            plot_ly(labels = ~selected_markers, values = ~total_marker_subset,
                    textinfo = 'label+percent',
                    insidetextorientation='radial') %>% 
            add_pie(hole = 0.5)
        
        subset_totals
    }
    else {
        total_cells <- total_cells()
        
        subset_of_all <- totals %>% 
            pivot_longer(2:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
            plot_ly(labels = ~selected_markers, values = ~total_marker_subset,
                    textinfo = 'label+percent',
                    insidetextorientation='radial') %>% 
            add_pie(hole = 0.5)
        
        subset_of_all
    }
})
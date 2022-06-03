output$barChart <- renderPlot({
    
    classification_cols <- classification_col_filter()
    
    total_cells <- total_cells()
    
    
    totals <- classification_cols %>% 
        summarise_at(2:ncol(classification_cols), sum) %>% 
        select(matches(comp_markers()))
    
    
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
        select(matches(comp_markers())) %>% 
        ungroup()%>% 
        filter(markers %in% comp_markers()) %>% 
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
            geom_col(position = 'dodge')+
            theme_minimal()
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
            geom_col(position = 'dodge') +
            theme_minimal()
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
            add_pie()
        
        subset_totals
    }
    else {
        total_cells <- total_cells()
        
        subset_of_all <- totals %>% 
            pivot_longer(2:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
            plot_ly(labels = ~selected_markers, values = ~total_marker_subset,
                    textinfo = 'label+percent',
                    insidetextorientation='radial') %>% 
            add_pie()
        
        subset_of_all
    }
})

output$intensityChart <- renderPlotly({
    
    x_marker <- word(intensity_markers_x(), 1)
    y_marker <- word(intensity_markers_y(), 1)
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
        select(sums, matches(intensity_markers_x()), matches(intensity_markers_y()))

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
            ggplot(aes(x=!!as.name(intensity_markers_x()), y=!!as.name(intensity_markers_y())))+
            geom_hex(bins = 200, alpha = 1.0) +
            theme_minimal() +
            theme(aspect.ratio = 1/1)
    }
    else if (input$intensity_choices == 'positive') {
        intensity_ind_pos %>%
            bind_rows(intensity_double_pos) %>%
            ggplot(aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            geom_hex(bins = 200, alpha = 1.0)+
            geom_hex(data = intensity_no_pos, alpha = 0.4, bins = 200, aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            theme_minimal() +
            theme(aspect.ratio = 1/1)
    }
    else if (input$intensity_choices == 'double positive') {
        intensity_no_pos %>%
            bind_rows(intensity_ind_pos) %>%
            ggplot(aes(x=!!as.name(intensity_markers_x()), y=!!as.name(intensity_markers_y())))+
            geom_hex(bins = 200, alpha = 0.4) +
            geom_hex(data = intensity_double_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            theme_minimal() +
            theme(aspect.ratio = 1/1)
    }
    else if (input$intensity_choices == 'positive (no double positive)') {
        intensity_double_pos %>%
            bind_rows(intensity_no_pos) %>%
            ggplot(aes(x=!!as.name(intensity_markers_x()), y=!!as.name(intensity_markers_y())))+
            geom_hex(bins = 200, alpha = 0.4)+
            geom_hex(data = intensity_ind_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            theme_minimal() +
            theme(aspect.ratio = 1/1)
    }
    else if(input$intensity_choices == 'negative') {
        intensity_double_pos %>%
            bind_rows(intensity_ind_pos) %>%
            ggplot(aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            geom_hex(bins = 200, alpha = 0.4)+
            geom_hex(data = intensity_no_pos, alpha = 1.0, bins = 200, aes(x=!!as.name(intensity_markers_x()), y = !!as.name(intensity_markers_y())))+
            theme_minimal() +
            theme(aspect.ratio = 1/1)
    }
})

output$cellMap <-renderPlotly({
    
    x_y_coord <- uploaded_file() %>% 
        select(`Object Id`, XMin:YMax) %>% 
        mutate(x = round((XMin + XMax)/2, 0),
               y = round((YMin + YMax)/2, 0)) %>% 
        select(`Object Id`, x, y)
    
    classification_cols <- classification_col_filter() %>% 
        right_join(x_y_coord)
    
    marker1 = input$marker_1
    marker2 = input$marker_2
    
    class_x <- classification_cols %>%
        select(`Object Id`, x, y, matches(marker1)) %>%
        filter(!!as.name(input$marker_1) == 1)
    class_y <- classification_cols %>%
        select(`Object Id`, x, y, matches(marker2)) %>%
        filter(!!as.name(input$marker_2) == 1)
    
    dp_cells <- class_x %>%
        inner_join(class_y) %>%
        mutate(marker = 'Double Positive') %>%
        select(!matches(marker1, marker2))
    
    dp_ob_ids <- dp_cells$`Object Id`
    
    class_x_only <- class_x %>%
        filter(`Object Id` != dp_ob_ids) %>%
        mutate(marker = marker1) %>%
        select(!matches(marker1))
    
    class_y_only <- class_y %>%
        filter(`Object Id` != dp_ob_ids) %>%
        mutate(marker = marker2) %>%
        select(!matches(marker2))
    
    # if (nrow(class_x_only) > 0 & nrow(class_y_only) > 0) {
    comb_markers <- class_x_only %>%
        bind_rows(class_y_only)
    # }
    # else if (nrow(class_x_only) == 0 & nrow(class_y_only) > 0) {
    #     comb_markers <- class_y_only
    # }
    # else if (nrow(class_y_only) == 0 & nrow(class_x_only) > 0) {
    #     comb_markers <- class_x_only
    # }
    # else if (nrow(class_y_only) == 0 & nrow(class_x_only) == 0) {
    #     comb_markers <- x_y_coord 
    # }
    marker_ob_ids <- comb_markers$`Object Id`
    x_y_none <- x_y_coord %>%
        filter(`Object Id` != marker_ob_ids) %>%
        mutate(marker = 'Negative')
    final_graph <-(x_y_none %>%
                       ggplot(aes(x, y, color=marker)) +
                       geom_point(size = 0.7, alpha = 0.2, shape = 16) +
                       geom_point(data = comb_markers, size = 1.1, alpha = 1, shape = 16, aes(x, y))+
                       geom_point(data = dp_cells, size = 1.1, alpha = 1, shape = 16, aes(x,y))+
                       theme_void()+
                       theme(aspect.ratio = (max(x_y_coord$x)/max(x_y_coord$y)),
                             legend.key.size = unit(2, 'cm'),
                             legend.title = element_text(size = 20),
                             legend.text = element_text(size = 16)) +
                       guides(color = guide_legend(override.aes = list(size=5))) +
                       scale_color_manual(values = c(marker1 = 'blue',
                                                     marker2 = 'red',
                                                     'Double Positive' = 'green',
                                                     'Negative' = 'grey50')))
    final_graph %>% 
        toWebGL()
    # partial_bundle(toWebGL(x_y_none %>%
    #                            ggplot(aes(x, y, color=marker)) +
    #                            geom_point(size = 0.7, alpha = 0.2, shape = 16) +
    #                            geom_point(data = comb_markers, size = 1.1, alpha = 1, shape = 16, aes(x, y))+
    #                            geom_point(data = dp_cells, size = 1.1, alpha = 1, shape = 16, aes(x,y))+
    #                            theme_void()+
    #                            theme(aspect.ratio = (max(x_y_coord$x)/max(x_y_coord$y)),
    #                                  legend.key.size = unit(2, 'cm'),
    #                                  legend.title = element_text(size = 20),
    #                                  legend.text = element_text(size = 16)) +
    #                            guides(color = guide_legend(override.aes = list(size=5))) +
    #                            scale_color_manual(values = c(marker1 = 'blue',
    #                                                          marker2 = 'red',
    #                                                          'Double Positive' = 'green',
    #                                                          'Negative' = 'grey50'))))
})
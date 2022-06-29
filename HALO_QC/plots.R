output$barChart <- renderPlotly({
    
    if (!is.null(comp_markers_2())) {
    classification_cols <- classification_col_filter() %>% 
        filter(!!as.name(comp_markers_2()) == 1)
    }
    else {
        classification_cols <- classification_col_filter()
    }
    
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
        pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'Target Cell')
    
    filtered_markers <- double_pos_occurances %>% 
        select(matches(comp_markers())) %>% 
        ungroup()%>% 
        filter(markers %in% comp_markers()) %>% 
        mutate(sums = rowSums(across(where(is.numeric))))
    colnames(filtered_markers) <- paste(colnames(filtered_markers), 'double +', sep = ' ')
    filtered_markers <- filtered_markers %>% 
        rename(markers = `markers double +`) %>% 
        rename(sums = `sums double +`)
    
    if (input$subset_bar == '% of whole') {
        final_df <- filtered_markers %>% 
            left_join(totals_2) %>%  
            select(!sums) %>% 
            pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>%
            mutate(percentage_of_total = round((dp_rates/total_cells)*100, 4)) %>%
            filter(percentage_of_total != 0) %>%
            select(markers, dp_markers, percentage_of_total) %>%
            arrange(percentage_of_total)
        
        final_df %>%
            ggplot(aes(x=markers, y = percentage_of_total, fill = dp_markers))+
            geom_col(position = position_dodge(preserve = 'single')) +
            coord_flip() +
            theme_minimal() +
            labs(title = 'Percent of Whole Tissue',
                 fill = 'Target Cell & \n Double Positives',
                 y = 'Percent (%)',
                 x = 'Markers')+
            scale_fill_manual(values = comp_colors)
        
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
            filter(dp_rates != 0) %>%
            select(markers, dp_markers, dp_rates)
        
        final_df%>%
            ggplot(aes(x=markers, y = dp_rates, fill = dp_markers))+
            geom_col(position = position_dodge(preserve = 'single')) +
            coord_flip() +
            theme_minimal() +
            labs(title = 'Percent of Subset',
                 fill = 'Target Cell & \n Double Positives',
                 y = 'Percent (%)',
                 x = 'Markers')+
            scale_fill_manual(values = comp_colors)
        
    }
})

output$doughnutChart <- renderPlotly({
    
    m1 <- list(
        b = 75,
        t = 75,
        pad = 30
    )
    
    m2 <- list(
        b = 50,
        t = 100,
        pad = 30
    )
    
    totals <- doughnut_totals()
    
    if (input$subset_pie == '% of subset') {
        subset_totals <- totals %>% 
            pivot_longer(2:(last_col()-1), names_to = 'selected_markers', values_to = 'total_marker_subset')
        
        subset_totals$selected_markers <- factor(subset_totals$selected_markers, levels = subset_totals$selected_markers)
        
        subset_totals %>%  
            plot_ly(labels = ~selected_markers, values = ~total_marker_subset,
                    marker = list(colors = comp_colors),
                    textinfo = 'label+percent',
                    insidetextorientation='radial') %>% 
            add_pie(hole = 0.5) %>% 
            layout(autosize = T,
                   margin = m1)
        
        
    }
    else {
        total_cells <- total_cells()
        
        subset_of_all <- totals %>% 
            pivot_longer(2:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset')
        
        subset_of_all$selected_markers <- factor(subset_of_all$selected_markers, levels = subset_of_all$selected_markers)
        
        subset_of_all %>% 
            plot_ly(labels = ~selected_markers, values = ~total_marker_subset,
                    marker = list(colors = comp_colors),
                    textinfo = 'label+percent',
                    insidetextorientation='radial') %>% 
            add_pie(hole = 0.5) %>% 
            layout(autosize = T,
                   margin = m2)
    }
})

output$intensityChart <- renderPlotly({
    
    
    
    x_marker <- word(intensity_markers_x(), 1)
    y_marker <- word(intensity_markers_y(), 1)
    
    validate(
        need(x_marker != c("") & y_marker != c(""), 'Choose Markers for X and Y')
    )
    # validate(
    #     need(y_marker != c(), 'Choose Markers for y')
    # )
    
    
    dataset <- data()
    
    intensity_class_x <- dataset %>%
        select(`Object Id`, contains(x_marker))
    intensity_class_y <- dataset %>%
        select(`Object Id`, contains(y_marker))
    
    intensity_all <- intensity_class_x %>%
        merge(intensity_class_y)
    
    class_sums <- intensity_all %>%
        select(`Object Id`, contains('Positive Classification')) 
    
    validate(
        need(ncol(class_sums) > 2, 'Choose Markers for X and Y')
    )
    
    class_sums <- class_sums %>% 
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

#--Cell plotting graphs

x_y_coord_prep <- function(data){ #gets x, y coordinates prepped
    data %>% 
        select(`Object Id`, XMin:YMax) %>% 
        mutate(x = round((XMin + XMax)/2, 0),
               y = round(-(YMin + YMax)/2, 0)) %>% 
        select(`Object Id`, x, y)
}

output$cellMap <-renderPlotly({
    x_y_coord <- x_y_coord_prep(data())
    
    classification_cols <- classification_col_filter() %>% 
        right_join(x_y_coord)
    
    marker1 = cell_mapping_m1()
    marker2 = cell_mapping_m2()
    
    validate(
        need(is.null(marker1)==F | !is.null(marker2)==F, 'Choose Markers'),
        need(marker1 %in% colnames(classification_cols) & marker2 %in% colnames(classification_cols), 'Choose Markers'))
    
    class_x <- classification_cols %>%
        select(`Object Id`, x, y, matches(marker1)) %>%
        filter(!!as.name(marker1) == 1)
    
    class_y <- classification_cols %>%
        select(`Object Id`, x, y, matches(marker2)) %>%
        filter(!!as.name(marker2) == 1)
    
    
    
    dp_cells <- class_x %>%
        inner_join(class_y) %>%
        mutate(marker = 'Double Positive') %>%
        select(!c(matches(marker1), matches(marker2)))
    
    dp_ob_ids <- dp_cells$`Object Id`
    
    class_x_only <- class_x %>%
        filter(!`Object Id` %in% dp_ob_ids) %>%
        mutate(marker = marker1) %>%
        select(!matches(marker1))
    
    class_y_only <- class_y %>%
        filter(!`Object Id` %in% dp_ob_ids) %>%
        mutate(marker = marker2) %>%
        select(!matches(marker2))
    
    comb_markers <- class_x_only %>%
        bind_rows(class_y_only) %>%
        bind_rows(dp_cells)
    
    marker_ob_ids <- comb_markers$`Object Id`
    
    x_y_none <- x_y_coord %>%
        filter(!`Object Id` %in% marker_ob_ids) %>%
        mutate(marker = 'Negative')
    
    axis = list(showgrid = FALSE, showticklabels = FALSE, showgrid = FALSE)
    bg_color = '#000000'
    tx_color = list(color = '#000000',
                    size = 16,
                    bgcolor = '#FFFFFF')
    lg_layout = list(font = list(color = '#000000',
                                 size = 12),
                     bgcolor = '#FFFFFF')
    
    if (marker1 == marker2){
        final_graph <-plot_ly(type = 'scatter', mode = 'markers') %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative', hoverinfo = 'name') %>%
            add_trace(x=~class_x$x, y=~class_x$y, marker = list(color = 'rgb(0, 0, 164)'), name = marker1, hoverinfo = 'name') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    else if (nrow(class_x_only) > 0 & nrow(class_y_only) > 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative', hoverinfo = 'name') %>%
            add_trace(x=~class_x_only$x, y=~class_x_only$y, marker = list(color = 'rgb(0, 0, 164)'), name = marker1, hoverinfo = 'name') %>%
            add_trace(x=~class_y_only$x, y=~class_y_only$y, marker = list(color = 'rgb(255, 0, 0)'), name = marker2, hoverinfo = 'name') %>%
            add_trace(x=~dp_cells$x, y=~dp_cells$y, marker = list(color='rgb(57, 255, 20)'), name = 'Double Positive', hoverinfo = 'name') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    else if (nrow(class_x_only) == 0 & nrow(class_y_only) > 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative', hoverinfo = 'name') %>%
            add_trace(x=~class_y_only$x, y=~class_y_only$y, marker = list(color = 'rgb(255, 0, 0)'), name = marker2, hoverinfo = 'name') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    else if (nrow(class_y_only) == 0 & nrow(class_x_only) > 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative', hoverinfo = 'name') %>%
            add_trace(x=~class_x_only$x, y=~class_x_only$y, marker = list(color = 'rgb(0, 0, 164)'), name = marker1, hoverinfo = 'name') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    else if (nrow(class_y_only) == 0 & nrow(class_x_only) == 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative', hoverinfo = 'name') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    
    final_graph %>%
        toWebGL() 
})

output$cellMap_ind <- renderPlotly({
    
    x_y_coord <- x_y_coord_prep(data())
    
    classification_cols <- classification_col_filter() %>% 
        right_join(x_y_coord)
    
    ind_marker = input$marker_ind
    
    validate(
        need(ind_marker %in% colnames(classification_cols),'No marker selected'))
    
    class_x <- classification_cols %>%
        select(`Object Id`, x, y, matches(ind_marker)) %>%
        filter(!!as.name(ind_marker) == 1)
    
    marker_ob_ids <- class_x$`Object Id`
    
    x_y_none <- x_y_coord %>%
        filter(!`Object Id` %in% marker_ob_ids) %>%
        mutate(marker = 'Negative')
    
    axis = list(showgrid = FALSE, showticklabels = FALSE, showgrid = FALSE)
    bg_color = '#000000'
    tx_color = list(color = '#000000',
                    size = 16,
                    bgcolor = '#FFFFFF')
    lg_layout = list(font = list(color = '#000000',
                                 size = 12),
                     bgcolor = '#FFFFFF')
    
    if (nrow(class_x) > 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative') %>%
            add_trace(x=~class_x$x, y=~class_x$y, marker = list(color = 'rgb(57, 255, 20)'), name = ind_marker) %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    else if (nrow(class_x) == 0) {
        final_graph <-plot_ly() %>%
            add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative') %>%
            layout(paper_bgcolor = bg_color,
                   plot_bgcolor = bg_color,
                   xaxis = axis,
                   yaxis = axis,
                   font = tx_color,
                   legend = lg_layout)
    }
    
    final_graph %>%
        toWebGL() 
})

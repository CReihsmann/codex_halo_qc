library(shiny)



options(shiny.maxRequestSize=500*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
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
        
        ggplot(intensity_no_pos, aes(x=matches(input$x_input), y = matches(input$y_input))) +
            geom_hex(bins = 200, alpha = 0.4) +
            geom_hex(data = intensity_ind_pos, alpha = 0.4, bins = 200, aes(x=matches(input$x_input), y = matches(input$y_input))) +
            geom_hex(data = intensity_double_pos, alpha = 1.0, bins = 200, aes(x=matches(input$x_input), y = matches(input$y_input)))
        
    })
    
    output$cellMap <-renderPlot({
        
        uploaded_file() %>% 
            select(XMin:YMax) %>% 
            mutate(x = round((XMin + XMax)/2, 0),
                   y = round((YMin + YMax)/2, 0)) %>% 
            select(x, y) %>% 
            ggplot(aes(x = x, y = y))+
            geom_point()
        
        
        # fig <- plot_ly(data = x_y_coord,
        #                x = ~x,
        #                y = ~y)
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
        
        # final_df <- filtered_markers %>% 
        #     left_join(totals_2) %>% 
        #     mutate(non_dp = non_dp - sums) %>% 
        #     select(!sums)
        
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
    doughnut_totals <- reactive({
        total_cells <- as.numeric(nrow(classification_col_filter()))
        
        classification_col_filter() %>% 
            summarise_at(2:ncol(classification_col_filter()), sum) %>% 
            select(matches(input$barChart_input)) %>% 
            mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
            mutate(other_cells = total_cells - total_of_subset) %>% 
            relocate(c(total_of_subset, other_cells), .before = 1)
    })
    
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
    
})
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
    
    output$cellMap <-renderPlotly({
        
        x_y_coord <- uploaded_file() %>% 
            select(XMin:YMax) %>% 
            mutate(x = round((XMin + XMax)/2, 0),
                   y = round((YMin + YMax)/2, 0)) %>% 
            select(x, y)
        
        
        fig <- plot_ly(data = x_y_coord,
                       x = ~x,
                       y = ~y)
    })
    
    output$barChart <- renderPlot({
        
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
        
        filtered_markers <- double_pos_occurances %>% 
            select(matches(input$barChart_input)) %>% 
            ungroup()%>% 
            filter(markers %in% input$barChart_input) %>% 
            mutate(sums = rowSums(across(where(is.numeric))))
        
        totals_2 <- totals %>% 
            pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'non_dp')
        
        final_df <- filtered_markers %>% 
            left_join(totals_2) %>% 
            mutate(non_dp = non_dp - sums) %>% 
            select(!sums) 
        
        final_df %>% 
            pivot_longer(2:ncol(final_df), names_to = 'dp_markers', values_to = 'dp_rates') %>%
            mutate(percentage_of_total = round((dp_rates/total_cells)*100, 2)) %>% 
            select(markers, dp_markers, percentage_of_total) %>%
            arrange(percentage_of_total) %>% 
            ggplot(aes(x=markers, y = percentage_of_total, fill = reorder(dp_markers, percentage_of_total)))+
            geom_col()
        
        
    })
    
})
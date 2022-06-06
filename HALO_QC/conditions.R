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
    
    # test_dataset <- read_csv('../data/AEIM373-object_data_test.csv')
    # 
    # intensity_cols <- test_dataset %>%  
    #     select(`Object Id`, contains('Intensity')) %>% 
    #     rename_with(~ gsub(" Intensity", "", .x, fixed = T))
    # 
    # classification_cols <- test_dataset %>% 
    #     select(`Object Id`,contains('Positive Classification'))%>%
    #     rename_with(~ gsub(" Positive Classification", "", .x, fixed = T)) %>%
    #     select(!'Object Id') %>% 
    #     colnames() %>% 
    #     as_tibble()
    # 
    # marker_names_intensity <- intensity_cols %>%
    #     select(!`Object Id`) %>%
    #     colnames()
    # 
    # markers_intensity = c()
    # 
    # for(i in marker_names_intensity){
    #     print(i)
    #     i = str_replace(i, '([.])', "-")
    #     markers_intensity = append(marker_names_intensity, i)
    # }
    # 
    # markers_intensity <- as_tibble(markers_intensity)
    # paired_intensity <- markers_intensity %>% 
    #     mutate(base_marker = word(value)) %>% 
    #     rename(select_cols = value) %>% 
    #     inner_join(classification_cols, by = c('base_marker' = 'value'))
    # final_intensity <- paired_intensity$select_cols
    
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
    
    updateSelectInput(session,
                      'marker_1',
                      choices = markers_dict)
    
    updateSelectInput(session,
                      'marker_2',
                      choices = markers_dict)
    updateSelectInput(session,
                      'marker_ind',
                      choices = markers_dict)
    
})
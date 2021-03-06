observeEvent(input$file, {
    
    uploaded_filename <-input$file$name
    
    updateSelectInput(session,
                      'file_select',
                      choices = c('example.csv', uploaded_filename))
})

observeEvent(input$update_file, {
    
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
    
    classification_cols <- classification_col_filter() %>% 
        colnames() %>%
        as_tibble()
    
    markers_intensity <- as_tibble(markers_intensity)
    paired_intensity <- markers_intensity %>%
        mutate(base_marker = word(value)) %>%
        rename(select_cols = value) %>%
        inner_join(classification_cols, by = c('base_marker' = 'value'))
    final_intensity <- paired_intensity$select_cols 
    
    markers_dict <- list()
    for(i in 1:length(marker_names)) {
        markers_dict[markers_bars[i]] <- marker_names[i]
    }
    
    
    updateSelectizeInput(session,
                         'barChart_input',
                         choices = markers_dict,
                         options = list(
                             placeholder = 'Choose markers',
                             onInitialize = I('function() { this.setValue(""); }')
                         ))
    
    updateSelectizeInput(session,
                         'y_input',
                         choices = final_intensity,
                         options = list(
                             placeholder = 'Choose a marker',
                             onInitialize = I('function() { this.setValue(""); }')
                         ))
    
    updateSelectizeInput(session,
                      'x_input',
                      choices = final_intensity,
                      options = list(
                          placeholder = 'Choose a marker',
                          onInitialize = I('function() { this.setValue(""); }')
                      ))
    
    updateSelectizeInput(session,
                      'marker_1',
                      choices = markers_dict,
                      selected = NULL,
                      options = list(
                          placeholder = 'Choose a marker',
                          onInitialize = I('function() { this.setValue(""); }')
                      ))
    
    updateSelectizeInput(session,
                      'marker_2',
                      choices = markers_dict,
                      selected = NULL,
                      options = list(
                          placeholder = 'Choose a marker',
                          onInitialize = I('function() { this.setValue(""); }')
                      ))
    
    updateSelectizeInput(session,
                      'marker_ind',
                      choices = markers_dict,
                      selected = NULL,
                      options = list(
                          placeholder = 'Choose a marker',
                          onInitialize = I('function() { this.setValue(""); }')
                      ))
    
    reset('marker_1')
    reset('marker_2')
    reset('marker_ind')
    reset('x_input')
    reset('y_input')
    reset('barChart_input')
    
})
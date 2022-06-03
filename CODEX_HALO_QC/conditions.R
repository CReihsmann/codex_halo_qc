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
  
  updateSelectInput(session,
                    'marker_1',
                    choices = markers_dict)
  
  updateSelectInput(session,
                    'marker_2',
                    choices = markers_dict)
  
})
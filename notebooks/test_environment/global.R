library(tidyverse)
library(ggplot2)
library(plotly)
#library(reticulate)
library(DT)
library(patchwork)
library(shiny)

# #--test dataset that would be loaded
# #test_dataset <- read.csv('../../data/AEIM373-object_data_test.csv', check.names = T)
# req(input$file)
# data <- data.frame(marker.Positive.Classification = 1:3, marker.Intensity = 1:3)
# test_dataset <- df
# 
# #--pulling out classification columns and intensity value columns into two tibbles
# classification_cols <- test_dataset %>%  
#     #mutate(unosid = str_sub(Algorithm.Name, 10, 19)) %>% #--pulled unosid
#     select(Object.Id,contains('Positive.Classification'))%>% 
#     rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T))
# 
# intensity_cols <- test_dataset %>%  
#     #mutate(unosid = str_sub(Algorithm.Name, 10, 19)) %>% 
#     select(Object.Id, contains('.Intensity')) %>% 
#     rename_with(~ gsub(".Intensity", "", .x, fixed = T))
# 
# #--Getting x, y coordinates for cell mapping
# x_y_coord <- test_dataset %>% 
#     select(Object.Id, XMin:YMax) %>% 
#     mutate(x = round((XMin + XMax)/2, 0),
#            y = round((YMin + YMax)/2, 0)) %>% 
#     select(Object.Id, x, y)
# 
# #--Pulls list of markers in document and re-formats for use in ui
# marker_names <- classification_cols %>% 
#     select(!Object.Id) %>% 
#     colnames() 
# 
# markers = c() 
# 
# for(i in marker_names){
#     print(i)
#     i = str_replace(i, '([.])', "-")
#     markers = append(markers, i)
# }
# 
# markers_dict <- list()
# for(i in 1:length(markers)) {
#     markers_dict[markers[i]] <- marker_names[i]
# }


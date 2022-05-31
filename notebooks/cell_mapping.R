library(tidyverse)
library(ggplot2)
library(plotly)
library(reticulate)
# use_virtualenv('../env/')
library(DT)
# library(svgPanZoom)
# library(svglite)
# library(lattice)
# library(gridSVG)
# library(data.table)

test_dataset <- read_csv('../data/AEIM373-object_data_test.csv')

x_y_coord <- test_dataset %>% 
    select(`Object Id`, XMin:YMax) %>% 
    mutate(x = round((XMin + XMax)/2, 0),
           y = round((YMin + YMax)/2, 0)) %>% 
    select(`Object Id`, x, y)

classification_cols <- test_dataset %>% 
    select(`Object Id`,contains('Positive Classification'))%>%
    rename_with(~ gsub(" Positive Classification", "", .x, fixed = T)) %>% 
    right_join(x_y_coord)

class_x <- classification_cols %>% 
    select(`Object Id`, x, y, CPEP) %>% 
    filter(CPEP == 1)
class_y <- classification_cols %>% 
    select(`Object Id`, x, y, GCG) %>% 
    filter(GCG == 1)
dp_cells <- class_x %>% 
    inner_join(class_y) %>% 
    mutate(marker = 'Double Positive') %>% 
    select(!c(CPEP, GCG))
dp_ob_ids <- dp_cells$`Object Id`
class_x_only <- class_x %>% 
    filter(`Object Id` != dp_ob_ids) %>% 
    mutate(marker = 'CPEP') %>% 
    select(!CPEP)
class_y_only <- class_y %>% 
    filter(`Object Id` != dp_ob_ids) %>% 
    mutate(marker = 'GCG') %>% 
    select(!GCG)
comb_markers <- class_x_only %>% 
    bind_rows(class_y_only) 
marker_ob_ids <- comb_markers$`Object Id`
x_y_none <- x_y_coord %>% 
    filter(`Object Id` != marker_ob_ids) %>% 
    mutate(marker = 'Negative')
test <- data.table(x_y_none)[-sample(which(marker=='Negative'), 100000)]
x_y_none %>%
    ggplot(aes(x, y, color=marker)) +
    geom_point(size = 0.7, alpha = 0.2, shape = 16) +
    geom_point(data = comb_markers, size = 1.1, alpha = 1, shape = 16, aes(x, y))+
    geom_point(data = dp_cells, size = 1.1, alpha = 1, shape = 16, aes(x,y))+
    theme_void()+
    theme(aspect.ratio = (max(x_y_coord$x)/max(x_y_coord$y)))+
    scale_color_manual(values = c('CPEP' = 'blue',
                                  'GCG' = 'red',
                                  'Double Positive' = 'green',
                                  'Negative' = 'grey50'))



# fig <- plot_ly(data = x_y_coord,
#                x = ~x,
#                y = ~y
# )
# 
# pd <- import("pandas")
# plotly <- import("plotly")
# plt <- import('matplotlib.pyplot')
# px <- import('plotly.express')
# 
# fig <- px$scatter(r_to_py(x_y_coord),
#                   x = 'x',
#                   y = 'y',
#                   height = 1000,
#                   width = 1000)
# 
# fig <-fig$update_yaxes(
#     scaleanchor = 'x',
#     scaleratio = 1
# )
# 
# fig$show()

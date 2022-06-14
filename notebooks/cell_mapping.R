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
partial_bundle(toWebGL(x_y_none %>%
                           ggplot(aes(x, y, color=marker)) +
                           geom_point(size = 0.7, alpha = 0.2, shape = 16) +
                           geom_point(data = comb_markers, size = 1.1, alpha = 1, shape = 16, aes(x, y))+
                           geom_point(data = dp_cells, size = 1.1, alpha = 1, shape = 16, aes(x,y))+
                           theme_void()+
                           theme(aspect.ratio = (max(x_y_coord$x)/max(x_y_coord$y)))+
                           scale_color_manual(values = c('CPEP' = 'blue',
                                                         'GCG' = 'red',
                                                         'Double Positive' = 'green',
                                                         'Negative' = 'grey50'))))

fig <- plot_ly(data = x_y_none, x = ~x, y = ~y, type = 'scatter') %>%
    add_trace(data = comb_markers, x = ~x, y = ~y) %>%
    add_trace(data = dp_cells, x = ~x, y = ~y) %>% 
    partial_bundle()

fig %>% toWebGL()


#---troubleshooting
test_dataset <- read_csv('../data/test.csv')

length(colnames(test_dataset))

x_y_coord <- test_dataset %>% 
    select(`Object Id`, XMin:YMax) %>% 
    mutate(x = round(((XMin + XMax)/2), 0),
           y = round(-((YMin + YMax)/2), 0)) %>% 
    select(`Object Id`, x, y)

classification_cols <- test_dataset %>% 
    select(`Object Id`,contains('Positive Classification'))%>%
    rename_with(~ gsub(" Positive Classification", "", .x, fixed = T)) %>% 
    right_join(x_y_coord)

length(colnames(classification_cols))

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
    filter(!`Object Id` %in% dp_ob_ids) %>%
    mutate(marker = 'CPEP') %>%
    select(!CPEP)

class_y_only <- class_y %>%
    filter(!`Object Id` %in% dp_ob_ids) %>%
    mutate(marker = 'GCG') %>%
    select(!GCG)

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


final_graph <-plot_ly() %>%
    add_trace(x=~x_y_none$x, y=~x_y_none$y, marker = list(color = 'rgb(169, 169, 169)'), name = 'Negative') %>%
    add_trace(x=~class_x_only$x, y=~class_x_only$y, marker = list(color = 'rgb(0, 0, 164)'), name = 'CPEP') %>%
    add_trace(x=~class_y_only$x, y=~class_y_only$y, marker = list(color = 'rgb(255, 0, 0)'), name = 'GCG') %>%
    add_trace(x=~dp_cells$x, y=~dp_cells$y, marker = list(color='rgb(57, 255, 20)'), name = 'Double Positive') %>%
    layout(paper_bgcolor = bg_color,
           plot_bgcolor = bg_color,
           xaxis = axis,
           yaxis = axis,
           font = tx_color,
           legend = lg_layout)%>%
    toWebGL()
final_graph

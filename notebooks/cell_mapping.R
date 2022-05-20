library(tidyverse)
library(ggplot2)
library(plotly)
library(reticulate)
use_virtualenv('../env/')
library(DT)

test_dataset <- read.csv('../data/AEIM373-object_data_test.csv', check.names = T)

x_y_coord <- test_dataset %>% 
  select(XMin:YMax) %>% 
  mutate(x = round((XMin + XMax)/2, 0),
         y = round((YMin + YMax)/2, 0)) %>% 
  select(x, y)


fig <- plot_ly(data = x_y_coord,
               x = ~x,
               y = ~y
)
fig
pd <- import("pandas")
plotly <- import("plotly")
plt <- import('matplotlib.pyplot')
px <- import('plotly.express')

fig <- px$scatter(r_to_py(x_y_coord),
                  x = 'x',
                  y = 'y',
                  height = 1300,
                  width = 1300)

fig <-fig$update_yaxes(
  scaleanchor = 'x',
  scaleratio = 1
)

fig$show()

library(tidyverse)
library(ggplot2)
library(plotly)

test_dataset <- read_csv('../data/AEIM373-object_data_test.csv')
c(as.list(test_dataset$`Object Id`))

test_dataset %>% 
  select(contains('ATP1a1'))

classification_cols <- test_dataset %>%  
  mutate(unosid = str_sub(Algorithm.Name, 10, 19)) %>% 
  select(unosid, contains('Positive.Classification'))%>% 
  rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T), pos_class_cols)

total_cells <- as.numeric(nrow(classification_cols))

totals <- classification_cols %>% 
  summarise_at(2:ncol(classification_cols), sum)

percentages <- totals %>% 
  pivot_longer(cols = 1:ncol(totals), names_to = "markers", values_to = "total_positive") %>% 
  mutate(percentage_of_total = round((total_positive/total_cells)*100, 2)) %>% 
  select(markers, percentage_of_total) %>% 
  pivot_wider(names_from = markers, values_from = percentage_of_total)


plot_setup <- percentages %>% 
  select(GCG, SST, CPEP, GHRL)

plot_setup %>% 
  pivot_longer(1:ncol(plot_setup), names_to = 'markers', values_to = 'percentages') %>% 
  ggplot(aes(x=markers, y=percentages)) + 
  geom_col(position = 'stack') +
  theme_minimal()+
  labs(y = 'percentages (%)')

# test <- classification_cols %>% 
#   select(!unosid) %>% 
#   mutate(ID = 1:nrow(classification_cols)) %>% 
#   pivot_longer(-ID, names_to = 'markers', values_to = "number") %>% 
#   xtabs(~ID + number, data = ., sparse = F) %>% 
#   crossprod(., .)
# 
# test_diag <- classification_cols %>% 
#   select(!unosid) %>% 
#   mutate(ID = 1:nrow(classification_cols)) %>%
#   pivot_longer(-ID, names_to = 'markers', values_to = 'number') %>% 
#   mutate(markers2 = markers) %>% 
#   xtabs(~markers + markers2, data = ., sparse = F) %>% 
#   diag()
# 
# test2 <- classification_cols %>% 
#   select(!unosid) %>% 
#   as.matrix() 
# 
# out <- crossprod(test2) 
# 
# diag(out) <- 0
# 
# 
# test3 <- classification_cols %>% 
#   select(!unosid) %>% mutate(ID = row_number()) %>% 
#   pivot_longer(-ID) %>% 
#   filter(value != 0)

double_pos_occurances <- merge(test3, test3, by = 'ID', all = T) %>% 
  filter(name.x != name.y) %>% 
  group_by(name.x, name.y) %>% 
  summarise(val=n()) %>% 
  pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
  rename(markers = name.x)

test <- double_pos_occurances %>% 
  select(GCG, SST, CPEP, GHRL) %>% 
  ungroup()%>% 
  filter(markers %in% c('GCG', 'SST', 'CPEP', 'GHRL'))
  

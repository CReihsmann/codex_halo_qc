library(tidyverse)
library(ggplot2)
library(plotly)

test_dataset <- read.csv('../data/AEIM373-object_data_test.csv', check.names = T)

classification_cols <- test_dataset %>%  
    mutate(unosid = str_sub(Algorithm.Name, 10, 19)) %>% 
    select(unosid, contains('Positive.Classification'))%>% 
    rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T))

total_cells <- as.numeric(nrow(classification_cols))

totals <- classification_cols %>% 
    summarise_at(2:ncol(classification_cols), sum) %>% 
    select(GCG, SST, CPEP, GHRL)

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

test3 <- classification_cols %>%
  select(!unosid) %>% mutate(ID = row_number()) %>%
  pivot_longer(-ID) %>%
  filter(value != 0)

double_pos_occurances <- merge(test3, test3, by = 'ID', all = T) %>% 
    filter(name.x != name.y) %>% 
    group_by(name.x, name.y) %>% 
    summarise(val=n()) %>% 
    pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
    rename(markers = name.x)

test <- double_pos_occurances %>% 
    select(GCG, SST, CPEP, GHRL) %>% 
    ungroup()%>% 
    filter(markers %in% c('GCG', 'SST', 'CPEP', 'GHRL')) %>% 
    mutate(sums = rowSums(across(where(is.numeric))))

totals_2 <- totals %>% 
    pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'non_dp')

test_b <- test %>% 
    left_join(totals_2) %>% 
    mutate(non_dp = non_dp - sums) %>% 
    select(!sums) 
    
    

test_b %>% 
    pivot_longer(2:ncol(test_b), names_to = 'dp_markers', values_to = 'dp_rates') %>%
    mutate(percentage_of_total = round((dp_rates/total_cells)*100, 2)) %>% 
    select(markers, dp_markers, percentage_of_total) %>%
    arrange(percentage_of_total) %>% 
    ggplot(aes(x=markers, y = percentage_of_total, fill = reorder(dp_markers, percentage_of_total)))+
    geom_col()



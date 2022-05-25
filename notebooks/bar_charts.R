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
# 
# percentages <- totals %>% 
#     pivot_longer(cols = 1:ncol(totals), names_to = "markers", values_to = "total_positive") %>% 
#     mutate(percentage_of_total = round((total_positive/total_cells)*100, 2)) %>% 
#     select(markers, percentage_of_total) %>% 
#     pivot_wider(names_from = markers, values_from = percentage_of_total)

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

#-----------Donut Chart

classification_cols <- test_dataset %>% 
    select(Object.Id,contains('Positive.Classification'))%>%
    rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T))

totals <- classification_cols %>% 
    summarise_at(2:ncol(classification_cols), sum) %>% 
    select(GCG, SST, CPEP) %>% 
    mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
    relocate(total_of_subset, .before = 1)

subset_totals <- totals %>% 
    pivot_longer(2:last_col(), names_to = 'selected_markers', values_to = 'total_marker_subset') %>% 
    mutate(percent_of_subset = round((total_marker_subset/total_of_subset),3)) %>% 
    mutate(ymax = cumsum(percent_of_subset)) %>% 
    mutate(ymin = c(0, head(ymax, n=-1))) %>% 
    mutate(label = paste0(selected_markers, ' \n', (percent_of_subset*100), "%"),
           labelPosition = (ymax + ymin)/2) %>% 
    ggplot(aes(ymax=ymax, ymin=ymin, xmax = 4, xmin=3, fill=selected_markers))+
    geom_rect()+
    geom_text(x=3.5, aes(y=labelPosition, label = label), size=5)+
    geom_text(x = 2, aes(y=0, label = '% of subset'), size = 6)+
    scale_fill_brewer(palette = 4)+
    scale_color_brewer(palette=3) +
    coord_polar(theta="y")+
    xlim(c(2, 4))+
    theme_void() +
    theme(legend.position = "none")

subset_totals

#------subset bar

classification_cols <- test_dataset %>%  
    select(contains('Positive.Classification'))%>% 
    rename_with(~ gsub(".Positive.Classification", "", .x, fixed = T))

total_cells <- as.numeric(nrow(classification_cols))

totals <- classification_cols %>% 
    summarise_at(1:ncol(classification_cols), sum) %>% 
    select(GCG, SST, CPEP) #%>% 
    # mutate(total_of_subset = rowSums(across(1:last_col()))) %>% 
    # relocate(total_of_subset, .before = 1)
total_of_subset_prep <- totals %>% 
    pivot_longer(1:last_col(), names_to = 'markers', values_to = 'totals_cell')

total_of_subset <- as.numeric(sum(total_of_subset_prep$totals_cell))
    
constant <- classification_cols %>%mutate(ID = row_number()) %>%
    pivot_longer(-ID) %>%
    filter(value != 0)

double_pos_occurances <- merge(constant, constant, by = 'ID', all = T) %>% 
    filter(name.x != name.y) %>% 
    group_by(name.x, name.y) %>% 
    summarise(val=n()) %>% 
    pivot_wider(names_from = name.y, values_from = val, values_fill = 0, names_sort = F) %>% 
    rename(markers = name.x)

get_sums <- double_pos_occurances %>% 
    select(GCG, SST, CPEP) %>% 
    ungroup()%>% 
    filter(markers %in% c('GCG', 'SST', 'CPEP')) %>% 
    mutate_all()

totals_2 <- totals %>% 
    pivot_longer(1:ncol(totals), names_to = 'markers', values_to = 'non_dp')

percentage <- function(x, na.rm=T){ (x/total_of_subset*100)}

final_setup_barchart_subset <- get_sums %>% 
    left_join(totals_2) %>% 
    mutate(non_dp = non_dp - sums) %>% 
    select(!sums) %>% 
    mutate_if(is.numeric, ~ round(.x/total_of_subset*100,2))%>% 
    pivot_longer(2:last_col(), names_to = 'dp_markers', values_to = 'dp_rates') %>% 
    select(markers, dp_markers, dp_rates) %>%
    ggplot(aes(x=markers, y = dp_rates, fill = reorder(dp_markers, dp_rates)))+
    geom_col()
ggplotly(final_setup_barchart_subset)

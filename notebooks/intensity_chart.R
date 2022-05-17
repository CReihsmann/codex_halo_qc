library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)

test_dataset <- read.csv('../data/AEIM373-object_data_test.csv', check.names = T)

intensity_cols <- test_dataset %>%  
  mutate(unosid = str_sub(Algorithm.Name, 10, 19)) %>% 
  select(unosid, contains('.Intensity')) %>% 
  rename_with(~ gsub(".Intensity", "", .x, fixed = T))

intensity_cols_list <- intensity_cols %>% 
  select(!unosid) %>% 
  colnames()

intensity_comp <- intensity_cols %>% 
  select(GCG.Cell, CPEP.Cell) %>% 
  mutate_all(~ log(.))

ggplotly(intensity_comp %>% 
           ggplot(aes(GCG.Cell, CPEP.Cell))+
           geom_bin2d(bins = 250) +
           xlim(-6, 5)+
           ylim(-6, 5) +
           theme(aspect.ratio = 1/1) +
           scale_fill_continuous(type = 'viridis') +
           theme_minimal()
)


intensity_class_1 <- test_dataset %>% 
  select(Object.Id, contains('CPEP'))
intensity_class_2 <- test_dataset %>% 
  select(Object.Id, contains('GCG'))
intensity_all <- intensity_class_1 %>% 
  merge(intensity_class_2)
class_sums <- intensity_all %>% 
  select(Object.Id, contains('Positive.Classification')) %>% 
  mutate(sums = rowSums(.[2:3])) %>% 
  select(Object.Id, sums)

intensity_with_sums <- intensity_all %>% 
  select(Object.Id, contains('.Intensity')) %>% 
  merge(class_sums) %>% 
  rename_with(~ gsub(".Intensity", "", .x, fixed = T)) %>% 
  select(sums, GCG.Cell, CPEP.Cell) %>% 
  mutate_at(vars(2:3), log)

intensity_no_pos <- intensity_with_sums %>% 
  filter(sums == 0) 
intensity_ind_pos <- intensity_with_sums %>% 
  filter(sums == 1)
intensity_double_pos <- intensity_with_sums %>% 
  filter(sums == 2)


ggplot(intensity_no_pos, aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_bin2d(bins = 200, alpha = 0.4) +
  geom_bin2d(data = intensity_ind_pos, alpha = 0.4, bins = 200, aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_bin2d(data = intensity_double_pos, alpha = 1.0, bins = 200, aes(x=GCG.Cell, y = CPEP.Cell))

negative <- intensity_double_pos %>% 
  bind_rows(intensity_no_pos)

ggplot(negative, aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_bin2d(bins = 200, alpha = 0.4) +
  geom_bin2d(data = intensity_ind_pos, alpha = 1, bins = 200, aes(x=GCG.Cell, y = CPEP.Cell))+
  theme(aspect.ratio = 1/1) +
  scale_fill_continuous(type = 'viridis') +
  theme_minimal()+
  xlim(-6, 5)+
  ylim(-6, 5)

ggplot(negative, aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_hex(bins = 200, alpha = 0.2) +
  geom_hex(data = intensity_ind_pos, alpha = 1, bins = 200, aes(x=GCG.Cell, y = CPEP.Cell))+
  theme(aspect.ratio = 1/1) +
  scale_fill_continuous(type = 'viridis',
                        limits = c(1, 800)) +
  theme_minimal()+
  xlim(-6, 5)+
  ylim(-6, 5)

no_pos <- intensity_no_pos %>% 
  ggplot(aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_hex(bins = 200)+
  theme_minimal()+
  theme(axis.title.x = element_blank())+
  xlim(-6, 5)+
  ylim(-6, 5)+
  scale_fill_continuous(type = 'viridis',
                        limits = c(1, 800),
                        guide = 'none')
ind_pos <- intensity_ind_pos %>% 
  ggplot(aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_hex(bins = 200)+
  theme_minimal()+
  theme(axis.title.y = element_blank())+
  xlim(-6, 5)+
  ylim(-6, 5)+
  scale_fill_continuous(type = 'viridis',
                        limits = c(1, 800),
                        guide = 'none')
d_pos <- intensity_double_pos %>% 
  ggplot(aes(x=GCG.Cell, y = CPEP.Cell)) +
  geom_hex(bins = 200)+
  theme_minimal()+
  theme(axis.title = element_blank())+
  xlim(-6, 5)+
  ylim(-6, 5)+
  scale_fill_continuous(type = 'viridis',
                        limits = c(1, 800),
                        guide = guide_colorbar(barwidth = 0.5))
no_pos + ind_pos + d_pos + theme(aspect.ratio = 1/1)
  
d_pos

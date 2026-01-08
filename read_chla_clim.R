library(tidyverse)
library(janitor)

# Script reads in masked chl-a estimates provided by Tanaka (Dec 2025)
# ESA = 4 km	Daily, 1997 - 2023	ESA Ocean Colour Climate Change Initiative (OC-CCI)
# VIIRS = 4 km	Daily, 2012 - 2025	Visible Infrared Imaging Radiometer Suite (VIIRS)

# these contain various temporal windows of chl-a at each island
vir<-read.csv('data/noaa-virrs/eds_chla.csv') %>% 
  mutate(date = as.Date(date, '%d/%m/%Y'))

# mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_NPP_VIIRS_monthly_01wk

# these are island-means to use for long-term climatology
vir2<-read.csv('data/noaa-virrs/chla_stats_2023.csv') %>% 
  clean_names() %>% 
  mutate(island = str_replace_all(island, '_', '\\ '),
         island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

vir2_C<-vir2 %>% group_by(sensor, island_group) %>% 
  summarise(mean_chlorophyll = mean(mean_chlorophyll))



## summary stats and explore 
# vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
#   pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% ggplot() +
#   geom_point(aes(VIIRS, ESA))
# 
# vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
#   pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% 
#   mutate(diff = VIIRS - ESA) %>% ggplot() +
#   geom_col(aes(island, diff)) + coord_flip()
# 
# # ESA is systematically larger than VIIRS, by ~ 0.0130
# # ESA and VIIRS are highly correlated (r = 0.993)
# 
# vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
#   pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% summarise(cor(VIIRS, ESA))
# 
# 
# 

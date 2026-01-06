library(tidyverse)
library(janitor)

vir<-read.csv('data/noaa-virrs/eds_chla.csv') %>% 
  mutate(date = as.Date(date, '%d/%m/%Y'))

tail(vir)

# mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_NPP_VIIRS_monthly_01wk

vir2<-read.csv('data/noaa-virrs/chla_stats_2023.csv') %>% clean_names()

vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
  pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% ggplot() +
  geom_point(aes(VIIRS, ESA))


vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
  pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% 
  mutate(diff = VIIRS - ESA) %>% ggplot() +
  geom_col(aes(island, diff)) + coord_flip()

# ESA is systematically larger than VIIRS, by ~ 0.0130
# ESA and VIIRS are highly correlated (r = 0.993)

vir2 %>% select(island, mean_chlorophyll, sensor) %>% 
  pivot_wider(names_from = sensor, values_from = mean_chlorophyll) %>% summarise(cor(VIIRS, ESA))

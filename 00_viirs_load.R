library(tidyverse)

vir<-read.csv('data/noaa-virrs/eds_chla.csv') %>% 
  mutate(date = as.Date(date, '%d/%m/%Y'))

tail(vir)

# mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy
# mean_annual_range_Chlorophyll_A_NPP_VIIRS_monthly_01wk

vir2<-read.csv('data/noaa-virrs/chla_stats_2023.csv')
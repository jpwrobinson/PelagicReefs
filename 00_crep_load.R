library(tidyverse)

# laura richardson 2023 NEE
depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv')
depth %>% distinct(LONGITUDE, LATITUDE, ISLAND, SITE, SITEVISITID, OBS_YEAR) %>% 
  write.csv('data/richardson_2023/crep_lat_lon_site.csv', row.names=FALSE)

# Heenan et al. 2017
crep<-read.csv('data/noaa-crep/NOAA_PACIFIC_RAMP_FISH_SPC_2010_2017_SCI_DATA_.csv') %>%
  distinct(OBS_YEAR)

depth %>% distinct(OBS_YEAR)

depth %>% distinct(LONGITUDE, LATITUDE, ISLAND, SITE, SITEVISITID, OBS_YEAR) %>% 
  group_by(ISLAND) %>% 
  summarise(LONGITUDE = mean(LONGITUDE), LATITUDE = mean(LATITUDE)) %>% 
  write.csv('data/richardson_2023/crep_lat_lon_island.csv', row.names=FALSE)

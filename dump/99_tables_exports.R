

ime_island<-read.csv(file = 'island_ime_dat.csv')

dat<-left_join(
  island %>% ungroup() %>%
    select(island_code, island, island_group, latitude, longitude),
  ime_island %>% select(island, island_messie),
  by = 'island') %>% filter(!is.na(island_group)) %>% 
  write.csv(file = 'noaa_island_lat_lon.csv', row.names=FALSE)

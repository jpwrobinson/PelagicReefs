library(tidyverse)

# laura richardson 2023 NEE
depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv')
depth %>% distinct(LONGITUDE, LATITUDE, ISLAND, SITE, SITEVISITID, OBS_YEAR) %>% 
  write.csv('data/richardson_2023/crep_lat_lon_site.csv', row.names=FALSE)

# Heenan et al. 2017
crep<-read.csv('data/noaa-crep/NOAA_PACIFIC_RAMP_FISH_SPC_2010_2017_SCI_DATA_.csv') 

depth %>% distinct(OBS_YEAR)

depth %>% distinct(LONGITUDE, LATITUDE, ISLAND, SITE, SITEVISITID, OBS_YEAR) %>% 
  group_by(ISLAND) %>% 
  summarise(LONGITUDE = mean(LONGITUDE), LATITUDE = mean(LATITUDE)) %>% 
  write.csv('data/richardson_2023/crep_lat_lon_island.csv', row.names=FALSE)


# from Tye NOAA, full dataset with 0s
lw<-read.csv('data/noaa-crep/NCRMP Fish L-W.csv') %>% 
  mutate(across(c(LW_A, LW_B, LENGTH_CONVERSION_FACTOR), as.numeric))

crep_full<-readRDS("data/noaa-crep/NCRMP.nSPC.site_meancount_species-size.rds") %>% 
  filter(MEAN_COUNT > 0) %>% 
  # join length weight parameters from Heenan 2017 to estimate biomass
  left_join(lw) %>% 
  mutate(SIZE_cmTL = as.numeric(SIZE_cmTL), 
         body_mass_g = LW_A * (SIZE_cmTL * LENGTH_CONVERSION_FACTOR) ^ LW_B,
         biomass_g = body_mass_g * MEAN_COUNT,
         biomass_g_m2 = biomass_g / (pi*(7.5^2))) %>% 
  # join bathymetry for islands ~ years available (2010-2015)
  left_join(depth %>% distinct(SITEVISITID, DEPTH, ComplexityValue, SITE_SLOPE_400m))

dim(crep_full) # 566,439 obs

# drop large mobile predators
drops<-c('Caranx melampygus', 'Un-id fish sp', 'Triaenodon obesus', 'Carcharhinus melanopterus', 'Carcharhinus amblyrhynchos', 'Nebrius ferrugineus',
         'Sarda orientalis', 'Caranx ignobilis', 'Carcharhinus galapagensis','Caranx sp', 'Thunnus albacares','Carangidae', 'Scombridae', 'Katsuwonus pelamis', 'Alectis ciliaris')
crep_full<-crep_full %>% filter(!TAXONNAME %in% drops)
dim(crep_full) # 561,265 obs

write.csv(crep_full, 'data/noaa-crep/crep_full_merged.csv', row.names=FALSE)
write.csv(crep_full %>% filter(!is.na(SITE_SLOPE_400m)), 'data/noaa-crep/crep_bathymetry_merged.csv', row.names=FALSE)


summer<-crep_full %>% filter(!is.na(SITE_SLOPE_400m) & !is.na(biomass_g)) %>% 
  group_by(SITEVISITID) %>% 
  summarise(biom_g = sum(biomass_g_m2)) %>% 
  mutate(biom_kg  = biom_g / 1000, biom_kg_ha = biom_kg * 10000)

ggplot(summer) + geom_histogram(aes(biom_kg_ha))
max(summer$biom_kg_ha) # 19,780.77 kg/ha [Palmyra - Chanos chanos]

# big biomass in Jarvis due to sharks - exclude these from our analyses
ss<-summer %>% filter(biom_kg_ha > 10000) %>% pull(SITEVISITID)
crep_full %>% filter(SITEVISITID %in% ss)

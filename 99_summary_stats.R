set.seed(43)
library(tidyverse)

## IME database
source('0_loads/00_ime_dataframe.R')
dat<-ime_island %>% left_join(
  island_complex %>% ungroup() %>% 
    mutate(island = str_replace_all(island_group, '_C', '')) %>%
    select(island, island_group, region, region.col, sst_mean:ted_sum),
  by = 'island') %>% filter(!is.na(lat))

dat %>% reframe(range(mean_chlorophyll))
dat %>% reframe(range(mean_chl_percent))



## IME model results

load(file = 'results/mod_ime.rds')

range(dat_month$mean_chlorophyll, na.rm=TRUE)
range(dat_month$Chl_increase_nearby, na.rm=TRUE)
dat_month %>% group_by(REGION) %>% summarise(min(mld), max(mld))
dat_month %>% group_by(island) %>% summarise(min(mld), max(mld)) %>% filter(island %in% c("Jarvis", 'Palmyra', 'Kingman'))

m_chl_inc %>% gather_draws(`b_.*`, regex=TRUE) %>%  
  group_by(.variable) %>% summarise(medi = median(.value))


## CREP surveys
crep_bathy_fill<-read.csv('data/noaa-crep/crep_full_merged_bathymetry_fill.csv')
with(crep_bathy_fill, range(OBS_YEAR)) # 2009-2024
with(crep_bathy_fill, length(unique(ISLAND))) # 39
with(crep_bathy_fill, length(unique(SITEVISITID))) # 6,769
with(crep_bathy_fill, length(unique(TAXONNAME))) # 693
with(crep_bathy_fill, range(DEPTH, na.rm=TRUE)) # 1.3 - 30

with(crep_bathy_fill %>% 
       filter(!is.na(SITE_SLOPE_400m)), 
     length(unique(SITEVISITID))) # 4,343

6769 - 4343 # excluded from bathy filling


## CREP fish models
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

plank_scaled %>% dim ## n = 3,631

checker<-m2_plank
checker<-m2_herb
bayes_R2(checker) 
checker %>% gather_draws(`b_.*`, regex=TRUE) %>%  
  group_by(.variable) %>% summarise(medi = median(.value))

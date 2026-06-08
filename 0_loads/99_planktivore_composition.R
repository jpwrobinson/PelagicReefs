library(tidyverse)
library(rfishbase)
source('0_loads/00_crep_metabolic.R')
mod_sites<-depth %>% distinct(SITEVISITID)

crep<-read.csv( 'data/noaa-crep/crep_for_analysis.csv')
guild<-read.csv(file = 'data/trophic_guild_dataset_202605.csv') %>% 
  mutate(species2 = str_replace_all(str_to_title(species), '_', '\\ '),
         guild2 = ifelse(trophic_guild %in% c('omnivore-pelagic', 'invertivore-pelagic'), 
                                      'facultative planktivore', trophic_guild))

crep$trophic_guild<-guild$guild2[match(crep$TAXONNAME, guild$species2)]




# missing species
missin<-crep %>% filter(is.na(trophic_guild)) %>% distinct(TAXONNAME)
crep %>% filter(TAXONNAME %in% missin$TAXONNAME) %>% dim

57985 / 564878 * 100 # 10% of obs

guild %>% filter(str_detect(species2, 'Chromis*'))

fb_names<-species(species_list = missin %>% filter(!str_detect(TAXONNAME, 'sp')) %>% pull(TAXONNAME))

guild %>% filter(species2 %in% fb_names$Species)


library(tidyverse)
library(rfishbase)
source('0_loads/00_crep_metabolic.R')
mod_sites<-depth %>% distinct(SITEVISITID) %>% pull(SITEVISITID)

crep<-read.csv( 'data/noaa-crep/crep_for_analysis.csv') %>% 
  filter(SITEVISITID %in% mod_sites)

guild<-read.csv(file = 'data/trophic_guild_dataset_202605.csv') %>% 
  mutate(species2 = str_replace_all(str_to_title(species), '_', '\\ '),
         guild2 = ifelse(trophic_guild %in% c('omnivore_pelagic','omnivores-pelagic', 'invertivores-pelagic'), 
                                      'facultative planktivore', trophic_guild))

crep$trophic_guild<-guild$guild2[match(crep$TAXONNAME, guild$species2)]

levs<-crep %>% group_by(REGION, ISLAND) %>% summarise(lat = mean(lat)) %>% 
  group_by(REGION) %>% arrange(ISLAND, lat, .by_group=TRUE) %>% pull(ISLAND)

crep_guild<-crep %>% 
  group_by(ISLAND, SITEVISITID, REGION) %>% 
  mutate(tbiom = sum(biomass_g_m2)) %>% 
  group_by(ISLAND, SITEVISITID, REGION, trophic_guild, tbiom) %>% 
  summarise(biom = sum(biomass_g_m2)) %>% 
  ungroup() %>% 
  mutate(rel_biom = biom/tbiom) %>% 
  group_by(ISLAND, REGION, trophic_guild) %>% 
  summarise(rel_biom = median(rel_biom)) %>% 
  mutate(ISLAND = factor(ISLAND, levels = rev(levs)))

ggplot(crep_guild, aes(ISLAND, rel_biom, fill=trophic_guild)) + geom_col()

pdf(file = 'fig/ime_crep/crep_planktivore_composition.pdf', height=6, width=10)
ggplot(crep_guild %>% 
         filter(trophic_guild %in% c('planktivores', 'facultative planktivore')), 
       aes(fct_reorder(ISLAND,REGION), rel_biom, col=trophic_guild)) + 
  geom_path(aes(group=ISLAND), col='grey') + geom_point() + 
       coord_flip() + labs(x = '',y = 'Relative biomass', colour='') +
  scale_y_continuous(labels=label_percent()) 
dev.off()

# missing species
missin<-crep %>% filter(is.na(trophic_guild)) %>% distinct(TAXONNAME)
crep %>% filter(TAXONNAME %in% missin$TAXONNAME) %>% dim

57985 / 564878 * 100 # 10% of obs

guild %>% filter(str_detect(species2, 'Chromis*'))

fb_names<-species(species_list = missin %>% filter(!str_detect(TAXONNAME, 'sp')) %>% pull(TAXONNAME))

guild %>% filter(species2 %in% fb_names$Species)


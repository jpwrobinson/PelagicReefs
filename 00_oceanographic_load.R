library(tidyverse)
library(janitor)
theme_set(theme_bw())

# Climatological covariates from Gove et al. 2013
island<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=2) %>% 
  clean_names() %>% 
  mutate(island = recode(island, 'French Frigate Shoals' = 'French Frigate'))

# We are investigating site-level predictors of planktivore abundance, biomass, composition and carbon flux.

# BASIC COVARIATES [Williams et al. 2015]
# depth = energy availability increases with depth [0-30m]
# climatalogical chl-a = average energy availability to reefs [surface]
# human pop. density = fishing decreases planktivore abundance
# island type = atoll/island important predictor
# SST = planktivores increase with temperature

# IMPROVED COVARIATES [Richardson et al. 2023 NEE]
# bathymetry = potential for internal waves to reach reef zones

# NEW COVARIATES
# mixed layer depth = shallower MLD helps upwelling to reach reefs, increases planktivores
mld<-read.csv('data/crep_oceanographic/island_mld_timeseries.csv') %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), year = year(Date), .before=Island)

mld_avg<-mld %>% group_by(Island) %>% summarise(mld = mean(MLD))

# MLD mean over past 3 months (inclusive of that month)
mld_recent<-mld %>% 
  mutate(mean_mld_3months = zoo::rollmean(MLD, k = 3, align = "right", fill = NA))

# tidal conversion = stronger means more internal wave action + mixing, increases planktivores
tc<-read.csv('data/crep_oceanographic/TEDestimates_CREPislands.csv') %>% 
  group_by(ISLAND) %>% summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM))

island<-island %>% 
  left_join(mld_avg %>% rename(island = Island)) %>% 
  left_join(tc %>% mutate(island_code = ISLAND) %>% select(-ISLAND))


# island oceanographic summaries
levs<-island %>% distinct(region, island_code, latitude) %>% 
  group_by(region) %>% arrange(island_code, latitude, .by_group=TRUE) %>% pull(island_code)

pdf(file = 'fig/crep_island_oceanography.pdf', height=7, width=15)
island %>% 
  mutate(island_code = factor(island_code, levels = rev(levs))) %>% 
  rename(
         mean_tidal_energy_W_m1 = ted_mean,
         sum_tidal_energy_W_m1 = ted_sum,
         mixed_layer_depth_m = mld) %>% 
  pivot_longer(-c(island_code, island, region, latitude, longitude), names_to = 'cov', values_to = 'val') %>% 
  ggplot(aes(island_code, val, fill=region)) + geom_col() +
  facet_grid(~cov, scales='free') + 
  coord_flip() +
  theme(legend.position = 'none') +
  labs( x= '', y = '')
dev.off()
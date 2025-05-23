library(tidyverse)
library(janitor)
theme_set(theme_bw())

# Climatological covariates from Gove et al. 2013
island<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=2) %>% 
  clean_names()

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


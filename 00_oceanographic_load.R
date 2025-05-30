library(tidyverse)
library(janitor)
source('00_plot_theme.R')

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
mld<-read.csv('data/crep_oceanographic/MLD_All_Islands-lrg_island_means.csv') %>% 
  mutate(Date = as.Date(Date), year = year(Date), .before=Island, X=NULL) 

pdf(file = 'fig/crep_island_MLD.pdf', height=7, width=15)
ggplot(mld, aes(Date, MLD)) + geom_line() + facet_wrap(~Island)
dev.off()

# Island values - mean
mld_avg<-mld %>% group_by(Island) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD))

# MLD mean over past 3 months (inclusive of that month)
mld_recent<-mld %>% 
  mutate(mean_mld_3months = zoo::rollmean(MLD, k = 3, align = "right", fill = NA))

# tidal conversion = stronger means more internal wave action + mixing, increases planktivores
tc<-read.csv('data/crep_oceanographic/TEDestimates_CREPislands.csv') %>% 
  group_by(ISLAND) %>% summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM))

island<-island %>% 
  left_join(mld_avg %>% rename(island = Island)) %>% 
  left_join(tc %>% mutate(island_code = ISLAND) %>% select(-ISLAND)) %>% 
  left_join(island_cols)


# island oceanographic summaries
levs<-island %>% distinct(region, island_code, latitude) %>% 
  group_by(region) %>% arrange(island_code, latitude, .by_group=TRUE) %>% pull(island_code)

pdf(file = 'fig/crep_island_oceanography.pdf', height=7, width=15)
# Island mean values
island %>% 
  mutate(island_code = factor(island_code, levels = rev(levs))) %>% 
  rename(
         tidal_energy_mean_W_m1 = ted_mean,
         tidal_energy_sum_W_m1 = ted_sum,
         mixed_layer_depth_avg_m = mld,
         mixed_layer_depth_sd_m = mld_sd) %>% 
  pivot_longer(-c(island_code, island, region,REGION, latitude, longitude, region.col), names_to = 'cov', values_to = 'val') %>% 
  ggplot(aes(island_code, val, fill=region.col), col='black') + geom_col() +
  facet_grid(~cov, scales='free') + 
  coord_flip() +
  scale_fill_identity() +
  theme(legend.position = 'none') +
  scale_y_continuous(expand=c(0,0)) +
  labs( x= '', y = '')
dev.off()

source('pairs2.R')
pairs2(island %>% select(ted_mean, ted_sum, mld, mld_sd, sst_mean, wave_energy_mean_kw_m1,chl_a_mg_m3_mean,
                         irradiance_einsteins_m2_d1_mean, latitude, longitude) %>% na.omit())

library(tidyverse)
library(janitor)
source('00_plot_theme.R')

# Climatological covariates from Gove et al. 2013
island<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=2) %>% 
  clean_names() %>% 
  mutate(island = recode(island, 'French Frigate Shoals' = 'French Frigate'))

island2<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=3) %>% 
  clean_names() %>% 
  mutate(island = recode(location_name, 'French Frigate Shoals' = 'French Frigate',
                         'Pearl & Hermes Reef' = 'Pearl & Hermes',
                         'Maui, Lanai, Molokai, Lanai, Kahoolawe' = 'Maui',
                         'Saipan, Tinian, Aguijan' = 'Saipan',
                         'Ofu, Olosega, Tau' = 'Tau'))

island<-left_join(island, island2 %>% select(-location_name, -location_code, -region, -lat, -lon))

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
  mutate(Date = as.Date(Date), year = year(Date), month = month(Date), 
         time = as.numeric(Date), Island = factor(Island), 
         below_30m = ifelse(MLD > 30, 'deep', 'shallow'),
         .before=Island, X=NULL) 

pdf(file = 'fig/crep_island_MLD.pdf', height=7, width=15)
print(
  ggplot(mld, aes(Date, MLD)) + geom_line() + facet_wrap(~Island)
)
dev.off()

# Island values - mean
mld_avg<-mld %>% group_by(Island, year) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD),
            mld_months_deep = n_distinct(Date[MLD > 30])) %>% 
  group_by(Island) %>% 
  summarise(mld = mean(mld),
            mld_sd = mean(mld_sd),
            mld_months_deep = mean(mld_months_deep)) 
  
mld_month<-mld %>% group_by(Island, month) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD)) 

# MLD mean over past 3 months (inclusive of that month)
mld_recent<-mld %>% 
  mutate(mean_mld_3months = zoo::rollmean(MLD, k = 3, align = "right", fill = NA))

# tidal conversion = stronger means more internal wave action + mixing, increases planktivores
tc_all<-read.csv('data/crep_oceanographic/TEDestimates_CREPislands.csv') %>% 
  left_join(island %>% mutate(ISLAND = island_code) %>% select(ISLAND, region))

tc<-tc_all %>% 
  group_by(ISLAND, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM), ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))

pdf(file = 'fig/crep_island_TC.pdf', height=7, width=15)
ggplot(tc_all, aes(fct_reorder(ISLAND, TED_SUM), TED_SUM, col=region)) + #geom_boxplot() +
  ggdist::stat_halfeye() +
  labs(x = '', y = 'Tidal energy (sum)', col='') + 
  theme(legend.position.inside = c(0.5, 0.7))

ggplot(tc_all, aes(TED_SUM, fill=region)) + #geom_boxplot() +
  geom_density() + facet_grid(ISLAND ~ region, scales='free') +
  labs(x = 'Tidal energy (sum)', col='') + 
  theme(legend.position = 'none')

ggplot(tc_all, aes(TED_SUM, TED_MEAN, col=region)) + #geom_boxplot() +
  geom_point() + 
  facet_grid( ~ region, scales='free') +
  labs(x = 'Tidal energy (sum)', y='Tidal energy (mean)') + 
  theme(legend.position = 'none')
dev.off()

island<-island %>% 
  left_join(mld_avg %>% rename(island = Island)) %>% 
  left_join(tc %>% mutate(island_code = ISLAND) %>% ungroup() %>% select(-ISLAND, -ted_sd)) %>% 
  left_join(island_cols)


# island oceanographic summaries
levs<-island %>% distinct(region, island_code, latitude) %>% 
  group_by(region) %>% arrange(island_code, latitude, .by_group=TRUE) %>% pull(island_code)

pdf(file = 'fig/crep_island_oceanography.pdf', height=7, width=15)
print(
  # Island mean values
island %>% 
  mutate(island_code = factor(island_code, levels = rev(levs))) %>% 
  rename(
         tidal_energy_mean_W_m1 = ted_mean,
         tidal_energy_sum_W_m1 = ted_sum,
         mixed_layer_depth_avg_m = mld,
         mixed_layer_deep_months = mld_months_deep,
         mixed_layer_depth_sd_m = mld_sd) %>% 
  pivot_longer(-c(island_code, island, region,REGION, population_status, geomorphic_type, latitude, longitude, region.col), names_to = 'cov', values_to = 'val') %>% 
  ggplot(aes(island_code, val, fill=region.col), col='black') + geom_col() +
  facet_grid(~cov, scales='free') + 
  coord_flip() +
  scale_fill_identity() +
  theme(legend.position.inside = NULL) +
  scale_y_continuous(expand=c(0,0)) +
  labs( x= '', y = '')
)
dev.off()


pdf(file = 'fig/crep_island_correlations.pdf', height=7, width=15)
print(
  pairs2(island %>% select(ted_mean, ted_sum, mld, mld_sd,mld_months_deep,
                         sst_mean, wave_energy_mean_kw_m1,chl_a_mg_m3_mean,
                         irradiance_einsteins_m2_d1_mean, reef_area, latitude, longitude) %>%
         na.omit()))
dev.off()

library(tidyverse)
library(janitor)
source('00_plot_theme.R')

# Climatological covariates from Gove et al. 2013
island<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=2) %>% 
  clean_names() %>% 
  mutate(island = recode(island, 'French Frigate Shoals' = 'French Frigate',
                         'Pearl & Hermes Reef' = 'Pearl & Hermes'))

island2<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=3) %>% 
  clean_names() %>% 
  mutate(island = island_name, 
         geomorphic_type = ifelse(str_detect(island_type, 'island',), 'Island', 'Atoll'),
         island = recode(island, 'French Frigate Shoals' = 'French Frigate',
                         'Pearl & Hermes Reef' = 'Pearl & Hermes'))

# This is island level covariates 
island<- 
  left_join(island, island2 %>% select(-island_name, -island_type, -latitude, -longitude)) %>%
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))


# Read in island complex level covariates
island_C<-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=4) %>% 
  clean_names() %>% select(-region) %>% 
  mutate(island_group = recode(location_name, 'French Frigate Shoals' = 'French Frigate',
                         'Pearl & Hermes Reef' = 'Pearl & Hermes',
                         'Maui, Lanai, Molokai, Lanai, Kahoolawe' = 'Maui_C',
                         'Saipan, Tinian, Aguijan' = 'Saipan_C',
                         'Ofu, Olosega, Tau' = 'Tau_C'))

island_complex<-left_join(
  island %>% group_by(island_group, region) %>% 
  summarise(across(c(sst_mean:irradiance_einsteins_m2_d1_mean), ~ mean(.x))),
  island_C %>% select(-location_name, -location_code))


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

# 1. island precipitation
precip<-read.csv(file = 'data/gee-exports/crep_monthly_precipitation_mm.csv') %>% 
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

precip_ann<-precip %>% group_by(island) %>% summarise(avg_monthly_mm = mean(avg_monthly_mm))
precip_ann_C<-precip %>% group_by(island_group) %>% summarise(avg_monthly_mm = mean(avg_monthly_mm))


# 2. mixed layer depth = shallower MLD helps upwelling to reach reefs, increases planktivores
mld<-read.csv('data/crep_oceanographic/MLD_All_Islands-lrg_island_means.csv') %>% 
  mutate(Date = as.Date(Date), year = year(Date), month = month(Date), 
         time = as.numeric(Date), below_30m = ifelse(MLD > 30, 'deep', 'shallow'),
         .before=Island, X=NULL) %>% 
  mutate(island_group = ifelse(Island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', Island),
         island_group = ifelse(Island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(Island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

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

mld_avg_C<- mld %>% group_by(island_group, year) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD),
            mld_months_deep = n_distinct(Date[MLD > 30])) %>% 
  group_by(island_group) %>% 
  summarise(mld = mean(mld),
            mld_sd = mean(mld_sd),
            mld_months_deep = mean(mld_months_deep)) 
  
mld_month<-mld %>% group_by(Island, month) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD)) 

mld<-mld %>% group_by(Island) %>% 
  mutate(time_num = scale(time)[,1]) %>% 
  group_by(Island, month) %>% 
  mutate(month_mean = mean(MLD)) %>% 
  ungroup() %>% 
  mutate(anomaly = MLD - month_mean) %>% 
  group_by(Island) %>% mutate(anomaly_s = scale(anomaly)[,1]) %>% 
  left_join(island %>% rename(Island = island) %>% select(Island, region)) 

# MLD mean over past 3 months (inclusive of that month)
mld_recent<-mld %>% 
  mutate(mean_mld_3months = zoo::rollmean(MLD, k = 3, align = "right", fill = NA))

# tidal conversion = stronger means more internal wave action + mixing, increases planktivores
tc_all<-read.csv('data/crep_oceanographic/TEDestimates_CREPislands.csv') %>% 
  left_join(island %>% mutate(ISLAND = island_code, island = island) %>% select(ISLAND, island, region)) %>% 
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

tc<-tc_all %>% 
  group_by(ISLAND, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM), ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))

tc_C<-tc_all %>% 
  group_by(island_group, region) %>% 
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

# add precip, MLD and TC to island and island complex
island<-island %>% 
  left_join(precip_ann) %>% 
  left_join(mld_avg %>% rename(island = Island)) %>% 
  left_join(tc %>% mutate(island_code = ISLAND) %>% ungroup() %>% select(-ISLAND, -ted_sd)) %>% 
  left_join(island_cols)

island_complex<-island_complex %>% 
  left_join(precip_ann_C) %>% 
  left_join(mld_avg_C) %>% 
  left_join(tc_C %>% ungroup() %>% select(-ted_sd, -region), by = 'island_group') %>% 
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
  pivot_longer(-c(island_code, island, region,REGION, geomorphic_type, latitude, longitude, region.col, island_group), names_to = 'cov', values_to = 'val') %>% 
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

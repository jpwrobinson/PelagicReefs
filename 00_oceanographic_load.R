library(tidyverse)
library(janitor)
source('00_plot_theme.R')
source('00_islands.R')

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
# mld<-read.csv('data/crep_oceanographic/MLD_All_Islands-lrg_island_means.csv') %>% 
mld<-read.csv('data/glorys/mld_1993-2021_glory_island.csv') %>% 
  filter(island != 'Maro Reef') %>% 
  mutate(Date = as.Date(time), year = year(Date), month = month(Date), 
         MLD = mean, mean = NULL,
         time = as.numeric(Date), below_30m = ifelse(MLD > 30, 'deep', 'shallow'),
         .before=island) %>% 
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

pdf(file = 'fig/crep_island_MLD.pdf', height=7, width=15)
print(
  ggplot(mld, aes(Date, MLD)) + geom_line() + facet_wrap(~island)
)
dev.off()

# Island values - mean
mld_avg<-mld %>% group_by(island, year) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD),
            mld_months_deep = n_distinct(Date[MLD > 30])) %>% 
  group_by(island) %>% 
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

# MLD by month  
mld_month<-mld %>% group_by(island, month) %>% 
  summarise(mld = mean(MLD),
            mld_sd = sd(MLD)) 

# MLD anomaly: MLD difference from monthly mean at each island
mld<-mld %>% group_by(island) %>% 
  mutate(time_num = scale(time)[,1]) %>% 
  group_by(island, month) %>% 
  mutate(month_mean = mean(MLD)) %>% 
  ungroup() %>% 
  mutate(anomaly = MLD - month_mean) %>% 
  group_by(island) %>% mutate(anomaly_s = scale(anomaly)[,1]) %>% 
  left_join(island %>% select(island, region)) 


# MLD mean over past 3 months (inclusive of that month)
mld_recent<-mld %>% 
  mutate(mean_mld_3months = zoo::rollmean(MLD, k = 3, align = "right", fill = NA))

## Sea Surface Height
source('read_ssh_godas.R')
source('read_tidal_energy.R')

# add precip, SSH, MLD and TC to island and island complex
island<-island %>% 
  left_join(precip_ann) %>% 
  left_join(mld_avg) %>% 
  left_join(ssh_vals) %>% 
  left_join(tc %>% mutate(island_code = ISLAND) %>% ungroup() %>% select(-ISLAND, -ted_sd)) %>% 
  left_join(island_cols)

island_complex<-island_complex %>% 
  left_join(precip_ann_C) %>% 
  left_join(mld_avg_C) %>% 
  left_join(ssh_vals_C) %>% 
  left_join(tc_C %>% ungroup() %>% select(-ted_sd, -region), by = 'island_group') %>% 
  left_join(island_cols)


# island oceanographic summaries
levs<-island %>% distinct(region, island_code, latitude) %>% 
  group_by(region) %>% arrange(island_code, latitude, .by_group=TRUE) %>% pull(island_code)

pdf(file = 'fig/crep_island_oceanography.pdf', height=6, width=17)
print(
  # Island mean values
island %>% 
  mutate(island_code = factor(island_code, levels = rev(levs))) %>% 
  rename(
         tidal_mean_W_m1 = ted_mean,
         tidal_sum_W_m1 = ted_sum,
         sea_surface_height_m = ssh,
         mld_avg_m = mld,
         n_months_deep_mld = mld_months_deep,
         mld_sd_m = mld_sd) %>% 
  pivot_longer(-c(island_code, island, region,REGION, geomorphic_type, latitude, longitude, 
                  region.col, island_group, n_grids, irradiance_einsteins_m2_d1_mean), names_to = 'cov', values_to = 'val') %>% 
  ggplot(aes(island_code, val, fill=region.col)) + geom_col() +
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
  pairs2(island %>% select(ted_mean, ted_sum, mld, mld_sd,mld_months_deep,ssh,
                         sst_mean, wave_energy_mean_kw_m1,chl_a_mg_m3_mean,
                         irradiance_einsteins_m2_d1_mean, reef_area, latitude, longitude) %>%
         na.omit()))
dev.off()

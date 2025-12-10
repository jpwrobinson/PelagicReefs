
# ime = mean upwelling %
ime_island<-read.csv(file = 'island_ime_dat.csv') %>% select(-lon, -lat, -type)
ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% select(-lon, -lat, -type)

hist(ime_island$mean_ime_percent) # Gamma

# dim(dat) = 30 island (complexes)
dat<-ime_island %>% left_join(
  island_complex %>% ungroup() %>% 
    mutate(island = str_replace_all(island_group, '_C', '')) %>%
    select(island, island_group, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))


# missing islands
island$island[!island$island %in% ime_island$island]
# ime_island %>%  filter(str_detect(island, 'L')) %>% distinct(island) %>% data.frame

# missing CREP from modelled dataset
island %>% filter(!island %in% dat$island) %>% data.frame
# but note that we are using island complex, so this captures 
# 'Maui, Lanai, Molokai, Lanai, Kahoolawe' = 'Maui_C',
# 'Saipan, Tinian, Aguijan' = 'Saipan_C',
# 'Ofu, Olosega, Tau' = 'Tau_C'

# 6 missing CREP islands in modelled dataset
unique(island$island[!island$island %in% dat$island]) 
# "Ofu & Olosega" "Lanai"         "Molokai"         "Tinian"        "Aguijan"   
# but these are because IME dataset contains IME for 'lead' island (Maui, Saipan, Tau)
# Maro Reef is NA

# These islands are missing tidal energy values
island_complex %>% filter(island_group %in% c('Johnston', 'Necker', 'Nihoa')) %>% data.frame

# ime_island %>%  filter(str_detect(island, 'Laysan')) %>% 
#   distinct(island) %>% data.frame

# crep_depth<-read.csv('data/noaa-crep/crep_bathymetry_merged.csv')
# island %>% filter(!island %in% crep_depth$ISLAND) %>% distinct(island) %>% data.frame

# csv of modelled data
dat %>% distinct(island, lat, lon, REGION, geomorphic_type) %>% write.csv('ime_complex_crep_lat_lon.csv', row.names=FALSE)
island %>% distinct(island, latitude, longitude, REGION, geomorphic_type) %>% write.csv('ime_island_crep_lat_lon.csv', row.names=FALSE)

# dim(dat_month) = 420 (12 * 35)
dat_month<-ime_month %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  left_join(island_complex %>% ungroup() %>% 
              mutate(island = str_replace_all(island_group, '_C', '')) %>%
              select(island, REGION, region.col, sst_mean:ted_sum, -mld, -mld_sd, -avg_monthly_mm),
            by = 'island') %>% 
  left_join(mld_month %>% ungroup() %>% 
              mutate(month_num=month) %>% 
              select(month_num, island, mld)) %>% 
  left_join(ssh_vals_m %>% ungroup() %>% 
              select(island, month_num, ssh)) %>% 
  left_join(precip %>% ungroup() %>% 
              mutate(month_num=month) %>% 
              select(month_num, island, avg_monthly_mm), by = c('month_num', 'island')) %>% 
  mutate(avg_monthly_mm = ifelse(is.na(avg_monthly_mm), 0, avg_monthly_mm)) %>% 
  filter(!is.na(mld)) %>% 
  group_by(island) %>% 
  mutate(chl_anom = Chl_max - chl_a_mg_m3_mean,
         mld_lag1 = mld[c(12, 1:11)], mld_lag2 = mld[c(11:12, 1:10)]) %>% 
  ungroup()

pdf(file = 'fig/ime_db/ime_month_crep.pdf', height=5, width=12)
ggplot(dat_month, aes(month_num, Chl_increase_nearby, col=island)) + 
  geom_line() + facet_wrap(~REGION) +
  geom_text(data = dat_month %>% group_by(island) %>% 
              slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
  theme(legend.position = 'none') +
  scale_y_continuous(limits=c(0, 220))

ggplot(dat_month, aes(month_num, chl_anom, col=island)) + 
  geom_line() + facet_wrap(~REGION) +
  geom_text(data = dat_month %>% group_by(island) %>% 
              slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
  theme(legend.position = 'none') 
dev.off()


dat_scaled<-dat %>%  
  select(island:island_area_km2, mean_chl_percent, REGION:ted_sum) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, avg_monthly_mm, sst_mean:ted_sum, -geomorphic_type,-population_status, -mean_chl_percent), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit()

dat_scaled_month<-dat_month %>% 
  select(island:island_area_km2, month, month_num, Chl_increase_nearby, chl_anom, REGION:mld_lag2) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(month_num, island_area_km2, reef_area_km2, avg_monthly_mm, sst_mean:irradiance_einsteins_m2_d1_mean, 
                  bathymetric_slope, ssh:mld_lag2), 
                ~terra::scale(., center=TRUE, scale=TRUE)[,1]),
         population_status_num = ifelse(population_status == 'U', 0, 1))

# Create pairs plot for IME covariates
pdf(file = 'fig/crep_island_month_correlations.pdf', height=7, width=15)
pairs2(
  dat_scaled_month %>% 
    filter(!is.na(bathymetric_slope) & !is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% 
    select(island_area_km2, reef_area_km2, bathymetric_slope,avg_monthly_mm,population_status_num,
           sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld, ssh, ted_mean, ted_sum, month_num))
dev.off()

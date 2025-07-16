source('00_plot_theme.R')

# Is the strength of upwelling (IME) linked to MLD and tidal conversion?

# exp. vars = MLD + tidal conversion
source('00_oceanographic_load.R')

# ime = mean upwelling %
ime_island<-read.csv(file = 'island_ime_dat.csv') 
ime_month<-read.csv(file = 'island_ime_month_dat.csv')

hist(ime_island$mean_ime_percent) # Gamma

# dim(dat) = 30 islands
dat<-ime_island %>% left_join(
  island %>% select(island, island_code, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))

# dim(dat_month) = 360 (12 * 30)
dat_month<-ime_month %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  left_join(island %>% 
              select(island, island_code, REGION, region.col, 
                     sst_mean:ted_sum, -mld, -mld_sd),
  by = 'island') %>% 
  left_join(mld_month %>% ungroup() %>% 
              mutate(island=Island, month_num=month) %>% 
              select(month_num, island, mld)) %>% 
  filter(!is.na(mld))

pdf(file = 'fig/ime_db/ime_month_crep.pdf', height=5, width=12)
ggplot(dat_month, aes(month_num, Chl_increase_nearby, col=island)) + 
  geom_line() + facet_wrap(~REGION) +
  geom_text(data = dat_month %>% group_by(island) %>% 
              slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
  theme(legend.position = 'none') +
  scale_y_continuous(limits=c(0, 220))
dev.off()


dat_scaled<-dat %>%  
  select(island:island_area_km2, mean_chl_percent, island_code:ted_sum) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, sst_mean:ted_sum, -geomorphic_type,-population_status, -mean_chl_percent), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit()

dat_scaled_month<-dat_month %>% 
  select(island:island_area_km2, month, month_num, Chl_increase_nearby, island_code:mld) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, sst_mean:mld, -geomorphic_type, -population_status, -Chl_increase_nearby), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit()

# 6 missing CREP islands in IME dataset
unique(island$island[!island$island %in% ime_island$island]) 
# "Ofu & Olosega" "Lanai"         "Molokai"         "Tinian"        "Aguijan"     "Maro Reef
ime_island %>%  filter(str_detect(island, 'ala')) %>% 
  distinct(island) %>% data.frame

# Create pairs plot for IME covariates
pairs2(
  dat_scaled_month %>% 
    select(island_area_km2, reef_area_km2, bathymetric_slope,
             sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
             chl_a_mg_m3_mean, mld, ted_mean, ted_sum))

# y distributions
hist(dat$mean_chl_percent)
hist(dat_month$Chl_increase_nearby)

ggplot(dat, aes(mld, mean_chl_percent, col=REGION)) + geom_point()

# basic model fitting Chl increase (%) by  island and biophysical covariates
m<-brm(mean_chl_percent ~ geomorphic_type * reef_area_km2 + island_area_km2 + 
         bathymetric_slope + population_status +
         # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
         chl_a_mg_m3_mean + mld + ted_mean +
         (1 | REGION),
       family = lognormal(), data = dat_scaled,
       chains = 3, iter = 3000, warmup = 500, cores = 4)

save(dat, dat_scaled, m, file = 'results/mod_ime_crep_attributes.rds')

summary(m)
pp_check(m)
conditional_effects(m)

# Extract posterior draws
effects <- m %>%
  gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
               b_bathymetric_slope, b_population_statusU,
               b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_crep_model.pdf', height=5, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 
  
dev.off()


# basic model fitting Chl increase (%) by month, island and biophysical covariates
m2<-brm(Chl_increase_nearby ~ s(month_num, bs = 'cc', k=12) + 
          geomorphic_type * reef_area_km2 + island_area_km2 + 
          bathymetric_slope + population_status +
         # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
         chl_a_mg_m3_mean + mld + ted_mean +
         (1 | island / REGION),
       family = lognormal(),
       data = dat_scaled_month,
       chains = 3, iter = 2000, warmup = 500, cores = 4)

save(dat_month, dat_scaled_month, m2, file = 'results/mod_ime_month_crep_attributes.rds')

summary(m2)
pp_check(m2)
conditional_effects(m2)
bayes_R2(m2)

# Extract posterior draws
effects2 <- m2 %>%
  gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
               b_bathymetric_slope, b_population_statusU,
               b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_month_crep_model.pdf', height=5, width=6)
ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 

dev.off()

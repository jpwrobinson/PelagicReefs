source('00_plot_theme.R')

# Is the strength of upwelling (IME) linked to MLD and tidal conversion?

# exp. vars = MLD + tidal conversion
source('00_oceanographic_load.R')

# ime = mean upwelling %664
ime_island<-read.csv(file = 'island_ime_dat.csv') 
ime_month<-read.csv(file = 'island_ime_month_dat.csv')

hist(ime_island$mean_ime_percent) # Gamma

# dim(dat) = 30 islands
dat<-ime_island %>% left_join(
  island %>% select(island, island_code, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))

# missing islands
island$island[!island$island %in% ime_island$island]
ime_island %>%  filter(str_detect(island, 'L')) %>% distinct(island) %>% data.frame

ggplot(dat, aes(chl_a_mg_m3_mean, mean_ime_percent)) + geom_point()

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
  select(island:island_area_km2, mean_chl_percent, island_code:ted_sum) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, sst_mean:ted_sum, -geomorphic_type,-population_status, -mean_chl_percent), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit()

dat_scaled_month<-dat_month %>% 
  select(island:island_area_km2, month, month_num, Chl_increase_nearby, chl_anom, island_code:mld_lag2) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(month_num, island_area_km2, reef_area_km2, sst_mean:irradiance_einsteins_m2_d1_mean, 
                  bathymetric_slope, ted_mean:mld_lag2), 
                ~terra::scale(., center=TRUE, scale=TRUE)[,1])) %>% na.omit()

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
             chl_a_mg_m3_mean, mld, mld_anom, ted_mean, ted_sum))

ggplot(dat_month, aes(month, mld, group=island)) +
  geom_line() + facet_grid(~REGION)

# y distributions
hist(dat$mean_chl_percent)
hist(dat_month$Chl_increase_nearby)

ggplot(dat, aes(mld, mean_chl_percent, col=REGION)) + geom_point()

# # basic model fitting Chl increase (%) by  island and biophysical covariates
# m<-brm(mean_chl_percent ~ geomorphic_type * reef_area_km2 + island_area_km2 + 
#          bathymetric_slope + population_status +
#          # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
#          chl_a_mg_m3_mean + mld + ted_mean +
#          (1 | REGION),
#        family = lognormal(), data = dat_scaled,
#        chains = 3, iter = 3000, warmup = 500, cores = 4)
# 
# save(dat, dat_scaled, m, file = 'results/mod_ime_crep_attributes.rds')
# 
# summary(m)
# pp_check(m)
# conditional_effects(m)
# 
# # Extract posterior draws
# effects <- m %>%
#   gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
#                b_bathymetric_slope, b_population_statusU,
#                b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>%  
#   mutate(.variable = str_replace_all(.variable, 'b_', ''),
#          var_fac = factor(.variable, 
#                           levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
#                                          'bathymetric_slope', 'population_statusU',
#                                          'ted_mean', 'mld','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1'))))
# 
# # Plot effect sizes
# pdf(file = 'fig/ime_db/ime_crep_model.pdf', height=5, width=6)
# ggplot(effects, aes(x = .value, y = var_fac)) +
#   stat_halfeye(.width = c(0.5, 0.95)) +  
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
#   labs(x = "Effect size", y = "") 
#   
# dev.off()


# basic model fitting Chl increase (%) by island and biophysical covariates
m2_linear<-brm(chl_anom ~ 
                 geomorphic_type * reef_area_km2 + island_area_km2 + 
                 bathymetric_slope + population_status +
                 # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
                 chl_a_mg_m3_mean + mld + ted_mean +
                 (1 | island / REGION),
               # family = lognormal(),
               data = dat_scaled_month,
               chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_smooth<-brm(Chl_increase_nearby ~ 
          geomorphic_type + s(reef_area_km2, k=3) + s(island_area_km2, k=3) + 
          s(bathymetric_slope, k=3) + population_status +
         # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
         s(chl_a_mg_m3_mean, k=3) + s(mld, k=3) + s(ted_mean, k=3) +
         (1 | island / REGION),
       family = lognormal(),
       data = dat_scaled_month,
       chains = 3, iter = 2000, warmup = 500, cores = 4)

# use month smoother instead of MLD
m2_month_full<-brm(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12) + 
          geomorphic_type * reef_area_km2 + island_area_km2 + 
          bathymetric_slope + population_status +
          # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
          chl_a_mg_m3_mean + ted_mean + mld +
          (1 | island / REGION),
        family = lognormal(),
        data = dat_scaled_month,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

# smooth MLD as seasonal
m2_mld_smoo<-brm(Chl_increase_nearby ~ 
                     geomorphic_type * reef_area_km2 + island_area_km2 + 
                     bathymetric_slope + population_status +
                     # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
                     chl_a_mg_m3_mean + ted_mean + s(mld) +
                     (1 | island / REGION),
                   family = lognormal(),
                   data = dat_scaled_month,
                   chains = 3, iter = 2000, warmup = 500, cores = 4)

save(dat_month, dat_scaled_month, m2_linear, m2_smooth, m2_month_full, file = 'results/mod_ime_month_crep_attributes.rds')

checker<-m2_linear
summary(checker)
pp_check(checker)
conditional_effects(checker)
bayes_R2(checker)

# compare smooth vs linear
loo_linear <- loo(m2_linear)
loo_smooth <- loo(m2_smooth)
loo_month_full <- loo(m2_month_full)
loo_mld_smoo <- loo(m2_mld_smoo)
loo_compare(loo_linear, loo_smooth, loo_month_full, loo_mld_smoo)

# Extract posterior draws
effects2 <- m2_linear %>%
  gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
               b_bathymetric_slope, b_population_statusU,
               b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_month_crep_model.pdf', height=5, width=6)
ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 

dev.off()


# What about lags [month is better than MLD...but not a process]
m2_mld0<-brm(Chl_increase_nearby ~ mld, family = lognormal(), data = dat_scaled_month,
              chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_mld1<-brm(Chl_increase_nearby ~ mld_lag1, family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_mld2<-brm(Chl_increase_nearby ~ mld_lag2, family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_month<-brm(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12), family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_mld0,m2_mld1, m2_mld2, m2_month, file = 'results/mod_ime_month_vs_mld_crep_loo.rds')

# compare mld lag with MLD model
loo0 <- loo(m2_mld0)
loo1 <- loo(m2_mld1)
loo2 <- loo(m2_mld2)
looM <- loo(m2_month)
loo_compare(loo0, loo1, loo2, looM) # No lag is supported

nd<-dat_scaled_month %>% select(month_num, island)
condo<-conditional_effects(m2_month, 'month_num', prob=0.95,
                           conditions = nd, re_formula=NULL)[[1]] %>% 
  mutate(lower95 = lower__, upper95 = upper__) %>% 
  left_join(dat_scaled_month %>% select(island, REGION))

ggplot(condo, aes(month_num, estimate__, ymax= upper95, ymin = lower95, group=island)) + 
  geom_ribbon(alpha=0.1) +
  geom_line() + facet_grid(~REGION, space='free_y') + lims(y = c(0, 500))

ggplot(dat_month, aes(month_num,mld, group=island)) + 
  geom_line() + 
  geom_point(aes(col=Chl_increase_nearby)) + 
  facet_grid(~REGION, space='free_y') + lims(y = c(0, 200))

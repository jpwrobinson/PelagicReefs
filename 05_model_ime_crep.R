library(GGally)
detach('package:terra')

# Is the strength of upwelling (IME) linked to MLD and tidal conversion?

# ime = mean upwelling %
ime_island<-read.csv(file = 'island_ime_dat.csv') 

hist(ime_island$mean_ime_percent) # Gamma

# exp. vars = MLD + tidal conversion
source('00_oceanographic_load.R')

dat<-ime_island %>% left_join(
  island %>% select(island, island_code, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))

dat_scaled<-dat %>% 
  mutate(type = factor(type), reef_area_km2 = log10(reef_area_km2.y+1), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2:ime_diff, sst_mean:ted_sum, -atoll_island, -mean_ime_percent), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit

# 6 missing CREP islands in IME dataset
unique(island$island[!island$island %in% ime_island$island]) 
# "Ofu & Olosega" "Lanai"         "Molokai"         "Tinian"        "Aguijan"     "Maro Reef
ime_island %>%  filter(str_detect(island, 'ala')) %>% 
  distinct(island) %>% data.frame

# Create pairs plot for IME covariates
pairs2(
  dat_scaled %>% 
    select(island_area_km2, reef_area_km2.y, 
             sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
             chl_a_mg_m3_mean, mld, mld_months_deep, ted_mean, ted_sum))
hist(dat$mean_ime_percent)

# basic model fitting Chl increase (%) by month / island and biophysical covariates
m<-brm(mean_ime_percent ~ atoll_island + island_area_km2 + reef_area_km2 + 
         sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
         chl_a_mg_m3_mean + mld + ted_mean +
         (1 | REGION),
       family = exponential, data = dat_scaled,
       chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m, file = 'results/mod_ime_crep_attributes.rds')

summary(m)
conditional_effects(m)

# Extract posterior draws
effects <- m %>%
  spread_draws(b_atoll_islandIsland, b_reef_area_km2, b_island_area_km2,
               b_chl_a_mg_m3_mean, b_wave_energy_mean_kw_m1, b_ted_mean, b_mld) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect") %>% 
  mutate(Variable = str_replace_all(Variable, 'b_', ''),
         var_fac = factor(Variable, 
                          levels = rev(c('atoll_islandIsland','reef_area_km2','island_area_km2',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_crep_model.pdf', height=5, width=6)
ggplot(effects, aes(x = Effect, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 
  # scale_x_continuous(breaks=seq(-1, 1, by = 0.5), limits=c(-1.1, .6), 
  #                    sec.axis = dup_axis()) 
dev.off()

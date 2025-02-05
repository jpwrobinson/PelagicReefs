library(brms)
ime<-read.csv(file = 'island_ime_dat.csv')
depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv')
depth_ime<-depth %>% 
  filter(ISLAND %in% ime$island) %>% 
  mutate(island = ISLAND) %>% 
  left_join(ime)
 
# From Richardson sup mat 
fix<- ~ DEPTH_c +
  POP_STATUS +
  SITE_SLOPE_400m_c +
  # DEPTH_c:POP_STATUS +
  #s(DEPTH_c, by=POP_STATUS) +
  # s(DEPTH_c, SITE_SLOPE_400m_c)
  (1|OBS_YEAR) +
  # (1|OBS_YEAR:ECOREGION) +
  # (1|OBS_YEAR:ISLAND) +
  (1|ISLAND) +
  # (1|ECOREGION)  +
  (1|SITE) 
  # (1|DIVER)
  # (1+DEPTH_c||ISLAND)

fix2 <- ~DEPTH_c +
  POP_STATUS +
  SITE_SLOPE_400m_c +
  mean_ime_percent + max_chl + months_ime +
  (1|OBS_YEAR) +
  (1|ISLAND) +
  (1|SITE) 

# with Richardson covariates
pos_form <- formula(paste(c('PLANKTIVORE', fix), collapse = " ")) # Formula for models with positive data only
bin_form <- formula(paste(c('hu', fix), collapse = " "))

# with IME covariates
pos_form2 <- formula(paste(c('PLANKTIVORE', fix2), collapse = " ")) # Formula for models with positive data only
bin_form2 <- formula(paste(c('hu', fix2), collapse = " "))


m1<-brm(bf(pos_form, bin_form), family = hurdle_gamma, data = depth_ime,
    chains = 3, iter = 2000, warmup = 500, cores = 4)

m1_ime<-brm(bf(pos_form2, bin_form2), family = hurdle_gamma, data = depth_ime,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

summary(m1_ime)
loo(m1, m1_ime)

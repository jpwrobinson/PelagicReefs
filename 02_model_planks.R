library(brms)
library(fuzzyjoin)

depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv')

# read ime and change island names
ime<-read.csv(file = 'island_ime_dat.csv') %>% 
  mutate(island2 = trimws(str_replace_all(island, 'Atoll', '')),
         island2 = trimws(str_replace_all(island2, 'Island', '')),
         island2 = trimws(str_replace_all(island2, 'Reef', '')),
         island2 = trimws(str_replace_all(island2, '\\ and', '\\ &')),
         island2 = case_match(island2, 
                              'Hawai’i' ~ 'Hawaii',
                              'French Frigate Shoals' ~ 'French Frigate',
                              'Kaua’i' ~ 'Kauai',
                              'Ni’ihau' ~ 'Niihau',
                              'Swains  (Olohega)' ~ 'Swains',
                              'Ta’u' ~ 'Tau', .default = island2))

# 6 missing islands in depth
unique(depth$ISLAND[!depth$ISLAND %in% ime$island2]) 


depth_ime<-depth %>% 
  filter(ISLAND %in% ime$island2) %>% 
  mutate(island = ISLAND) %>% 
  left_join(ime) %>% 
  mutate(across(c(DEPTH_c, SITE_SLOPE_400m_c, mean_ime_percent, max_chl, months_ime), 
                ~scale(., center=TRUE, scale=TRUE)))

# 29 islands, excluding 6 islands, mostly large MHI or Marianas

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
  SITE_SLOPE_400m_c + #* max_chl +
  # mean_ime_percent + 
  months_ime*max_chl +
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

library(brms)
ime<-read.csv(file = 'island_ime_dat.csv')
depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv')
depth_ime<-depth %>% 
  filter(ISLAND %in% ime$island) %>% 
  mutate(island = ISLAND) %>% 
  left_join(ime)
 

# From Richardson sup mat 
# form <- ~ DEPTH_c +
#   POP_STATUS +
#   SITE_SLOPE_400m_c +
#   DEPTH_c:POP_STATUS +
#   #s(DEPTH_c, by=POP_STATUS) +
#   s(DEPTH_c, SITE_SLOPE_400m_c) +
#   (1|OBS_YEAR) +
#   (1|OBS_YEAR:ECOREGION) + 
#   (1|OBS_YEAR:ISLAND) + 
#   (1|ECOREGION)  + 
#   (1|SITE) +
#   (1|DIVER) +
#   (1+DEPTH_c||ISLAND)



m1<-brm(PLANKTIVORE ~ DEPTH_c +
    POP_STATUS +
    SITE_SLOPE_400m_c +
    DEPTH_c:POP_STATUS +
    s(DEPTH_c, SITE_SLOPE_400m_c) +
    (1|OBS_YEAR) +
    (1|OBS_YEAR:ECOREGION) +
    (1|OBS_YEAR:ISLAND) +
    (1|ECOREGION)  +
    (1|SITE) +
    (1|DIVER) +
    (1+DEPTH_c||ISLAND), 
    family = hurdle_gamma, data = depth_ime,
    chains = 3, iter = 2000, warmup = 500, cores = 4)

m1<-brm(PLANKTIVORE ~ DEPTH_c +
          POP_STATUS +
          SITE_SLOPE_400m_c +
          DEPTH_c:POP_STATUS +
          s(DEPTH_c, SITE_SLOPE_400m_c) +
          max_chl + mean_ime_percent + months_ime +
          (1|OBS_YEAR) +
          (1|OBS_YEAR:ECOREGION) +
          (1|OBS_YEAR:ISLAND) +
          (1|ECOREGION)  +
          (1|SITE) +
          (1|DIVER) +
          (1+DEPTH_c||ISLAND), 
        family = hurdle_gamma, data = depth_ime,
        chains = 3, iter = 2000, warmup = 500, cores = 4)



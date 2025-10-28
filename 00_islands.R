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


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

## Nihoa doesn't have bathymetry but does have reef and island area in a separate table
island_complex$reef_area[island_complex$island_group=='Nihoa']<-island$reef_area[island$island=='Nihoa']
island_complex$geomorphic_type[island_complex$island_group=='Nihoa']<-island$geomorphic_type[island$island=='Nihoa']
island_complex$lat[island_complex$island_group=='Nihoa']<-island$latitude[island$island=='Nihoa']
island_complex$lon[island_complex$island_group=='Nihoa']<-island$longitude[island$island=='Nihoa']
island_complex$population_status[island_complex$island_group=='Nihoa']<-'U'


## Island shapefiles
# islands_dat <- read.csv("ime_island_crep_lat_lon.csv")
# 
# # shapefiles from https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273
# # Global Islands database from USGS
# # Sayre, R., 2023, Global Islands: U.S. Geological Survey data release, https://doi.org/10.5066/P91ZCSGM.
# # https://gkhub.earthobservations.org/records/5wg96-bvv84?package=7yjze-2g558
# 
# # islands_big <- st_read("spatial/usgs_islands/usgs_globalislandsv2_bigislands.shp")
# # islands_small <- st_read("spatial/usgs_islands/usgs_globalislandsv2_smallislands.shp")
# # islands_verysmall <- st_read("spatial/usgs_islands/usgs_globalislandsv2_verysmallislands.shp")
# #
# # islands <- bind_rows(islands_big, islands_small, islands_verysmall)
# # st_write(islands, "spatial/usgs_islands/merged/islands.shp")
# 
# islands<-st_read("spatial/usgs_islands/merged/islands.shp")
# 
# lister<-islands_dat$island[islands_dat$geomorphic_type=='Island']
# 
# # filter islands. doing this manually to catch all islands. atolls are missed and excluded.
# islands2<-islands %>%  filter(Name_USGSO %in% lister) %>% mutate(island = Name_USGSO) %>%
#         filter(!island == 'Baker')
# islands3<-islands %>%  filter(NAME_wcmcI %in% c('Howland', 'Alamagan', 'Swains', 'Tau', 'Laysan', 'Nihoa')) %>% mutate(island = NAME_wcmcI)
# # Maug - multiple islands but drop those caught from Belize
# maug<-islands %>% filter(str_detect(NAME_wcmcI, 'Maug')) %>% filter(!str_detect(NAME_wcmcI, 'Mauger')) %>% mutate(island = 'Maug')
# # Necker in NWHI
# necker<-islands %>% filter(str_detect(NAME_wcmcI, 'Mokumanamana')) %>% mutate(island = 'Necker')
# ofu<-islands %>% filter(NAME_wcmcI %in% c('Nuusilaelae Island', 'Olosega')) %>% mutate(island = 'Ofu & Olosega')
# 
# # Nuusilaelae Island is misnamed in island database. Confirmed with map plots that it is Ofu.
# # islands %>% filter(str_detect(NAME_wcmcI, 'Manua'))
# 
# # checked Baker is correct lat-lon (out of 18 possible Bakers)
# baker<-islands %>% filter(NAME_wcmcI == 'Baker' & OBJECTID == 799) %>% mutate(island='Baker')
# 
# # bind. n = 33
# isl_dat<-bind_rows(islands2, islands3, maug, necker, ofu, baker) %>%
#   left_join(islands_dat)
# 
# # all atolls missing
# islands_dat %>% filter(!island %in% isl_dat$island)
# 
# # save isl_dat
# saveRDS(isl_dat, file = 'data/noaa_island_shp.rds')


# get coords to check locations
# rs<-islands %>% filter(str_detect(Name_USGSO, 'Baker'))
# isl_ll <- st_transform(rs, crs = 4326)
# centroids <- st_centroid(isl_ll$geometry)
# coords <- st_coordinates(centroids)
# rs$coords<-coords

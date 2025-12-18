source('00_plot_theme.R')

crep_full<-read.csv('data/noaa-crep/crep_full_merged.csv')
crep_depth<-read.csv('data/noaa-crep/crep_bathymetry_merged.csv')

sites_bathy<-crep_depth %>% distinct(ISLAND, SITEVISITID, lon, lat) %>% data.frame()
new_sites<-crep_full %>% distinct(ISLAND, SITEVISITID, lon, lat) %>% data.frame()

# perform site bathymetry matching
sites_bathy <- st_as_sf(sites_bathy, coords = c("lon", "lat"), crs = 4326)
new_sites <- st_as_sf(new_sites, coords = c("lon", "lat"), crs = 4326)

# identify nearest site ID
nearest_idx <- st_nearest_feature(new_sites, sites_bathy)
new_sites$nearest_site <- sites_bathy[nearest_idx,]$SITEVISITID
new_sites$nearest_site_geo <- sites_bathy[nearest_idx, ]$geometry

# estimate distance to nearest site [m]
dist_mat<-st_distance(new_sites$geometry, new_sites$nearest_site_geo, by_element=TRUE)
new_sites$site_distance_m<-as.numeric(dist_mat)

# add bathy data
new_sites<-new_sites %>% 
  left_join(crep_depth %>% distinct(SITEVISITID, SITE_SLOPE_400m)) %>% 
  mutate(site_bathy = ifelse(is.na(SITE_SLOPE_400m), 'FALSE', 'TRUE'))

## sites with far distances are for Laysan and Wake which have zero bathy data
new_sites %>% filter(site_distance_m > 100000) %>% distinct(ISLAND)

new_sites %>% filter(SITEVISITID == nearest_site) %>% dim # 2250 (33% of sites have bathy data)
new_sites %>% filter(SITEVISITID != nearest_site & site_distance_m < 400) %>% dim # 2093 (31% of sites are within 400m of bathy)
new_sites %>% filter(SITEVISITID != nearest_site & site_distance_m > 400) %>% dim # 2426 (35% of sites are beyond 400m of bathy data)

island.vec<-unique(new_sites$ISLAND)
pdf(file = 'fig/crep_bathymetry_matching.pdf', height=5, width=7)
for(i in 1:length(island.vec)){
  
  dat<-new_sites %>% filter(ISLAND==island.vec[i])
  
  g2<-ggplot(dat, aes(col=site_distance_m)) + 
    geom_sf() +
    labs(col = 'Dist. from bathymetry, m', subtitle = island.vec[i]) +
    scale_colour_continuous(palette = c("#3182BD", "#9ECAE1","#DEEBF7", "#FEE0D2", "#FC9272", "#DE2D26"))
  
  g3<-ggplot(dat, aes(site_distance_m)) + geom_histogram() +
    labs(x = 'Dist. to bathymetry site, m') +
    geom_vline(xintercept=400, col='red', linetype=5) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0))
  
  print(
    plot_grid(g2, g3, rel_widths=c(1, 0.5), align='h')
  )
}

dev.off()

n<-new_sites %>% group_by(ISLAND) %>% 
  summarise(tot = n_distinct(SITEVISITID),
            n_bathy = n_distinct(SITEVISITID[SITEVISITID==nearest_site]),
            n = n_distinct(SITEVISITID[SITEVISITID != nearest_site & site_distance_m < 400])) %>% 
  mutate(prop_na_but_near_bathy = n / tot * 100,
         prop_with_bathy = n_bathy / tot * 100)

ggplot(n, aes(fct_reorder(ISLAND, prop_na_but_near_bathy), prop_na_but_near_bathy)) + 
  geom_col() + coord_flip() +
  labs(x = '', y = '% sites near to bathymetry site')


# potential to add 2093 sites
adder<-new_sites %>% filter(SITEVISITID != nearest_site & site_distance_m < 400) %>% 
  mutate(nearest_bathy_site = nearest_site,
         bathy_site_distance_m = site_distance_m)

adder$SITE_SLOPE_400m<-crep_depth$SITE_SLOPE_400m[match(adder$nearest_site, crep_depth$SITEVISITID)]

crep_bathy_fill<-read.csv('data/noaa-crep/crep_full_merged.csv') %>% 
  left_join(new_sites %>% mutate(nearest_bathy_site = nearest_site,
                                 bathy_site_distance_m = site_distance_m) %>% 
              select(SITEVISITID, nearest_bathy_site, bathy_site_distance_m) %>% st_drop_geometry())

crep_bathy_fill$SITE_SLOPE_400m[is.na(crep_bathy_fill$SITE_SLOPE_400m)]<-adder$SITE_SLOPE_400m[match(crep_bathy_fill$SITEVISITID[is.na(crep_bathy_fill$SITE_SLOPE_400m)], adder$SITEVISITID)]

crep_bathy_fill %>% filter(is.na(SITE_SLOPE_400m)) %>% summarise(n_distinct(SITEVISITID))
crep_bathy_fill %>% filter(!is.na(SITE_SLOPE_400m)) %>% summarise(n_distinct(SITEVISITID))
crep_bathy_fill %>% filter(!is.na(SITE_SLOPE_400m) & bathy_site_distance_m == 0) %>% summarise(n_distinct(SITEVISITID))
crep_bathy_fill %>% filter(!is.na(SITE_SLOPE_400m) & bathy_site_distance_m > 0) %>% summarise(n_distinct(SITEVISITID))

# 4343 sites with any bathy data
# 2252 sites with measured bathy data
# 2091 sites with nearby bathy data
# 2426 sites without nearby bathy data

write.csv(crep_bathy_fill, 'data/noaa-crep/crep_full_merged_bathymetry_fill.csv', row.names=FALSE)

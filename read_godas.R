library(sf)

# get reef lat lon
source('00_islands.R')
latlon<-data.frame(lon = island$longitude, lat = island$latitude, island_group=island$island_group)
# align projection systems - longitude wrap
latlon$lon<-ifelse(latlon$lon < 0, 360+latlon$lon, latlon$lon)
# points(latlon)


# https://psl.noaa.gov/data/gridded/data.godas.html
# Behringer, D.W., M. Ji, and A. Leetmaa, 1998: An improved coupled model for ENSO prediction and implications for ocean initialization. Part I: The ocean data assimilation system. Mon. Wea. Rev., 126, 1013-1021.

# Long-term monthly means for SSH (m) relative to geoid for 1991-2020
# 0.333 degree latitude x 1.0 degree longitude global grid (418x360)
# 74.5S - 64.5N, 0.5E - 359.5E
ssh<-terra::rast('data/godas/sshg.mon.ltm.1991-2020.nc')

# plot(ssh) # this is a layer for each month
# names(ssh)
# crs(ssh)

# work with January to start
# jan<-ssh$sshg_1
# plot(jan)

ssh_vals_m<-terra::extract(ssh, latlon[,1:2]) %>% select(starts_with('sshg')) %>% 
  dplyr::mutate(mean_annual_ssh =rowMeans(across(everything())), island = island$island, island_group=latlon$island_group) %>% 
  pivot_longer(-c(mean_annual_ssh, island, island_group), names_to = 'month', values_to = 'ssh') %>% 
  mutate(month_num = as.numeric(str_replace_all(month, 'sshg_', '')),
         month = month.abb[month_num])

ssh_vals_m_C<-ssh_vals_m %>% group_by(island_group, month_num, month) %>% 
  summarise(ssh = mean(ssh))

ssh_vals<-ssh_vals_m %>% group_by(island) %>% summarise(ssh = mean(ssh))
ssh_vals_C<-ssh_vals_m_C %>% group_by(island_group) %>% summarise(ssh = mean(ssh))


# Long-term monthly means for ocean mixed layer depth below sea surface (m) for 1991-2020
# 0.333 degree latitude x 1.0 degree longitude global grid (418x360)
# 74.5S - 64.5N, 0.5E - 359.5E

# this is the long-term monthly mean
# mld<-terra::rast('data/godas/dbss_obml.mon.ltm.1991-2020.nc')

# plot tester
# mhi_extent <- terra::ext(min(latlon$lon)-5, max(latlon$lon)+5, min(latlon$lat)-5, max(latlon$lat)+5)
# mld_mhi <- terra::crop(mld, mhi_extent)
# mld_df <- as.data.frame(mld_mhi, xy = TRUE, na.rm = TRUE)
# names(mld_df)[3] <- "mld"
# 
# ggplot(mld_df) +
#   geom_raster(aes(x = x, y = y, fill = mld)) +
#   geom_point(data = latlon, aes(x = lon, y = lat)) + 
#   coord_quickmap() +
#   scale_fill_viridis_c(name = "Mixed Layer Depth (m)", na.value = "grey80") +
#   labs(
#     x = "Longitude (0–360°)",
#     y = "Latitude",
#     title = "Mixed Layer Depth around Pacific"
#   ) +
#   theme_minimal()

# mld_vals_m<-terra::extract(mld, latlon[,1:2]) %>% dplyr::select(starts_with('dbss_obml')) %>% 
#   dplyr::mutate(mean_annual_mld = rowMeans(across(everything())), island = island$island, island_group=latlon$island_group) %>% 
#   pivot_longer(-c(mean_annual_mld, island, island_group), names_to = 'month', values_to = 'mld') %>% 
#   mutate(month_num = as.numeric(str_replace_all(month, 'dbss_obml_', '')),
#          month = month.abb[month_num])


# Read directly from NOAA via HTTPS [shell script using wget in godas folder]
file.list <- list.files('data/godas', pattern = 'dbss_obml')
file.list <- setdiff(file.list, 'dbss_obml.mon.ltm.1991-2020.nc')

mld_vals_m<-numeric()
for (i in 1:length(file.list)){
  nc<-terra::rast(paste0('data/godas/', file.list[i]))
  yr<-str_split_fixed(file.list[i], '\\.', 3)[,2]
  
  vals<-terra::extract(nc, latlon[,1:2]) %>% select(starts_with('dbss_obml')) %>% 
    mutate(island = island$island, island_group=latlon$island_group) %>% 
    pivot_longer(-c(island, island_group), names_to = 'month', values_to = 'mld') %>% 
    mutate(month_num = as.numeric(str_replace_all(month, 'dbss_obml_', '')),
           month = month.abb[month_num],
           year = yr)
  
  mld_vals_m<-rbind(mld_vals_m, vals)
}


mld_vals_m_C<-mld_vals_m %>% group_by(island_group, month_num, month) %>% 
  summarise(mld = mean(mld))

mld_vals<-mld_vals_m %>% group_by(island) %>% summarise(mld = mean(mld))
mld_vals_C<-mld_vals_m_C %>% group_by(island_group) %>% summarise(mld = mean(mld))


## comparing with mld loaded in 00_oceanographic_load.R
tt<-mld_vals_m %>% 
  left_join(mld_month %>% mutate(mld2 = mld, month_num = month, island=Island) %>% select(island,month_num, mld2)) 

ggplot(tt, aes(mld, mld2)) + geom_point() + facet_wrap(~island)
tt %>% group_by(island) %>% summarise(cor(mld, mld2)) %>% data.frame
with(tt %>% na.omit, cor(mld, mld2))

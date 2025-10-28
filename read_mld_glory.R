library(sf)

# get reef lat lon and island shapes
source('00_islands.R')
isl<-readRDS('data/noaa_island_shp.rds') # shapefiles
latlon<-data.frame(lon = island$longitude, lat = island$latitude, island_group=island$island_group)
# align projection systems - longitude wrap
latlon$lon<-ifelse(latlon$lon < 0, 360+latlon$lon, latlon$lon)

# read mld, extract time
mld<-terra::rast('data/glorys/cmems_mod_glo_phy_my_0.083deg_P1M-m_mlotst_118.33E-289.42E_49.33S-56.58N_1993-01-01-2021-06-01.nc')
times<-terra::time(mld)

# align geometries (wrap sf object latitude)
isl <- isl %>% st_transform(terra::crs(mld)) %>% 
  st_shift_longitude()

# plot tester
mhi_extent <- terra::ext(min(latlon$lon)-5, max(latlon$lon)+5, min(latlon$lat)-5, max(latlon$lat)+5)
mld_mhi <- terra::crop(mld, mhi_extent)
mld_df <- as.data.frame(mld_mhi, xy = TRUE, na.rm = TRUE)
names(mld_df)[3] <- "mld"

ggplot(data=latlon) +
    geom_raster(data = mld_df, aes(x = x, y = y, fill = mld)) +
    geom_point(aes(x = lon, y = lat)) +
    geom_sf(data =isl) +
    coord_sf() +
    lims(x = c(201, 206), y = c(18.5,22.5)) +
    scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") +
    labs(
      x = "Longitude (0–360°)",
      y = "Latitude",
      title = "Mixed Layer Depth around Pacific"
    ) +
    theme_minimal()

# identify the cells that intersect each island polygon

## raster output version that we can plot
cropper<-terra::mask(mld_mhi$mlotst_1,isl)
cropper_df<-as.data.frame(cropper, xy=TRUE)
ggplot(cropper_df) + geom_raster( aes(x=x, y = y, fill=mlotst_1)) +
  geom_sf(data =isl) +
  lims(x = c(201, 206), y = c(18.5,22.5)) +
  scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") 

## value output version that we can assign to each island
vals <- terra::extract(mld_mhi$mlotst_1, isl, touches = TRUE, bind=TRUE, 
                       fun = function(x) {
    c(mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    na_cells = n_distinct(x[is.na(x)]))
}) 

# TODO : fix the bind and test if mask and extract are equivalent
# TODO: 


library(magick)
dir.create("frames", showWarnings = FALSE)

for(i in 3:dim(mld_df)[2]){
  mld_df$mld<-mld_df[,i]
  
  p<-ggplot() +
  geom_raster(data = mld_df, aes(x = x, y = y, fill = mld)) +
  coord_sf() +
  scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") +
  theme_void()
  
  ggsave(sprintf("frames/frame_%03d.png", i), p, width = 8, height = 4, dpi = 150)
  
}


# Read all frames and join into a GIF
frames <- list.files("frames", full.names = TRUE, pattern = "*.png")
img_list <- lapply(frames, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 10)
image_write(img_animated, "mld_animation.gif")
library(tidyterra)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sv")  # returns SpatVector directly
world_360 <- shift(world, dx = 360)  # shift to 0-360

dir<-'data/ime_layers_messie_2022/messiem-toolbox_IME_detection-056f04b/inputs/Chl_climatology.mat'

mat <- hdf5r::H5File$new(dir, mode='r')

f <- mat[["Chl"]]
f$ls(recursive = TRUE)

# Once you know the names, read like:
chl_data <- f[["chl"]]$read()
lon      <- f[["lon"]]$read()
lat      <- f[["lat"]]$read()
month      <- f[["month"]]$read()

# Check dimensions
dim(chl_data) # 1440 x 4080 x 12

month_names <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

rast_list <- lapply(1:12, function(m) {
  mat <- chl_data[, , m]
  r <- rast(mat,
            extent = ext(min(lon), max(lon), min(lat), max(lat)),
            crs = "EPSG:4326")
  r <- flip(r, direction = "vertical")
  names(r) <- month_names[m]
  r
})

chl_stack <- rast(rast_list)
plot(chl_stack[[1]])
plot(world_360, add = TRUE, col = "grey80", border = "grey40", lwd = 0.5)
plot(world, add = TRUE, col = "grey80", border = "grey40", lwd = 0.5)

# get bbox for island groups
buffer=0.5
bbox_df <- island %>%
  group_by(region) %>%
  summarise(
    lon_min = min(longitude) - buffer,
    lon_max = max(longitude) + buffer,
    lat_min = min(latitude) - buffer,
    lat_max = max(latitude) + buffer
  ) %>% mutate(lon_min = ifelse(lon_min < 0, lon_min + 360, lon_min ),
               lon_max = ifelse(lon_max < 0, lon_max + 360, lon_max ))


plots <- lapply(1:nrow(bbox_df), function(i) {
  
  e <- ext(bbox_df$lon_min[i], bbox_df$lon_max[i],
           bbox_df$lat_min[i], bbox_df$lat_max[i])
  
  r_crop <- crop(chl_stack[[1]], e)
  
  ggplot() +
    geom_spatraster(data = r_crop) +
    geom_spatvector(data = crop(world_360, e), fill = "grey70", colour = "grey40", linewidth = 0.3) +
    scale_fill_viridis_c(limits = c(0, 0.5), na.value = NA, name = "Chl-a") +
    coord_sf(xlim = c(bbox_df$lon_min[i], bbox_df$lon_max[i]),
             ylim = c(bbox_df$lat_min[i], bbox_df$lat_max[i])) +
    labs(title = bbox_df$group[i]) +
    theme_minimal()
})

wrap_plots(plots, ncol = 3) +
  plot_layout(guides = "collect")


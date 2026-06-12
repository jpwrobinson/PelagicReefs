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
# get summary of island lat-lon
island %>%
  group_by(region) %>%
  summarise(
    lon_min = min(longitude) - buffer,
    lon_max = max(longitude) + buffer,
    lat_min = min(latitude) - buffer,
    lat_max = max(latitude) + buffer
  ) %>% mutate(lon_min = ifelse(lon_min < 0, lon_min + 360, lon_min ),
               lon_max = ifelse(lon_max < 0, lon_max + 360, lon_max ))

# manual save of bbox
bbox_df<-data.frame(region = c('Main Hawaiian', 'Northwest Hawaiian', 'Wake',
                                'Marianas', 'American Samoa', 'Jarvis','Palmyra-Kingman', 'Baker'),
                     lon_min = c(195, 181, 165, 142, 186, 199.5, 197, 183),
                     lon_max = c(207, 200, 168, 148, 190, 200.5,198.5, 185),
                     lat_min = c(18, 22, 19, 12, -15, -.65, 5.5, -.5),
                     lat_max = c(25, 29, 20, 21, -10, 0, 7, .5))

plots <- lapply(1:nrow(bbox_df), function(i) {
  
  e <- ext(bbox_df$lon_min[i], bbox_df$lon_max[i],
           bbox_df$lat_min[i], bbox_df$lat_max[i])
  
  r_crop <- crop(chl_stack, e)
  chl_range <- range(values(r_crop), na.rm = TRUE)
  
  month_plots <- lapply(1:12, function(m) {
    
  p<-ggplot() +
    geom_spatraster(data = r_crop[[m]]) +
    geom_spatvector(data = crop(world_360, e), fill = "grey70", colour = "grey40", linewidth = 0.3) +
    geom_spatvector(data = crop(world, e), fill = "grey70", colour = "grey40", linewidth = 0.3) +
    scale_fill_gradientn(colours = chl_grad_cols,
                         na.value = NA, name = "Chl (mg m-3)") +
    coord_sf(xlim = c(bbox_df$lon_min[i], bbox_df$lon_max[i]),
             ylim = c(bbox_df$lat_min[i], bbox_df$lat_max[i])) +
    labs(subtitle = month_names[m]) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.border = element_rect(colour='black'),
          axis.text = element_text(size=6, colour='black'),
          plot.subtitle = element_text(size=9))
  
  if (m != 12) p <- p + theme(legend.position = "none")
  p
  })
  
  wrap_plots(month_plots, ncol = 4) +
    plot_layout(guides = "collect") +
    plot_annotation(title = bbox_df$region[i])

})

pdf(file = 'fig/Figure_IME_maps.pdf', height=7, width=12)
for(i in 1:length(unique(bbox_df$region))){
print(plots[[i]])
}
dev.off()


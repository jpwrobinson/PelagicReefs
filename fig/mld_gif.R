library(sf)
library(magick)

# get reef lat lon and island shapes
source('00_islands.R')
isl<-readRDS('data/noaa_island_shp.rds') # shapefiles
latlon<-data.frame(lon = island$longitude, lat = island$latitude, island_group=island$island_group)
# align projection systems - longitude wrap
latlon$lon<-ifelse(latlon$lon < 0, 360+latlon$lon, latlon$lon)

# read mld, extract time
mld<-terra::rast('data/glorys/cmems_mod_glo_phy_my_0.083deg_P1M-m_mlotst_118.33E-289.42E_49.33S-56.58N_1993-01-01-2021-06-01.nc')
times<-terra::time(mld)

# plot tester
mhi_extent <- terra::ext(min(latlon$lon)-5, max(latlon$lon)+5, min(latlon$lat)-5, max(latlon$lat)+5)
mld_mhi <- terra::crop(mld, mhi_extent)
mld_df <- as.data.frame(mld_mhi, xy = TRUE, na.rm = TRUE)
names(mld_df)[3] <- "mld"

months <- format(times, "%m")
years <- format(times, "%Y")

# Group layers by month/year and average
monthly_mean <- terra::tapp(mld_mhi, months, mean, na.rm = TRUE)
names(monthly_mean) <- month.abb
monthly_mean_df<-as.data.frame(monthly_mean, xy = TRUE, na.rm = TRUE)

annual_mean <- terra::tapp(mld_mhi, years, mean, na.rm = TRUE)
names(annual_mean) <- unique(years)
annual_mean_df<-as.data.frame(annual_mean, xy = TRUE, na.rm = TRUE)

## This creates images for each month - year.
giffer<-function(raster, time_step){
  
  for(i in 3:dim(raster)[2]){
    raster$mld<-raster[,i]
    raster$mld<-ifelse(raster$mld > 100, 100, raster$mld)
    
    if(time_step=='yr-month'){time<-format(times[i-2], "%b %Y")} else {
      time<-colnames(raster)[i]
    }
    
    p<-ggplot() +
      geom_raster(data = raster, aes(x = x, y = y, fill = mld)) +
      coord_sf() +
      annotate('text',  142, 30.5, label = time, hjust=0, size=7, col = 'white', fontface=2) +
      scale_fill_distiller(palette = "YlGnBu", name = '',
                           limits=c(0, 100), breaks=c(0, 25, 50, 75, 100), 
                           labels=c('0 m', '25 m', '50 m', '75 m', '100+ m'), direction = 1, na.value = "grey80") +
      theme_void() +
      theme(legend.text = element_text(color='white'),
            panel.background = element_rect(fill = "black"),
            plot.background  = element_rect(fill = "black"),
            panel.grid       = element_blank())
    
    ggsave(sprintf(paste0("frames/", time_step, "/frame_%03d.png"), i), p, width = 8, height = 4, dpi = 150)
    
  }}

giffer(raster = monthly_mean_df, time_step = 'month')
giffer(raster = annual_mean_df, time_step = 'year')
giffer(raster = mld_df, time_step = 'month-year')

# Read all frames and join into a GIF

# Month-year
frames <- list.files("frames", full.names = TRUE, pattern = "*.png")
img_list <- lapply(frames, image_read)
img_joined <- image_join(img_list)
img_animated <- image_morph(img_joined, frames = 1) %>% image_animate(fps = 20)
image_write(img_animated, "gif/mld_animation.gif")

# Month-year recent
frames <- list.files("frames", full.names = TRUE, pattern = "*.png")
img_list <- lapply(frames[200:340], image_read)
img_joined <- image_join(img_list)
img_animated <- image_morph(img_joined, frames = 1) %>% image_animate(fps = 20)
image_write(img_animated, "gif/mld_animation_recent.gif")

# Month
frames <- list.files("frames/month", full.names = TRUE, pattern = "*.png")
img_list <- lapply(frames, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 5)
image_write(img_animated, "gif/mld_month_animation.gif")

# Year
frames <- list.files("frames/year", full.names = TRUE, pattern = "*.png")
img_list <- lapply(frames, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 5)
image_write(img_animated, "gif/mld_year_animation.gif")
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
cropper2<-terra::extract(mld_mhi$mlotst_1, terra::vect(isl), touches=TRUE, cells=TRUE, df = TRUE)

regions_df <- as.data.frame(isl)  # should include region_name
regions_df$ID <- 1:nrow(regions_df)        # match the ID assigned by extract

# Merge to get region_name for each raster cell
cell_region <- merge(cropper2, regions_df, by = "ID")

## attac region to cropper_df
cropper_df<-as.data.frame(cropper, xy=TRUE, cells=TRUE) %>% 
  left_join(cell_region %>% select(cell, REGION, island), by = 'cell')

ggplot(cropper_df) + geom_tile( aes(x=x, y = y, fill=mlotst_1)) +
  # geom_sf(data =isl) +
  facet_wrap(~REGION, scales='free') +
  # lims(x = c(201, 206), y = c(18.5,22.5)) +
  scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") 

## value output version that we can assign to each island
vals <- terra::extract(mld_mhi$mlotst_1, isl, touches = TRUE, bind=TRUE, 
                       fun = function(x) {
    c(mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    n_cells = n_distinct(x[!is.na(x)]),
    na_cells = n_distinct(x[is.na(x)]))
}) %>% as.data.frame() %>% 
  select(island, REGION, mlotst_1:mlotst_1.4)

names(vals)[c(3:7)]<-c('mean', 'sd', 'median', 'n_cells', 'na_cells')

vals %>%filter(!is.na(mean)) %>% 
  as.data.frame() %>% 
  write.csv('mld_glory_islands.csv', row.names=FALSE)

vals %>% arrange(-n_cells)

## Missing Lisianski. Ofu&Olosega only has 1 cell?
## Generate a PDF with each page = 1 island with MLD cells
isls<-unique(island$island)

pdf(file = 'fig/mld_island_cells.pdf', height=5, width=9)

for(i in 1:length(isls)){
  
  focal<-cropper_df %>% filter(island==isls[i])
  
  if(dim(focal)[1]==0){print(paste0('Missing cells for:', isls[i]))}
  
  p<-ggplot(focal) + geom_tile( aes(x=x, y = y, fill=mlotst_1)) +
    geom_sf(data =isl %>% filter(island==isls[i])) +
    labs(subtitle = isls[i])
    scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") 

  print(p)
}  

dev.off()

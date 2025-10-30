library(sf)

# get reef lat lon and island shapes
source('00_islands.R')
isl<-readRDS('data/noaa_island_shp.rds') # shapefiles
latlon<-data.frame(lon = island$longitude, lat = island$latitude, 
                   REGION = island$region,
                   island_group=island$island_group, island=island$island)
# align projection systems - longitude wrap
latlon$lon<-ifelse(latlon$lon < 0, 360+latlon$lon, latlon$lon)

# read mld, extract time
# Global Ocean Physics Reanalysis
# GLORYS12V1 product is the CMEMS global ocean eddy-resolving (1/12° horizontal resolution, 50 vertical levels) reanalysis 
mld<-terra::rast('data/glorys/cmems_mod_glo_phy_my_0.083deg_P1M-m_mlotst_118.33E-289.42E_49.33S-56.58N_1993-01-01-2021-06-01.nc')
times<-terra::time(mld)

# align geometries (wrap sf object latitude)
isl <- isl %>% st_transform(terra::crs(mld)) %>% 
  st_shift_longitude()

# plot tester
crep_extent <- terra::ext(min(latlon$lon)-5, max(latlon$lon)+5, min(latlon$lat)-5, max(latlon$lat)+5)
mld_crep <- terra::crop(mld, crep_extent)

mld_df <- as.data.frame(mld_crep, xy = TRUE, na.rm = TRUE)
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
# project for buffer and metres
mld_crep <- terra::project(mld_crep, "EPSG:3857")      # Project to web Mercator (meters)
isl_mercator <- isl %>% st_transform(terra::crs(mld_crep))

# set buffer around islands
buf <- st_buffer(isl_mercator, dist = 16000)  # 8km (ie the size of a pixel) (but either side of the polygon so double it)
    
# test buffer
ggplot(isl_mercator %>% filter(island=='Baker')) +
  geom_sf(fill='black') +
  geom_sf(data = buf %>% filter(island=='Baker'), alpha=0.5) 
  
## raster output version that we can plot
cropper<-terra::mask(mld_crep$mlotst_1,terra::vect(buf))
cropper2<-terra::extract(mld_crep$mlotst_1, terra::vect(buf), touches=TRUE, cells=TRUE, df = TRUE)

regions_df <- as.data.frame(isl)  # should include region_name
regions_df$ID <- 1:nrow(regions_df)        # match the ID assigned by extract

# Merge to get region_name for each raster cell
cell_region <- merge(cropper2, regions_df, by = "ID")

## attac region to cropper_df
cropper_df<-as.data.frame(cropper, xy=TRUE, cells=TRUE) %>% 
  left_join(cell_region %>% select(cell, REGION, island), by = 'cell')

focal<-cropper_df %>% filter(island=='Howland')
res<-terra::res(mld_crep)*2

ggplot(cropper_df) + geom_tile( aes(x=x, y = y, fill=mlotst_1), col='white') +
  geom_sf(data = buf, color = "black", alpha=0.5) +
  geom_sf(data = isl, color = "black") +
  coord_sf(crs = st_crs(cropper),xlim = c(min(focal$x) - res[1],max(focal$x) + res[1]), ylim = c(min(focal$y) - res[1],max(focal$y) + res[2])) +
  scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80")

## value output version that we can assign to each island
## this function loops through each layer, or takes the first layer
mld_extract<-function(raster, buffer, latlon, test=TRUE){
  
  output<-numeric()
  nvar<-names(raster)
  
  # align coords for atoll extract (point-based)
  raster84 <- terra::project(raster, "EPSG:4326")      # Project to WGS 84
  ll<-cbind(lon = latlon$lon180, lat = latlon$lat)
  
  if(test==TRUE){nvar = 1}
  
  for(i in 1:length(nvar)){
    
    raster$mld<-raster[[i]]
    
    # extract MLD from island buffers
    vals <- terra::extract(raster$mld, buffer, touches = TRUE, bind=TRUE, 
                           fun = function(x) {
        c(mean = mean(x, na.rm = TRUE),
        sd   = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        n_cells = n_distinct(x[!is.na(x)]),
        na_cells = n_distinct(x[is.na(x)]))
    }) %>% as.data.frame() %>% 
      filter(!(island == 'Ofu & Olosega' & mld.3 == 4)) %>%  # drop one of Ofu/Olosega as these are captured as one island (take max cells)
      distinct(island, REGION, mld,mld.1, mld.2, mld.3, mld.4) %>% 
      mutate(time = times[i])
    
    # now get atolls
    if(!is.null(latlon)){
      
      raster84$mld<-raster84[[i]]
      vals2 <- terra::extract(raster84$mld, ll) %>% 
        as.data.frame() %>% 
        mutate(island = latlon$island, REGION = latlon$REGION, mld.1=NA, mld.2=NA, mld.3=NA, mld.4=NA, time = times[i])
    
      vals<-rbind(vals, vals2 %>% select(names(vals)))
    }
    
    
    output<-rbind(output, vals)  
    print(paste0('Completed ', nvar[i]))
  }
  
  names(output)[c(3:7)]<-c('mean', 'sd', 'median', 'n_cells', 'na_cells')
  return(output)
  }

atolls<-latlon %>% filter(!island %in% isl$island)
# unwrap longitude
atolls$lon180<-with(atolls, ifelse(lon > 180, lon - 360, lon))


vals<-mld_extract(raster=mld_crep, buffer=buf, latlon=atolls, test=FALSE)
write.csv(vals, file = 'data/glorys/mld_1993-2021_glory_island.csv')

## Generate a PDF with each page = 1 island with MLD cells
isls<-unique(vals$island)
vals_test<-mld_extract(raster=mld_crep, buffer=buf, latlon = atolls, test = TRUE)
vals_test %>% arrange(-n_cells)

pdf(file = 'fig/mld_island_cells.pdf', height=5, width=9)

for(i in 1:length(isls)){
  
  focal<-cropper_df %>% filter(island==isls[i])
  
  if(dim(focal)[1]==0){print(paste0('Missing cells for:', isls[i]))}
  
  p<-ggplot(focal) + geom_tile(aes(x=x, y = y, fill=mlotst_1)) +
    geom_sf(data =isl_mercator %>% filter(island==isls[i])) +
    geom_sf(data = buf %>% filter(island==isls[i]), color = "black", alpha=0.5) +
    geom_sf(data = isl %>% filter(island==isls[i]), color = "black") +
    annotate('text', -Inf, Inf, label=paste0('N cells: ', vals_test$n_cells[vals_test$island==isls[i]]), vjust=0, hjust=1) +
    labs(subtitle = isls[i]) +
    scale_fill_viridis_c(name = "Mixed Layer Depth (m)", direction = -1, na.value = "grey80") 

  print(p)
}  

dev.off()

## Compare MLD with Dani's version
mld1<-read.csv('data/crep_oceanographic/MLD_All_Islands-lrg_island_means.csv') %>% 
  clean_names() %>% 
  mutate(date = as.Date(date), year = year(date), month = month(date), 
         time = as.numeric(date))

mld2<-read.csv('data/glorys/mld_1993-2021_glory_island.csv') %>% 
  mutate(mld = mean, date = as.Date(time), year = year(date), month = month(date), 
                time = as.numeric(date))

mld2<-mld2 %>% left_join(mld1 %>% mutate(mld_org = mld) %>% select(island, date, mld_org))

pdf(file = 'fig/mld_pixel_compare.pdf', height = 7 , width = 11)
ggplot(mld2 %>% filter(!is.na(mld_org)), aes(mld_org, mld)) + geom_point(size=.5) + facet_wrap(~island, scales='free')
dev.off()

mld2 %>% filter(!is.na(mld_org)) %>%  group_by(island) %>% summarise(cor(mld_org, mld)) %>% data.frame

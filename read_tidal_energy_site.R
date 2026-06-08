## Spatial data on tidal conversion
# library(terra)
# library(tidyverse)
# 
# file.list<-list.files('data/tidal/tidal_DAT')
# file.list<-file.list[str_detect(file.list, '.dat')]
# 
# pdf(file = 'data/tidal/tidal_DAT/tidal_maps.pdf', height=7, width=12)
# 
# for(i in 1:length(file.list)){
# 
#   print(paste('Starting', file.list[i]))
#   tidal_data <- read.table(paste0("data/tidal/tidal_DAT/",file.list[i]),  header = FALSE)
# 
#   # Extract coordinates
#   lon <- as.numeric(tidal_data[1, -1])  # First row, skip the NaN
#   lat <- as.numeric(tidal_data[-1, 1])  # First column, skip the NaN
# 
#   # Extract data matrix (skip first row and column) and flip for latitude
#   tidal_matrix <- as.matrix(tidal_data[-1, -1])
#   tidal_matrix <- tidal_matrix[nrow(tidal_matrix):1, ]
# 
# 
#   lon180 <- ifelse(lon > 180, lon - 360, lon)
# 
#   # Create raster
#   # terra expects matrix in [row, col] = [lat, lon] order
#   tidal_rast <- rast(tidal_matrix,
#                      extent = ext(min(lon180), max(lon180), min(lat), max(lat)),
#                      crs = "EPSG:4326")
# 
#   # plot to pdf
#   plot(log(tidal_rast), main = file.list[i])
# 
#   assign(str_replace_all(file.list[i], '.dat', ''), tidal_rast)
# 
# }
# 
# dev.off()
# 
# tidal_pacific <- merge(Hawaii_1, Hawaii_2, Kiribati, Palmyra, Mariana)
# 
# save(tidal_pacific, Hawaii_1, Hawaii_2, Kiribati, Palmyra, Mariana, file = 'data/tidal/tidal_DAT/tidal_rasters.Rdata')
# 
# load('data/tidal/tidal_DAT/tidal_rasters.Rdata')
# crep<-read.csv('data/noaa-crep/crep_full_merged.csv') %>%
#   distinct(ISLAND, lat, lon, SITEVISITID)
# 
# # extract tidal from raster using bilinear interpolation (4 nearest cells)
# crep<-crep %>% mutate(tidal_value = extract(tidal_pacific, cbind(lon, lat), method = 'bilinear')[, 1])
# 
# ## this is missing entire islands not in the rasters, and some sites that may fall on land or NA cells
# nana<-crep %>% group_by(ISLAND) %>%
#   summarise(na = length(which(is.na(tidal_value))), n = length(SITEVISITID), prop_na = na/n) %>% data.frame
# 
# nana %>% filter(prop_na == 1) %>% write.csv('data/tidal/tidal_DAT/tidal_missing_island.csv', row.names=FALSE)
# 
# # Check a specific NA site
# problem_site <- crep %>% filter(ISLAND=='Hawaii')
# 
# # Plot raster with that site
# plot(Hawaii_1)
# points(problem_site$lon, problem_site$lat, col = "red", pch = 19, cex = 0.5)
# 
# # Check cell value directly
# cellFromXY(Hawaii_2, cbind(problem_site$lon, problem_site$lat))
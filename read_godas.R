library(sf)
library(terra)


# https://psl.noaa.gov/data/gridded/data.godas.html
# Behringer, D.W., M. Ji, and A. Leetmaa, 1998: An improved coupled model for ENSO prediction and implications for ocean initialization. Part I: The ocean data assimilation system. Mon. Wea. Rev., 126, 1013-1021.

# Long-term monthly means for SSH relative to geoid for 1991-2020
# 0.333 degree latitude x 1.0 degree longitude global grid (418x360)
# 74.5S - 64.5N, 0.5E - 359.5E

ssh<-rast('data/godas/sshg.mon.ltm.1991-2020.nc')

plot(ssh) # this is a layer for each month
names(ssh)
crs(ssh)

# work with January to start
jan<-ssh$sshg_1
plot(jan)

# overlay reefs
source('00_oceanographic_load.R')
latlon<-c(island$longitude, island$latitude)
points(latlon)

terra::extract(jan, latlon) %>% head

# need to align projection systems

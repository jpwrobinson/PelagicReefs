library(rgee)
library(tidyverse)
library(sf)

# Initialize Earth Engine
ee_Initialize()

# Read your islands
# CSV format: Name,Lat,Lon
islands <- read.csv("ime_crep_lat_lon.csv")

# shapefiles from https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273
# Global Islands database from USGS
# Sayre, R., 2023, Global Islands: U.S. Geological Survey data release, https://doi.org/10.5066/P91ZCSGM. 
# https://gkhub.earthobservations.org/records/5wg96-bvv84?package=7yjze-2g558

# Convert to EE FeatureCollection
fc <- ee$FeatureCollection(lapply(1:nrow(islands), function(i){
  ee$Feature(ee$Geometry$Point(c(islands$lon[i], islands$lat[i])),
             list(name=islands$island[i]))
}))

# Define CHIRPS daily dataset and time range
startDate <- '2000-01-01'
endDate <- '2020-12-31'

chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
  filterDate(startDate, endDate)

# Function to compute monthly mean
months <- 1:12
monthly_totals <- lapply(months, function(m){
  chirps$
    filter(ee$Filter$calendarRange(m, m, "month"))$
    sum()$
    reduceRegions(
      collection = fc,
      reducer = ee$Reducer$mean(),
      scale = 5000  # ~5 km resolution
    )$
    map(function(f){ f$set("month", m) })
})

# Flatten list to single FeatureCollection
monthly_fc <- ee$FeatureCollection(monthly_totals)$flatten()

# Get as R dataframe
monthly_df <- ee_as_sf(monthly_fc) %>%
  st_drop_geometry() %>%
  rename(precip_mm = mean, island = name)

head(monthly_df)

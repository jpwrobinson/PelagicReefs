library(rgee)
# Initialize Earth Engine
ee_Initialize(drive = TRUE,gcs = FALSE)


library(tidyverse)
library(sf)
library(purrr)



# Read your islands
# CSV format: Name,Lat,Lon
islands_dat <- read.csv("ime_island_crep_lat_lon.csv")

# shapefiles from https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273
# Global Islands database from USGS
# Sayre, R., 2023, Global Islands: U.S. Geological Survey data release, https://doi.org/10.5066/P91ZCSGM. 
# https://gkhub.earthobservations.org/records/5wg96-bvv84?package=7yjze-2g558

# islands_big <- st_read("spatial/usgs_islands/usgs_globalislandsv2_bigislands.shp")
# islands_small <- st_read("spatial/usgs_islands/usgs_globalislandsv2_smallislands.shp")
# islands_verysmall <- st_read("spatial/usgs_islands/usgs_globalislandsv2_verysmallislands.shp")
# 
# islands <- bind_rows(islands_big, islands_small, islands_verysmall)
# st_write(islands, "spatial/usgs_islands/merged/islands.shp")

islands<-st_read("spatial/usgs_islands/merged/islands.shp") 
 
lister<-islands_dat$island[islands_dat$geomorphic_type=='Island']

# filter islands. doing this manually to catch all islands. atolls are missed and excluded.
islands2<-islands %>%  filter(Name_USGSO %in% lister) %>% mutate(island = Name_USGSO)
islands3<-islands %>%  filter(NAME_wcmcI %in% c('Howland', 'Alamagan', 'Swains', 'Tau', 'Laysan', 'Nihoa')) %>% mutate(island = NAME_wcmcI)
# Maug - multiple islands but drop those caught from Belize
maug<-islands %>% filter(str_detect(NAME_wcmcI, 'Maug')) %>% filter(!str_detect(NAME_wcmcI, 'Mauger')) %>% mutate(island = 'Maug')
# Necker in NWHI
necker<-islands %>% filter(str_detect(NAME_wcmcI, 'Mokumanamana')) %>% mutate(island = 'Necker')
ofu<-islands %>% filter(NAME_wcmcI %in% c('Nuusilaelae Island', 'Olosega')) %>% mutate(island = 'Ofu & Olosega')

# Nuusilaelae Island is misnamed in island database. Confirmed with map plots that it is Ofu.
# islands %>% filter(str_detect(NAME_wcmcI, 'Manua'))

# bind. n = 33
isl_dat<-bind_rows(islands2, islands3, maug, necker, ofu) %>% 
  left_join(islands_dat) 

# all atolls missing
islands_dat %>% filter(!island %in% isl_dat$island)

# Convert to EE FeatureCollection
islands_ee <- sf_as_ee(isl_dat %>% mutate(lat = latitude, lon = longitude) %>% select(geometry, island, lat, lon))

# -------------------------
# 3. Load CHIRPS daily data
# -------------------------
chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY") %>%
  ee$ImageCollection$filterDate('2000-01-01', '2020-12-31') %>% 
  ee$ImageCollection$filterBounds(islands_ee)

# -------------------------
# 4. Compute average total monthly precipitation per month
# -------------------------
start_year <- 2000
end_year   <- 2020

# ---- Safe monthly-total (sum of daily) image maker
monthly_total_img <- ee_utils_pyfunc(function(ym) {
  d <- ee$Dictionary(ym)
  y <- ee$Number(d$get('y'))
  m <- ee$Number(d$get('m'))
  
  start <- ee$Date$fromYMD(y, m, 1)
  end   <- start$advance(1, 'month')
  
  # Filter CHIRPS for this month and sum daily precipitation
  img <- chirps$filterDate(start, end)$select('precipitation')$sum()
  
  # Set month/year properties
  img$set('year', y)$set('month', m)
})


# ---- Build server-side list of (year,month)
ym_list <- ee$List$sequence(start_year, end_year)$map(
  ee_utils_pyfunc(function(y)
    ee$List$sequence(1,12)$map(
      ee_utils_pyfunc(function(m) ee$Dictionary(list(y=y, m=m)))
    )
  )
)$flatten()

monthly_ic <- ee$ImageCollection$fromImages(ym_list$map(monthly_total_img))

# ---- Zonal stats per island: mean across pixels (mm/month per island)
# NOTE: if you truly want area-scaled totals, change Reducer$mean() → Reducer$sum()
island_monthly_fc <- monthly_ic$map(
  ee_utils_pyfunc(function(img) {
    
    # Reduce over islands to get mean and pixel count
    reduced <- img$reduceRegions(
      collection = islands_ee,
      reducer = ee$Reducer$mean()$combine(
        reducer2 = ee$Reducer$count(),
        sharedInputs = TRUE
      ),
      scale = 5566,   # CHIRPS resolution ~5.56 km
      tileScale = 2
    )
    
    # Add year and month properties to each island
    reduced$map(
      ee_utils_pyfunc(function(f) {
        f$set('year', img$get('year'))$
          set('month', img$get('month'))
      })
    )
    
  })
)$flatten()

# Export to Google Drive
task <- ee_table_to_drive(
  collection = island_monthly_fc,
  description = 'island_monthly_precip',
  folder = 'GEE_exports',
  fileFormat = 'CSV'
)

task$start()
ee_monitoring()

# data copied from Drive into repo
island_monthly_df <- read.csv('data/gee-exports/island_monthly_precip_2025_08_21_16_57_24.csv') %>% 
  select(count, island, lat, lon, mean, month, year) %>% 
  mutate(
    year  = as.integer(year),
    month = as.integer(month),
    monthly_total_mm = mean,   # mean of monthly totals across pixels
    n_pixel = count   # number of pixels
  ) %>%
  select(-count, -mean) %>% 
  arrange(island, year, month)


# local side version
# island_monthly_df <- ee_as_sf(island_monthly_fc) %>% 
#   transmute(
#     year  = as.integer(year),
#     month = as.integer(month),
#     monthly_total_mm = mean,   # mean of monthly totals across pixels
#     n_pixel = count   # number of pixels
#   ) %>%
#   arrange(island, year, month)

# ---- Monthly climatology (avg across years) 
clim_monthly <- island_monthly_df %>%
  filter(!n_pixel == 0) %>% 
  group_by(island, month, n_pixel) %>%
  summarize(avg_monthly_mm = mean(monthly_total_mm, na.rm = TRUE)) %>%
  mutate(month_label = factor(month, levels = 1:12, labels = month.abb)) %>%
  select(island, n_pixel, month, month_label, avg_monthly_mm)

# Baker has NA precipitation because they do not overlap with a pixel (ie islands too small / land area not given pixel)
write.csv(clim_monthly, file = 'data/gee-exports/crep_monthly_precipitation_mm.csv', row.names=FALSE)

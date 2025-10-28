library(rgee)
# Initialize Earth Engine
ee_Initialize(drive = TRUE,gcs = FALSE)


library(tidyverse)
library(sf)
library(purrr)


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
island_monthly_df <- read.csv('data/gee-exports/island_monthly_precip_2025_08_22_12_30_23.csv') %>% 
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

# ---- Monthly climatology (avg across years). n = 26 islands.
clim_monthly <- island_monthly_df %>%
  filter(!n_pixel == 0) %>% # This drops Baker, Laysan, Maug, Necker, Nihoa
  group_by(island, month, n_pixel) %>%
  summarize(avg_monthly_mm = mean(monthly_total_mm, na.rm = TRUE)) %>%
  mutate(month_label = factor(month, levels = 1:12, labels = month.abb)) %>%
  select(island, n_pixel, month, month_label, avg_monthly_mm)

# Baker has NA precipitation because they do not overlap with a pixel (ie islands too small / land area not given pixel)
write.csv(clim_monthly, file = 'data/gee-exports/crep_monthly_precipitation_mm.csv', row.names=FALSE)

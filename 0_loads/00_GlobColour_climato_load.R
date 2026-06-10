
library(sf)
library(terra)
library(tidyverse)
library(ncdf4)
source('0_loads/00_islands.R')

# open the file
nc <- nc_open('data/GlobColour/IME_database_GlobColour_2026algorithm.nc')
ncM <- nc_open('data/GlobColour/IME_database_MODIS_2026algorithm.nc')

islands<-read.csv('data/ime_layers_messie_2022/island_database.csv', skip = 6) %>% 
  janitor::clean_names()

# list all dimensions
nc$dim

var_names <- names(nc$var)

# print long names for each variable
for (v in var_names) {
  longname <- ncatt_get(nc, v, "long_name")$value
  cat(v, ":", longname, "\n")
}

# island_latitude : Island position (latitude) 
# island_longitude : Island position (longitude) 
# is_emerged : Island type (0 = submerged reef, 1 = emerged island) 
# month : Climatological month 
# cChl : Chl contour delineating the IME region 
# Chl_max : Maximum Chl nearby island 
# Chl_IME : Chl averaged within IME region 
# Chl_REF : Chl averaged within REF region 
# Chl_increase_nearby : Proportion of Chl increase next to the island (Chl_max) relative to Chl_REF 
# area_IME : IME area 
# has_IME : 1 if belonging to an IME, 0 otherwise, NaN if too many clouds next to the island (no seasonal instance) 
# is_primaryIME :  
#   keep_IME : NaN if no IME of if belonging to a shared IME (lead by another island), 1 otherwise 


# 1️⃣ extract dimensions
island_names <- islands$island_name # ~664 islands loaded from NatGeosci dataset
lon <- ncvar_get(nc, "island_longitude")
lat <- ncvar_get(nc, "island_latitude")
month <- ncvar_get(nc, "month")           # raw numeric

var_names <- names(nc$var)[-c(1:4)]

# 3️⃣ loop over variables and build long table
df_list_gc <- lapply(var_names, function(var){
  
  # extract variable: dimensions = island x time
  df <- ncvar_get(nc, var) %>% as.data.frame()
  
  # convert to data.frame
  colnames(df) <- as.character(month)        # column names = actual dates
  df$island <- island_names                        # add island column
  df$lon <- lon                        # add island column
  df$lat <- lat                        # add island column
  
  # pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = -c(island,lon,lat),
      names_to = "month",
      values_to = "value"
    ) %>%
    mutate(
      month_name = month.abb[month],
      variable = var
    ) %>%
    select(island, month, month_name, variable, value)
  
  return(df_long)
})

# repeat for MODIS
df_list_modis <- lapply(var_names, function(var){
  
  # extract variable: dimensions = island x time
  df <- ncvar_get(ncM, var) %>% as.data.frame()
  
  # convert to data.frame
  colnames(df) <- as.character(month)        # column names = actual dates
  df$island <- island_names                        # add island column
  df$lon <- lon                        # add island column
  df$lat <- lat                        # add island column
  
  # pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = -c(island,lon,lat),
      names_to = "month",
      values_to = "value"
    ) %>%
    mutate(
      month_name = month.abb[month],
      variable = var
    ) %>%
    select(island, month, month_name, variable, value)
  
  return(df_long)
})

# 4️⃣ combine all variables into one tidy data.frame
ime_df_gc <- bind_rows(df_list_gc) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider( id_cols = c(island, month), names_from = variable, values_from=value) %>% 
  mutate(data = 'GlobColour')

ime_df_modis <- bind_rows(df_list_modis) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider( id_cols = c(island, month), names_from = variable, values_from=value) %>% 
  mutate(data = 'MODIS')

ime_df<-rbind(ime_df_gc, ime_df_modis) %>% 
  mutate(
    island_messie = island,
    island = trimws(str_replace_all(island, 'Atoll', '')),
    island = trimws(str_replace_all(island, 'Island', '')),
    island = trimws(str_replace_all(island, 'Reef', '')),
    island = trimws(str_replace_all(island, '\\ and', '\\ &')),
    island = case_match(island, 
                        'Hawai’i' ~ 'Hawaii',
                        'French Frigate Shoals' ~ 'French Frigate',
                        'Kaua’i' ~ 'Kauai',
                        'Ni’ihau' ~ 'Niihau',
                        'O’ahu' ~ 'Oahu',
                        'Swains  (Olohega)' ~ 'Swains',
                        'Ta’u' ~ 'Tau', .default = island)) %>% 
  left_join(region_df, by = 'island') %>% 
  left_join(island_cols)

write.csv(ime_df, file = 'data/GlobColour/IME_climato_GlobColour_MODIS.csv', row.names=FALSE)

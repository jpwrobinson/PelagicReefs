
library(sf)
library(terra)
library(tidyverse)
library(ncdf4)
source('0_loads/00_islands.R')

# open the file
nc <- nc_open('data/GlobColour/IME_database_GlobColour_monthly_NOAAislands.nc')

# list all dimensions
nc$dim

var_names <- names(nc$var)

# print long names for each variable
for (v in var_names) {
  longname <- ncatt_get(nc, v, "long_name")$value
  cat(v, ":", longname, "\n")
}

# island_number : Island number (Messié et al., 2022 database) 
# island_name : Island name (Messié et al., 2022 database) 
# island_latitude : Island position (latitude) 
# island_longitude : Island position (longitude) 
# is_emerged : Island type (0 = submerged reef, 1 = emerged island) 
# cChl : Chl contour delineating the IME region 
# Chl_max : Chl 99th percentile next to the island 
# Chl_IME : Chl averaged within IME region 
# Chl_REF : Chl averaged within REF region 
# Chl_increase_nearby : Proportion of Chl increase next to the island (strength_IME) relative to Chl_REF 
# strength_IME : IME strength, defined as Chl increase next to the island (Chl_max - Chl_REF) 
# area_IME : IME area 
# has_IME : 1 if belonging to an IME, 0 otherwise, NaN if too many clouds next to the island (no seasonal instance) 
# keep_IME : NaN if no IME of if belonging to a shared IME (lead by another island), 0 if more than 60% gaps in chl within the IME region (no seasonal instance), 1 otherwise 

# extract time and convert from base (days since 1970)

# 1️⃣ extract dimensions
islands <- ncvar_get(nc, "island_name")             # ~34 islands
lon <- ncvar_get(nc, "island_longitude")             # ~34 islands
lat <- ncvar_get(nc, "island_latitude")             # ~34 islands
time <- ncvar_get(nc, "time")           # raw numeric
nc$dim$time$units
origin <- as.Date('1970-01-01')
dates <- origin + time


# 2️⃣ list all variables you want (all 9 layers)
var_names <- names(nc$var)[-c(1:5)]

# 3️⃣ loop over variables and build long table
df_list <- lapply(var_names, function(var){
  
  # extract variable: dimensions = island x time
  df <- ncvar_get(nc, var) %>% as.data.frame()
  
  # convert to data.frame
  colnames(df) <- as.character(dates)        # column names = actual dates
  df$island <- islands                        # add island column
  df$lon <- lon                        # add island column
  df$lat <- lat                        # add island column
  
  # pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = -c(island,lon,lat),
      names_to = "date",
      values_to = "value"
    ) %>%
    mutate(
      date = as.Date(date),
      variable = var
    ) %>%
    select(island, date, variable, value)
  
  return(df_long)
})

# 4️⃣ combine all variables into one tidy data.frame
tidy_df <- bind_rows(df_list) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider( id_cols = c(island, date), names_from = variable, values_from=value) %>% 
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
  left_join(region_df, by = 'island')

write.csv(tidy_df, file = 'data/GlobColour/GlobColour_IME_output.csv', row.names=FALSE)

pdf(file = 'fig/ime_db/ime_globcol_timeseries.pdf', height=7, width=12)
island.vec<-tidy_df %>% distinct(island) %>% pull(island)
for(i in 1:length(island.vec)){
  print(
    ggplot(tidy_df %>% filter(island == island.vec[i]), aes(date, Chl_max)) + 
    geom_line() + labs(x = '', subtitle = island.vec[i])
  )
}
dev.off()
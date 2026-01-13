source('0_loads/00_ime_dataframe.R')
load(file = 'results/mod_ime.rds')

## IME database
range(dat_month$Chl_increase_nearby, na.rm=TRUE)


## CREP surveys
crep_bathy_fill<-read.csv('data/noaa-crep/crep_full_merged_bathymetry_fill.csv')
with(crep_bathy_fill, range(OBS_YEAR)) # 2009-2024
with(crep_bathy_fill, length(unique(ISLAND))) # 39
with(crep_bathy_fill, length(unique(SITEVISITID))) # 6,769
with(crep_bathy_fill, length(unique(TAXONNAME))) # 693
with(crep_bathy_fill, range(DEPTH, na.rm=TRUE)) # 1.3 - 30

with(crep_bathy_fill %>% 
       filter(!is.na(SITE_SLOPE_400m)), 
     length(unique(SITEVISITID))) # 4,343

6769 - 4343 # excluded from bathy filling

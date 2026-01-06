


crep_bathy_fill<-read.csv('data/noaa-crep/crep_full_merged_bathymetry_fill.csv')
with(crep_bathy_fill, range(OBS_YEAR)) # 2009-2024
with(crep_bathy_fill, length(unique(ISLAND))) # 39
with(crep_bathy_fill, length(unique(SITEVISITID))) # 6,769
with(crep_bathy_fill, length(unique(TAXONNAME))) # 6,769

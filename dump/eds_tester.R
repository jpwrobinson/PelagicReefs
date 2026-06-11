
## Alternative approoach through NOAA EDS package
# https://github.com/krtanaka/eds
# This downloads specific netCDF files from ERDDAP servers and extracts time and locations
library(eds)
eds<-read.csv('data/crep_oceanographic/EDS_parameters.csv')

## select environmental layer from the eds dataset provided by Courtney Couch
## This needs to be on your Desktop for run_eds to use param values
# eds_parameter <- eds %>% filter(Dataset %in% c("Sea_Surface_Height_GODAS_Monthly", "Mixed_Layer_Depth_GODAS_Monthly"))
# write.csv(eds_parameter, file = file.path("/Users/", Sys.info()[7], "Desktop", "eds_parameters.csv"), row.names = FALSE)

time<-as.Date('2000-01-01')

# filter df of NOAA sites to MHI
mhi <- subset(df, region == "MHI")

run_eds(lon = latlon$lon,
        lat = latlon$lat,
        unit = latlon$island,
        time=time)

run_eds(lon = mhi$lon,
        lat = mhi$lat,
        unit = mhi$island,
        time=time)

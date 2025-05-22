library(tidyverse)
remotes::install_github("andrew-edwards/sizeSpectra2")
library(sizeSpectra2)
source('size-spectra/countsFunctions.R')
source('size-spectra/PLBfunctions.R')
source('size-spectra/MLB_binned_function.R')
debug(PLB_binned)

# use PLB_binned function to estimate MLEbin for each site cylinder count
test<-crep_full %>% filter(!is.na(body_mass_g)) %>% mutate(body_mass_g = round(body_mass_g, 0)) %>% 
  group_by(ISLAND, body_mass_g) %>% summarise(MEAN_COUNT = sum(MEAN_COUNT)) %>% data.frame
id.vec<-unique(test$ISLAND)
b.vec<-numeric()

for(i in 1:length(id.vec)){
  # estimate b and CIs for each island
  b<-PLB_binned(counts=test[test$ISLAND==id.vec[i],], integer=FALSE, size.var='body_mass_g', count.var = 'MEAN_COUNT', binWidth=1)
  b$SITEVISITID<-id.vec[i]
  # bind to results
  b.vec<-rbind(b.vec, b)	
}
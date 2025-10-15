library(tidyverse)
crep_full<-read.csv('data/noaa-crep/crep_full_merged.csv')

# remotes::install_github("andrew-edwards/sizeSpectra2")
# library(sizeSpectra2)
# source('size-spectra/countsFunctions.R')
# source('size-spectra/PLBfunctions.R')
# source('size-spectra/MLB_binned_function.R')
# debug(PLB_binned)

# use PLB_binned function to estimate MLEbin for each site cylinder count
test<-crep_full %>% filter(!is.na(body_mass_g)) %>% #mutate(body_mass_g = round(body_mass_g, 0)) %>% 
  group_by(ISLAND, body_mass_g) %>% summarise(MEAN_COUNT = round(sum(MEAN_COUNT), 0)) %>% data.frame
id.vec<-unique(test$ISLAND)
b.vec<-numeric()

for(i in 1:length(id.vec)){
  # estimate b and CIs for each island
  b<-PLB_binned(counts=test[test$ISLAND==id.vec[i],], integer=FALSE, size.var='body_mass_g', count.var = 'MEAN_COUNT', binWidth=1)
  b$SITEVISITID<-id.vec[i]
  # bind to results
  b.vec<-rbind(b.vec, b)	
}


remotes::install_github("andrew-edwards/sizeSpectraFit")
library(sizeSpectraFit)

i=1

spec<-data.frame(b = NA, b_lo = NA, b_hi = NA, island = id.vec)
for(i in 1:length(id.vec)){
  
  # subset to island and convert to vector of sizes
  x <- test[test$ISLAND==id.vec[i],] %>% select(-ISLAND)
  x<-uncount(x, MEAN_COUNT) %>% pull()
  
  # fit size spectrum and save exponent and CI
  res <- fit_size_spectrum(x)
  spec$b[i]<-res$b_mle
  spec$b_lo[i]<-res$b_conf[1]
  spec$b_hi[i]<-res$b_conf[2]
  
  # plot(res)
}
  
ggplot(spec, aes(island, b, ymin = b_lo, ymax = b_hi)) + 
  geom_pointrange()


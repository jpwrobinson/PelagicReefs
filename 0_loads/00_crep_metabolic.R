library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)

source('0_loads/00_oceanographic_load.R')
source('0_loads/00_ime_dataframe.R')

crep<-read.csv(file = 'data/metabolic/crep_site_metabolic_rates.csv') 
biom<-read.csv(file = 'data/metabolic/crep_site_biomass.csv') 


# using full CREP data set with filled bathymetry
depth<-
  read.csv('data/noaa-crep/crep_for_analysis.csv') %>% 
  filter(!is.na(SITE_SLOPE_400m)) %>%  # drop sites with no bathymetry measured or nearby (within 400m)
  mutate(
         site_bathy_400m = SITE_SLOPE_400m,
         island = ISLAND,
         region = REGION,
         # population_status = POP_STATUS,
         year = OBS_YEAR,
         date = as.Date(DATE_, "%m/%d/%Y"),
         month_num = month(date),
         date_ym = floor_date(date, unit = "month")) %>% 
  distinct(depth_m, site_bathy_400m, island, region, hard_coral, reef_zone, #population_status,
           year, date, month_num, date_ym, SITEVISITID) %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  select(region, island, SITEVISITID, year, date_ym, month, month_num, depth_m, site_bathy_400m,
          hard_coral, reef_zone#, population_status
         ) %>% 
  left_join(region_df %>% select(-region), by = 'island')


# use bathymetry/benthic dataset as basis for model. join fish metabolic and then predictors from Gove/Williams
depth<- depth %>% 
  left_join(crep %>% select(SITEVISITID, PLANKTIVORE, PRIMARY), by = 'SITEVISITID') %>% 
  mutate(planktivore_metab=PLANKTIVORE,
         herbivore_metab = PRIMARY) %>%
  select(-PLANKTIVORE, -PRIMARY) %>% 
  left_join(biom %>% select(SITEVISITID, PLANKTIVORE, PRIMARY), by = 'SITEVISITID') %>% 
  mutate(planktivore_biom=PLANKTIVORE,
         herbivore_biom = PRIMARY) %>% 
  select(-PLANKTIVORE, -PRIMARY) %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% select(island, island_code, sst_mean:ted_sum)) %>% 
  mutate(avg_monthly_mm = ifelse(is.na(avg_monthly_mm), 0, avg_monthly_mm),
         precip_amp_mm = ifelse(is.na(precip_amp_mm), 0, precip_amp_mm)) %>% 
  left_join(ime_dat %>% 
              select(island_group, months_ime, median_ime_percent, chl_ime))



# 1. Create planktivore
# drop NA planktivore sites (n = 0)
plank<-depth %>% filter(!is.na(planktivore_metab))
# drop 0 planktivore sites (n = 49)
plank<-plank %>% filter(planktivore_metab>0)
# drop missing hard coral sites (675)
plank<-plank %>% filter(!is.na(hard_coral)) ## need to check with Tom
# keep forereef only (drops 31 sites)
plank<-plank %>% filter(reef_zone=='Forereef') ## need to check with Tom

# scale and center cont. covariates
plank_scaled <- plank %>% 
  mutate(reef_area_km2 = log10(reef_area_km2), 
         island_area_km2 = log10(land_area_km2),
         across(c(depth_m:hard_coral, 
                  sst_mean:irradiance_einsteins_m2_d1_mean, 
                  precip_amp_mm:chl_ime), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

pdf(file = 'fig/ime_crep/crep_planktivore_cov_correlations.pdf', height=7, width=15)
pairs2(
  plank_scaled %>% 
    filter(!is.na(ted_mean) & !is.na(median_ime_percent)) %>% 
    select(island_area_km2, reef_area_km2, site_bathy_400m,depth_m,hard_coral,
           precip_amp_mm, avg_monthly_mm,sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld_mean, mld_amp, ssh, ted_mean, ted_sum, month_num))
dev.off()

# 2. Create herbivore
# drop NA herbivore sites (n = 0)
herb<-depth %>% filter(!is.na(herbivore_metab))
# drop 0 herbivore sites (n = 3)
herb<-herb %>% filter(herbivore_metab>0)
# drop missing hard coral sites (n = 681)
herb<-herb %>% filter(!is.na(hard_coral)) ## need to check with Tom
# keep forereef only (drops 30 sites)
herb<-herb %>% filter(reef_zone=='Forereef') ## need to check with Tom

# scale and center cont. covariates
herb_scaled <- herb %>% 
  mutate(reef_area_km2 = log10(reef_area_km2), 
         island_area_km2 = log10(land_area_km2),
         across(c(depth_m:hard_coral, 
                  sst_mean:irradiance_einsteins_m2_d1_mean, 
                  precip_amp_mm:chl_ime), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

pdf(file = 'fig/ime_crep/crep_herbivore_cov_correlations.pdf', height=7, width=15)
pairs2(
  herb_scaled %>% 
    filter(!is.na(ted_mean) & !is.na(median_ime_percent)) %>% 
    select(island_area_km2, reef_area_km2, site_bathy_400m,depth_m,hard_coral,
           precip_amp_mm, avg_monthly_mm,sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld_mean, mld_amp, ssh, ted_mean, ted_sum, month_num))
dev.off()

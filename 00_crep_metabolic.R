library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)

source('00_oceanographic_load.R')
crep<-read.csv(file = 'data/metabolic/crep_site_metabolic_rates.csv') 
biom<-read.csv(file = 'data/metabolic/crep_site_biomass.csv') 

depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv') %>% 
  mutate(depth = DEPTH, 
         hard_coral = HARD_CORAL,
         site_bathy_400m = SITE_SLOPE_400m,
         island = ISLAND,
         region = REGION,
         population_status = POP_STATUS,
         year = OBS_YEAR,
         date = as.Date(DATE_, "%d/%m/%Y"),
         month_num = month(date),
         date_ym = floor_date(date, unit = "month")) %>% 
  distinct(depth, hard_coral, site_bathy_400m, island, region, population_status,
           year, date, month_num, date_ym, SITEVISITID) %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  select(region, island, SITEVISITID, year, date_ym, month, month_num, depth, hard_coral, site_bathy_400m, population_status) %>% 
  left_join(region_df, by = 'island')


# use depth dataset from Laura as basis for model. join predictors from Gove/Williams
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
  mutate(avg_monthly_mm = ifelse(is.na(avg_monthly_mm), 0, avg_monthly_mm)) %>% 
  left_join(mld_recent %>% mutate(date_ym = Date, mld_survey = MLD, 
                                  mld_recent = mean_mld_3months) %>% 
              select(island, date_ym, mld_recent, mld_deep, mld_survey))



# 1. Create planktivore
# drop NA planktivore sites (n = 1)
plank<-depth %>% filter(!is.na(planktivore_metab))
# drop 0 planktivore sites (n = 38)
plank<-plank %>% filter(planktivore_metab>0)

# scale and center cont. covariates
plank_scaled <- plank %>% 
  mutate(reef_area_km2 = log10(reef_area+1), 
         island_area_km2 = log10(land_area+1),
         across(c(depth:site_bathy_400m, 
                  sst_mean:irradiance_einsteins_m2_d1_mean, 
                  avg_monthly_mm:mld_survey), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

pdf(file = 'fig/crep_planktivore_cov_correlations.pdf', height=7, width=15)
pairs2(
  plank_scaled %>% 
    filter(!is.na(ted_mean)) %>% 
    select(island_area_km2, reef_area_km2, site_bathy_400m,depth,hard_coral,
           avg_monthly_mm,sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld, mld_survey, mld_recent, mld_months_deep, ssh, ted_mean, ted_sum, month_num))
dev.off()


# 2. Create herbivore
# drop NA herbivore sites (n = 0)
herb<-depth %>% filter(!is.na(herbivore_metab))
# drop 0 herbivore sites (n = 2)
herb<-herb %>% filter(herbivore_metab>0)

# scale and center cont. covariates
herb_scaled <- herb %>% 
  mutate(reef_area_km2 = log10(reef_area+1), 
         island_area_km2 = log10(land_area+1),
         across(c(depth:site_bathy_400m, 
                  sst_mean:irradiance_einsteins_m2_d1_mean, 
                  avg_monthly_mm:mld_survey), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

pdf(file = 'fig/crep_herbivore_cov_correlations.pdf', height=7, width=15)
pairs2(
  herb_scaled %>% 
    filter(!is.na(ted_mean)) %>% 
    select(island_area_km2, reef_area_km2, site_bathy_400m,depth,hard_coral,
           avg_monthly_mm,sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld, mld_survey, mld_recent, mld_deep, ssh, ted_mean, ted_sum, month_num))
dev.off()

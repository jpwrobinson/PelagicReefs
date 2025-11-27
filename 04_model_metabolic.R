library(brms)
library(modelr)
library(emmeans)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)

# LOAD 
# source('00_crep_load.R')
source('00_oceanographic_load.R')
plank<-read.csv(file = 'data/metabolic/crep_site_metabolic_rates.csv') 

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
  select(region, island, SITEVISITID, year, date_ym, month, month_num, depth, hard_coral, site_bathy_400m, population_status) 


# use depth dataset from Laura as basis for model. join predictors from Gove/Williams
depth<- depth %>% 
  left_join(plank %>% select(SITEVISITID, PLANKTIVORE), by = 'SITEVISITID') %>% 
  mutate(planktivore_metab=PLANKTIVORE,
         log10_planktivore_metab = log10(planktivore_metab)) %>% 
  select(-PLANKTIVORE) %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% select(island, island_code, sst_mean:ted_sum)) %>% 
  mutate(avg_monthly_mm = ifelse(is.na(avg_monthly_mm), 0, avg_monthly_mm)) %>% 
  left_join(mld_recent %>% mutate(date_ym = Date, mld_survey = MLD) %>% select(island, date_ym, mean_mld_3months, mld_survey))

# drop NA planktivore sites (n = 1)
depth<-depth %>% filter(!is.na(planktivore_metab))
# drop 0 planktivore sites (n = 38)
depth<-depth %>% filter(planktivore_metab>0)

# scale and center cont. covariates
depth_scaled <- depth %>% 
  mutate(reef_area_km2 = log10(reef_area+1), 
         island_area_km2 = log10(land_area+1),
         across(c(depth:site_bathy_400m, 
                  sst_mean:irradiance_einsteins_m2_d1_mean, 
                  avg_monthly_mm:mld_survey), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

pdf(file = 'fig/crep_planktivore_cov_correlations.pdf', height=7, width=15)
pairs2(
  depth_scaled %>% 
    filter(!is.na(ted_mean)) %>% 
    select(island_area_km2, reef_area_km2, site_bathy_400m,depth,hard_coral,
           avg_monthly_mm,sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
           chl_a_mg_m3_mean, mld, mld_survey, ssh, ted_mean, ted_sum, month_num))
dev.off()

# basic model fitting planktivore metabolic by island and biophysical covariates
# model N = 2214
m2_linear<-brm(planktivore_metab ~ 
                    geomorphic_type * reef_area_km2 + island_area_km2 + avg_monthly_mm +
                    site_bathy_400m + hard_coral + depth + population_status +
                    # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean + #rm for collinear reasons
                    chl_a_mg_m3_mean + mld + mld_survey +
                    # mi(ted_mean) + 
                    #ssh + 
                    # (1 + mld | island / REGION),
                    (1|year) +
                    (1|region / island),
                  family = lognormal(),
        data = depth_scaled,
        # backend = "cmdstanr",
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_linear, file = 'results/mod_planktivore_metabolic.rds')


load('results/mod_planktivore_metabolic.rds')
checker<-m2_linear
summary(checker)
pp_check(checker, resp = 'planktivore_metab')
conditional_effects(checker)
bayes_R2(checker) # 54.7%



### OUTPUTS

# For linear model, extract posterior draws
effects2 <- m2_linear %>%
  gather_draws(`b_.*`, regex=TRUE) %>%  
  # filter(.variable != 'b_Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth',
                                         'mld_survey', 'mld','chl_a_mg_m3_mean')))) %>% 
  filter(!is.na(var_fac))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_planktivore_effects2.pdf', height=5, width=6)
ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  xlim(c(-2, 2))
dev.off()

## Change in planktivore flux along MLD
mld_pred<-depth_scaled %>%  
  data_grid(geomorphic_type = geomorphic_type[1], # atoll
            population_status = population_status[1], # U
            depth = 0,
            site_bathy_400m = 0,
            hard_coral = 0,
            avg_monthly_mm = 0,
            reef_area_km2 = 0,
            island_area_km2 = 0,
            chl_a_mg_m3_mean = 0,
            mld = 0,
            mld_survey = seq_range(mld_survey, n = 100),
            island=unique(island)[1],
            region=unique(region)[1]) %>%  
  add_epred_draws(m2_linear, ndraws = 100, re_formula = NA)

ggplot(mld_pred, aes(x = mld_survey)) +
  # geom_point(data = depth, aes(x = mld_survey, y = planktivore_metab)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  labs(x = 'Mixed layer depth, m', y = 'Planktivore metabolic rate')

m2_linear %>% emmeans(~ mld_survey, var = 'mld_survey', 
               at = list(mld_survey = c(min(depth_scaled$mld_survey), 
                                              max(depth_scaled$mld_survey))), 
               epred =TRUE)

mld_range<-(max(depth$mld_survey) - min(depth$mld_survey))
meta_range<-(1.358 - 0.532 )
change_per_m<- meta_range / mld_range
(change_per_m*10) / 1.358 * 100
# Metabolic rate decreases by 0.022 per metre of MLD
# Metabolic rate decreases by 16% per 10 metre of MLD



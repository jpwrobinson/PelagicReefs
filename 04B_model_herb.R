# LOAD 
source('00_crep_metabolic.R')

# basic model fitting planktivore metabolic by island and biophysical covariates

# 1. Herbivore
# model N = 2250
m2_herb<-brm(herbivore_metab ~ 
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
             data = herb_scaled,
             # backend = "cmdstanr",
             chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_herb, file = 'results/mod_herbivore_metabolic.rds')


load('results/mod_herbivore_metabolic.rds')
checker<-m2_herb
summary(checker)
pp_check(checker, resp = 'herbivore_metab')
conditional_effects(checker)
bayes_R2(checker) # 30.1%

### OUTPUTS

# For linear model, extract posterior draws
effects2 <- m2_herb %>%
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
pdf(file = 'fig/ime_crep/crep_model_herbivore_effects.pdf', height=5, width=6)
ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  xlim(c(-2, 2))
dev.off()

## Change in planktivore flux along MLD
mld_pred<-herb_scaled %>%  
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
  add_epred_draws(m2_herb, ndraws = 100, re_formula = NA)

ggplot(mld_pred, aes(x = mld_survey)) +
  # geom_point(data = depth, aes(x = mld_survey, y = herbivore_metab)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  labs(x = 'Mixed layer depth, m', y = 'Herbivore metabolic rate')

m2_herb %>% emmeans(~ mld_survey, var = 'mld_survey', 
                     at = list(mld_survey = c(min(depth_scaled$mld_survey), 
                                              max(depth_scaled$mld_survey))), 
                     epred =TRUE)

mld_range<-(max(depth$mld_survey) - min(depth$mld_survey))
meta_range<-(3.05 - 1.98 )
change_per_m<- meta_range / mld_range
(change_per_m*10) / 3.05 * 100
# Metabolic rate decreases by 0.028 per metre of MLD
# Metabolic rate decreases by 9% per 10 metre of MLD



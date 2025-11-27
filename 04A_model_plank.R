# LOAD 
source('00_crep_metabolic.R')

# basic model fitting planktivore metabolic by island and biophysical covariates

priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sd")           # penalize large sd; mean ~1, P(s>~3) small
)

# 1. Planktivore
# model N = 2214
m2_plank<-brm(planktivore_metab ~ 
                    geomorphic_type + reef_area_km2 + island_area_km2 + avg_monthly_mm +
                    site_bathy_400m + hard_coral + depth + population_status +
                    # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean + #rm for collinear reasons
                    chl_a_mg_m3_mean + mld_survey + mld + #mld_deep + mld_recent +
                    # mi(ted_mean) + 
                    #ssh + 
                    # (1 + mld | island / REGION),
                    (1|year) +
                    (1|region2 / island),
                  family = lognormal(),
        data = plank_scaled,
        prior = priors,
        # backend = "cmdstanr",
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_plank, file = 'results/mod_planktivore_metabolic.rds')

m3_plank<-brm(planktivore_biom ~ 
                geomorphic_type + reef_area_km2 + island_area_km2 + avg_monthly_mm +
                site_bathy_400m + hard_coral + depth + population_status +
                # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean + #rm for collinear reasons
                chl_a_mg_m3_mean + mld_survey + mld + #mld_deep + mld_recent +
                # mi(ted_mean) + 
                #ssh + 
                # (1 + mld | island / REGION),
                (1|year) +
                (1|region / island),
              family = lognormal(),
              data = plank_scaled,
              # backend = "cmdstanr",
              chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m3_plank, file = 'results/mod_planktivore_biomass.rds')

load('results/mod_planktivore_metabolic.rds')
checker<-m2_plank
summary(checker)
pp_check(checker, resp = 'planktivore_metab')
conditional_effects(checker)
bayes_R2(checker) 
# metabolic = 54.6%
# biomass = 15.3%

### OUTPUTS

# For linear model, extract posterior draws
effects <- m2_plank %>%
  gather_draws(`b_.*`, regex=TRUE) %>%  
  # filter(.variable != 'b_Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth',
                                         'mld_survey','mld_recent', 'mld_deep', 'mld','chl_a_mg_m3_mean')))) %>% 
  filter(!is.na(var_fac))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_planktivore_effects2.pdf', height=5, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  xlim(c(-1.5, 1.5))
dev.off()

## Change in planktivore flux along MLD
nd<-plank_scaled %>%  
  data_grid(geomorphic_type = geomorphic_type[6], # island
            population_status = population_status[2], # P
            depth = 0,
            site_bathy_400m = 0,
            hard_coral = 0,
            avg_monthly_mm = 0,
            reef_area_km2 = 0,
            island_area_km2 = 0,
            chl_a_mg_m3_mean = 0,
            mld = 0,
            mld_survey = seq_range(mld_survey, n = 100))  %>% 
  # add_linpred_draws(m2_plank, ndraws = 1000, re_formula = NA)
  add_epred_draws(m2_plank, ndraws = 1000, re_formula = NA)

ggplot(nd, aes(x = mld_survey)) +
  # geom_point(data = depth, aes(x = mld_survey, y = planktivore_metab)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  stat_lineribbon(aes(y = .epred), .width = 0.5, alpha = 0.5) +
  labs(x = 'Mixed layer depth, m', y = 'Planktivore metabolic rate')

m2_plank %>% emmeans(~ mld_survey, var = 'mld_survey', 
               at = list(mld_survey = c(min(depth_scaled$mld_survey), 
                                              max(depth_scaled$mld_survey))), 
               epred =TRUE)

mld_range<-(max(depth$mld_survey) - min(depth$mld_survey))
meta_range<-(1.486 - 0.572 )
change_per_m<- meta_range / mld_range
(change_per_m*10) / 1.486 * 100
# Metabolic rate decreases by 0.022 per metre of MLD
# Metabolic rate decreases by 16% per 10 metre of MLD


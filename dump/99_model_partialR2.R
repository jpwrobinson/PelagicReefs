load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sd")           # penalize large sd; mean ~1, P(s>~3) small
)

## R2 variance decomposition
## --- PLANKTIVORES --- ##
m2_plank_withoutGeo<-brm(planktivore_metab ~ 
                           # geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                           hard_coral + depth_m +
                           avg_monthly_mm + mld_mean + ted_mean +
                           population_status +
                           (1 | year ) + (1 | island),
                         family = hurdle_lognormal(),
                         data = plank_scaled,
                         prior = priors,
                         # backend = "cmdstanr",
                         chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_plank_withoutOcean<-brm(planktivore_metab ~ 
                             geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                             hard_coral + depth_m +
                             # avg_monthly_mm + mld_mean + ted_mean +
                             population_status +
                             (1 | year ) + (1 | island),
                           family = hurdle_lognormal(),
                           data = plank_scaled,
                           prior = priors,
                           # backend = "cmdstanr",
                           chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_plank_withoutHabitat<-brm(planktivore_metab ~ 
                               geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                               # hard_coral + depth_m +
                               avg_monthly_mm + mld_mean + ted_mean +
                               population_status +
                               (1 | year ) + (1 | island),
                             family = hurdle_lognormal(),
                             data = plank_scaled,
                             prior = priors,
                             # backend = "cmdstanr",
                             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_plank_withoutHuman<-brm(planktivore_metab ~ 
                             geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                             hard_coral + depth_m +
                             avg_monthly_mm + mld_mean + ted_mean +
                             # population_status +
                             (1 | year ) + (1 | island),
                           family = hurdle_lognormal(),
                           data = plank_scaled,
                           prior = priors,
                           # backend = "cmdstanr",
                           chains = 3, iter = 2000, warmup = 500, cores = 4)


## --- HERBIVORES --- ##
m2_herb_withoutGeo<-brm(herbivore_metab ~ 
                           # geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                           hard_coral + depth_m +
                           avg_monthly_mm + mld_mean + ted_mean +
                           population_status +
                           (1 | year ) + (1 | island),
                         family = hurdle_lognormal(),
                         data = herb_scaled,
                         prior = priors,
                         # backend = "cmdstanr",
                         chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_herb_withoutOcean<-brm(herbivore_metab ~ 
                             geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                             hard_coral + depth_m +
                             # avg_monthly_mm + mld_mean + ted_mean +
                             population_status +
                             (1 | year ) + (1 | island),
                           family = hurdle_lognormal(),
                           data = herb_scaled,
                           prior = priors,
                           # backend = "cmdstanr",
                           chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_herb_withoutHabitat<-brm(herbivore_metab ~ 
                               geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                               # hard_coral + depth_m +
                               avg_monthly_mm + mld_mean + ted_mean +
                               population_status +
                               (1 | year ) + (1 | island),
                             family = hurdle_lognormal(),
                             data = herb_scaled,
                             prior = priors,
                             # backend = "cmdstanr",
                             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_herb_withoutHuman<-brm(herbivore_metab ~ 
                             geomorphic_type + reef_area_km2 + island_area_km2 + site_bathy_400m + 
                             hard_coral + depth_m +
                             avg_monthly_mm + mld_mean + ted_mean +
                             # population_status +
                             (1 | year ) + (1 | island),
                           family = hurdle_lognormal(),
                           data = herb_scaled,
                           prior = priors,
                           # backend = "cmdstanr",
                           chains = 3, iter = 2000, warmup = 500, cores = 4)
save(
  m2_plank_withoutGeo,
  m2_plank_withoutOcean,
  m2_plank_withoutHabitat,
  m2_plank_withoutHuman,
  m2_herb_withoutGeo,
  m2_herb_withoutOcean,
  m2_herb_withoutHabitat,
  m2_herb_withoutHuman, 
  file = 'results/mod_metabolic_nested_R2.rds'
  )

# estimate relative bayes
rel_bayes<-rbind(
  data.frame(part_R2 = c(bayes_R2(m2_plank_withoutGeo, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_plank_withoutOcean, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_plank_withoutHabitat, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_plank_withoutHuman, re.form=NA)[1, 'Estimate']),
             cov = c('Geomorphic', 'Oceanographic', 'Fish habitat', 'Human'),
             mod = 'Planktivore',
             full_R2 = bayes_R2(m2_plank, re.form=NA)[1,'Estimate']),
  data.frame(part_R2 = c(bayes_R2(m2_herb_withoutGeo, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_herb_withoutOcean, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_herb_withoutHabitat, re.form=NA)[1, 'Estimate'],
                         bayes_R2(m2_herb_withoutHuman, re.form=NA)[1, 'Estimate']),
             cov = c('Geomorphic', 'Oceanographic', 'Fish habitat', 'Human'),
             mod = 'Herbivore',
             full_R2 = bayes_R2(m2_herb, re.form=NA)[1,'Estimate'])) %>% 
  mutate(rel_R2 = full_R2 - part_R2)

write.csv(rel_bayes,  file = 'results/mod_nested_R2.csv',row.names=FALSE)

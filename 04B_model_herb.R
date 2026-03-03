# LOAD 
source('00_crep_metabolic.R')

# basic model fitting herbivore metabolic by island and biophysical covariates
priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sd")           # penalize large sd; mean ~1, P(s>~3) small
)

# check vif. mean chl-a is correlated with reef area and MLD amp
car::vif(glm(log(herbivore_metab) ~ 
               land_area_km2 + avg_monthly_mm +
               reef_area_km2 + site_bathy_400m + 
               # mean_chlorophyll +
               ted_mean +
               mld_amp, data=herb_scaled))

# 1. Herbivore
# model N = 4340 [2009-2024]
m2_herb<-brm(herbivore_metab ~ 
                geomorphic_type + reef_area_km2 + island_area_km2 + avg_monthly_mm +
                site_bathy_400m + 
                hard_coral + 
                depth_m + 
                mld_amp + 
               # mean_chlorophyll +
                (1 | year) +
                (1 | island),
              family = lognormal(),
              data = herb_scaled,
              prior = priors,
              # backend = "cmdstanr",
              chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_herb, file = 'results/mod_herbivore_metabolic.rds')


load('results/mod_herbivore_metabolic.rds')
checker<-m2_herb
summary(checker)
pp_check(checker, resp = 'herbivore_metab')
conditional_effects(checker)
bayes_R2(checker) # 32.6%

### OUTPUTS

# For linear model, extract posterior draws
effects <- checker %>%
  gather_draws(`b_.*`, regex=TRUE) %>%  
  # filter(.variable != 'b_Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth_m',
                                         # 'chl_a_mg_m3_mean'
                                         'mld_amp')))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value)))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_herbivore_effects.pdf', height=3, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  xlim(c(-1.2, 1.2))
dev.off()

## Change in herbivore flux along MLD

conditional_effects(m2_herb, c('mld_amp', 'depth')) %>% 
  plot(rug=TRUE)


m2_herb %>% emmeans(~ mld_amp, var = 'mld_amp', 
                     at = list(mld_amp = c(min(herb_scaled$mld_amp), 
                                           max(herb_scaled$mld_amp))), 
                     epred =TRUE)

mld_range<-(max(herb$mld_amp) - min(herb$mld_amp))
meta_range<-(1.68 - 1.23 )
change_per_m<- meta_range / mld_range
(change_per_m) / 1.68 
(change_per_m*10) / 1.68 * 100
# Metabolic rate decreases by 0.008 per metre of MLD amplitude
# Metabolic rate decreases by 8% per 10 metre of MLD amplitude


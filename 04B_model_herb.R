# LOAD 
source('00_crep_metabolic.R')

# basic model fitting herbivore metabolic by island and biophysical covariates
priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sd")           # penalize large sd; mean ~1, P(s>~3) small
)

# 1. Herbivore
# model N = 2250
m2_herb<-brm(herbivore_metab ~ 
                reef_area_km2 + island_area_km2 + avg_monthly_mm +
                site_bathy_400m + hard_coral + depth + 
                mld_amp + #chl_a_mg_m3_mean +
                (1 | year) +
                (1 | geomorphic_type) +
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
bayes_R2(checker) # 30.1%

### OUTPUTS

# For linear model, extract posterior draws
effects <- m2_herb %>%
  gather_draws(`b_.*`, regex=TRUE) %>%  
  # filter(.variable != 'b_Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth',
                                         'mld_amp', 'chl_a_mg_m3_mean')))) %>% 
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

# Extract posterior samples
nd<-herb_scaled %>%  
  data_grid(
    depth = 0,
    site_bathy_400m = 0,
    hard_coral = 0,
    avg_monthly_mm = 0,
    reef_area_km2 = 0,
    island_area_km2 = 0,
    chl_a_mg_m3_mean = seq_range(chl_a_mg_m3_mean, n =100),
    mld_amp = seq_range(mld_amp, n = 100))  %>% 
  # mutate(mld_amp_raw = seq_range(herb$mld_amp, n = 100)) %>% 
  add_epred_draws(m2_herb, ndraws = 1000, re_formula = NA) %>% 
  group_by(mld_amp, chl_a_mg_m3_mean) %>% 
  summarise(
    median = median(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975)
  )

ggplot(nd, aes(x = mld_amp, y = chl_a_mg_m3_mean, fill=median)) +
  # geom_point(data = depth, aes(x = mld_amp, y = herbivore_metab)) +
  # stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  # stat_lineribbon(aes(y = .epred), .width = 0.5, alpha = 0.5) +
  geom_tile()+
  labs(x = 'Mixed layer variability, m', y = 'Herbivore metabolic rate')

m2_herb %>% emmeans(~ mld_amp, var = 'mld_amp', 
                     at = list(mld_amp = c(min(herb_scaled$mld_amp), 
                                           max(herb_scaled$mld_amp))), 
                     epred =TRUE)

mld_range<-(max(herb$mld_amp) - min(herb$mld_amp))
meta_range<-(1.117 - 0.499 )
change_per_m<- meta_range / mld_range
(change_per_m) / 1.117 
(change_per_m*10) / 1.117 * 100
# Metabolic rate decreases by 0.016 per metre of MLD amplitude
# Metabolic rate decreases by 16% per 10 metre of MLD amplitude


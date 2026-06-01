
# basic model fitting planktivore metabolic by island and biophysical covariates

priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sigma")           # penalize large scale values; mean ~1, P(s>~3) small
)

load('results/mod_planktivore_metabolic.rds')
plank_scaledT<-plank_scaled %>% group_by(island) %>% mutate(n_years = n_distinct(year), mean_plank = mean(planktivore_metab)) %>% 
  filter(n_years > 3) %>% 
  ungroup() %>% 
  mutate(rel_plank = planktivore_metab - mean_plank,
         year_cat = as.character(year)) %>% 
  select(year, island, region, SITEVISITID, date_ym, depth_m, hard_coral, site_bathy_400m, planktivore_metab)


# 1. Planktivore
# model N = 3383 [2009-2024]

m2_plankTime<-brm(planktivore_metab ~ depth_m + hard_coral + site_bathy_400m +
                    (1 | island:year) + (1 | island),
              family = hurdle_lognormal(),
              data = plank_scaledT,
              prior = priors,
              # backend = "cmdstanr",
              chains = 3, iter = 2000, warmup = 500, cores = 4)

# save(m2_plank, plank_scaled, file = 'results/mod_planktivore_metabolic.rds')

# load('results/mod_planktivore_metabolic.rds')
checker<-m2_plankTime
summary(checker)
pp_check(checker, resp = 'planktivore_metab')
conditional_effects(checker)
ranef(checker)
bayes_R2(checker)  # r2 = 54.5%

### OUTPUTS
island_year_effects <- ranef(m2_plankTime)$`island:year` %>%
  as_tibble(rownames = "island_year") %>%
  separate(island_year, c("island", "year"), sep = "_") %>%
  mutate(year = as.numeric(year)) %>% 
  rename_with(~str_remove(., ".Intercept"), everything()) %>% 
  mutate(sig = ifelse(Q2.5 > 0 & Q97.5 > 0, 'blue', 'black'),
         sig = ifelse(Q2.5 < 0 & Q97.5 < 0, 'red', sig))

ggplot(island_year_effects, aes(year, Estimate, col=sig)) + 
  geom_hline(yintercept=0) +
  geom_point() + 
  facet_wrap(~island) +
  scale_colour_identity() 


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
                    # (1 | island:year) + (1 | island), # this uses shrinkage and is not appropriate - islands may respond differently over time
                    factor(island):factor(year),  # independent effect for each island-year
              family = hurdle_lognormal(),
              data = plank_scaledT,
              prior = priors,
              # backend = "cmdstanr",
              chains = 3, iter = 2000, warmup = 500, cores = 4)

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

pdf(file = 'fig/planktivore_time.pdf', height=7, width=12)
ggplot(island_year_effects, aes(year, Estimate, col=sig)) + 
  geom_hline(yintercept=0) +
  geom_point() + 
  labs(x = '', y = 'Random intercept', subtitle = 'Planktivore flux: site-level coral and depth and varying year-island') +
  facet_wrap(~island) +
  scale_colour_identity() 
dev.off()

# for fixed version
island_year_fixef<-fixef(m2_plankTime) %>%
  as_tibble(rownames = "term") %>%
  filter(str_detect(term, "island.*year")) %>%
  separate(term, c("island", "year"), sep = ":") %>%
  mutate(
    island = str_extract(island, "[A-Za-z0-9]+$"),
    island = str_replace(island, 'factorisland', ''),
    year = as.numeric(str_extract(year, "\\d+")),
    sig = ifelse(Q2.5 > 0 & Q97.5 > 0, 'blue', 'black'),
    sig = ifelse(Q2.5 < 0 & Q97.5 < 0, 'red', sig)
  )

ggplot(island_year_fixef, aes(year, Estimate, col=sig)) + 
  geom_hline(yintercept=0) +
  geom_point() + 
  labs(x = '', y = 'Random intercept', subtitle = 'Planktivore flux: site-level coral and depth and varying year-island') +
  facet_wrap(~island) +
  scale_colour_identity()

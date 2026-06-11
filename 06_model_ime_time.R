source('0_loads/00_oceanographic_load.R')
## This script models temporal trends in the IME% (chl enhancement).
## It is not ideal because lots of year - month - island combinations are NA. They do have Chl_max, but not always an IME detection.

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region)
  ) %>% 
  # matching MLD, but note this is for the 1st of the month, whereas IME is 15th
  left_join(mld %>% mutate(date = as.Date(format(Date, "%Y-%m-15"))) %>% select(date, island, MLD)) %>% 
  # filter(!is.na(MLD)) %>% 
  mutate(mld_s = scale(MLD)[,1]) %>% 
  group_by(island, month) %>% 
  mutate(mld_mean = mean(MLD, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(mld_anom = MLD - mld_mean, 
         mld_anom_s = scale(mld_anom),
         mld_mean_s = scale(mld_mean),
         island=factor(island))

## Hurdle approach of gamma + binomial models.
# Examining temopral trends in Chl_% by island, accounting for seasonality. 
focal<-ime_df %>% 
  filter(!is.na(has_IME)) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))

## 1. Fit binomial version = on/off IME
# n = 11149 [3 dropped from swains]

m_detect <- brm(bf(
  has_IME ~ 
    s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

# compare with original bam, frequentist model
# m_detect2 <- bam(
#   has_IME ~
#     s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
#     s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
#     s(time_s, by = island, bs = "cr", k = 10),   # island-level probability
#   family = binomial,
#   data = focal,
#   method = "fREML",
#   discrete = TRUE,
#   rho = 0.4,
#   AR.start = focal$new_series
# )

# hist(resid(m_detect2))
# acf(resid(m_detect2))
# intervals(m_detect2$lme, which = "var-cov")  # 95% CI for rho
# m_detect2$AR1.rho

gratia::draw(m_detect2, select = 'mld_mean', partial_match=TRUE)
gratia::draw(m_detect2, select = 'mld_anom', partial_match=TRUE)
gratia::draw(m_detect2, select = 'time_s', partial_match=TRUE)

save(ime_df, focal, m_detect, m_detect2, file = 'results/mod_ime_time_binom.rds')

load('results/mod_ime_time_binom.rds')

summary(m_detect)
pp_check(m_detect)

conditional_effects(m_detect, effects = 'mld_mean_s')
conditional_effects(m_detect, effects = 'mld_anom_s')
conditional_effects(m_detect, effects = 'time_s')
# 
# smooth_estimates <- smooth_estimates(m_detect2) %>%
#   filter(smooth == "s(time_s):island")
# 
# # 2. Visualize island-specific trajectories
# draw_smooth <- draw(m_detect2, select = "s(time_s)")

## 2. Gamma on has_IME == 1
# n = 6132 , ~5020 obs dropped
focalCont<-focal %>% filter(has_IME == 1)

m_hurdle<-brm(bf(
  Chl_increase_nearby ~ 
          s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
          s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
          s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
        family = Gamma(link = 'log'),
        data = focalCont,
        backend = "cmdstanr",
        chains = 3,
        cores = 4
        )

save(ime_df, focal, m_hurdle, file = 'results/mod_ime_time_hurdle.rds')

load('results/mod_ime_time_hurdle.rds')
summary(m_hurdle)
pp_check(m_hurdle)
conditional_effects(m_hurdle, effects = 'mld_mean_s')
conditional_effects(m_hurdle, effects = 'mld_anom_s')
conditional_effects(m_hurdle, effects = 'time_s')


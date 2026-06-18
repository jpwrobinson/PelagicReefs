source('0_loads/00_oceanographic_load.R')
## This script models temporal trends in the IME% (chl enhancement).
## It is not ideal because lots of year - month - island combinations are NA. They do have Chl_max, but not always an IME detection.

# mld_trend<-read.csv(file = 'results/mld_time_pred.csv') %>% mutate(date = as.Date(time), time=NULL, mld_pred = mld, mld=NULL)
delta_anom<-read.csv(file = 'results/MLD_anom_change.csv')

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region)
  ) %>% 
  # matching MLD, but note this is for the 1st of the month, whereas IME is 15th
  left_join(mld %>% mutate(date = as.Date(format(Date, "%Y-%m-15"))) %>% select(date, island, month_mean, anomaly)) %>% 
  # matching MLD anomaly predictions
  left_join(delta_anom) %>% 
  ungroup() %>% 
  mutate(mld_clim = month_mean,
         mld_anom = anomaly,
         mld_change = change,
         change = NULL, month_mean = NULL, anomaly = NULL,
         mld_anom_s = scale(mld_anom),
         mld_clim_s = scale(mld_clim),
         mld_change_s = scale(mld_change),
         island=factor(island)) 

## Hurdle approach of gamma + binomial models.
# Examining temopral trends in Chl_% by island, accounting for seasonality. 
focal<-ime_df %>% 
  filter(!is.na(has_IME)) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))

## 1. Fit binomial version = on/off IME
# n = 11149 [3 dropped from swains]

m_detectMLD <- brm(bf(
  has_IME ~ 
    s(mld_clim_s, k=3) + # MLD climatology
    s(mld_anom_s, k=3) + # MLD anomaly
    s(mld_change_s, k = 3) + 
    (1 | island)), # MLD trend
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

m_detectFull <- brm(bf(
  has_IME ~ 
    s(mld_clim_s, k=3) + # MLD climatology
    s(mld_anom_s, k=3) + # MLD anomaly
    s(mld_change_s, k = 3) + # MLD trend
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

# time null model
m_detectTime <- brm(bf(
  has_IME ~ 
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

save(ime_df, focal, m_detectMLD, m_detectFull, m_detectTime, file = 'results/mod_ime_time_binom.rds')

load('results/mod_ime_time_binom.rds')
checker<-m_detectFull
# checker<-m_detect
summary(checker)
pp_check(checker)

conditional_effects(checker, effects = 'mld_clim_s')
conditional_effects(checker, effects = 'mld_anom_s')
conditional_effects(checker, effects = 'mld_change_s')
conditional_effects(checker, effects = 'time_s')
bayes_R2(checker,  re.form=NA) # R2 = 0.119
loo(m_detect, m_detectTime, m_detectFull) # MLD slope is supported

## 2. Gamma on has_IME == 1
# n = 6132 , ~5020 obs dropped
focalCont<-focal %>% filter(has_IME == 1 & !is.na(Chl_increase_nearby))

# MLD only model
m_hurdleMLD<-brm(bf(
  Chl_increase_nearby ~ 
          s(mld_clim_s, k=3) + # MLD climatology
          s(mld_anom_s, k=3) + # MLD anomaly
          s(mld_change_s, k = 3)), # MLD trend
        family = Gamma(link = 'log'),
        data = focalCont,
        backend = "cmdstanr",
        chains = 3,
        cores = 4
        )

# full model
m_hurdleFull<-brm(bf(
  Chl_increase_nearby ~ 
    s(mld_clim_s, k=3) + # MLD climatology
    s(mld_anom_s, k=3) + # MLD anomaly
    s(mld_change_s, k = 3) + # MLD trend
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = Gamma(link = 'log'),
  data = focalCont,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)


# time null model
m_hurdleTime<-brm(bf(
  Chl_increase_nearby ~ 
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = Gamma(link = 'log'),
  data = focalCont,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)


save(ime_df, focalCont, m_hurdleMLD, m_hurdleFull, m_hurdleTime, file = 'results/mod_ime_time_hurdle.rds')

load('results/mod_ime_time_hurdle.rds')

checker<-m_hurdleFull
summary(checker)
pp_check(checker)
bayes_R2(checker) # 43.5%
conditional_effects(checker, effects = 'mld_clim_s')
conditional_effects(checker, effects = 'mld_anom_s')
conditional_effects(checker, effects = 'mld_change_s')
conditional_effects(checker, effects = 'time_s') # time marginalised over islands


# Extract LOO and save as table
loo1<-loo(m_detect, m_detectFull, m_detectTime)
loo2<-loo(m_hurdle, m_hurdleFull, m_hurdleTime)

rbind(
  loo1$diffs %>%
  as.data.frame() %>%
  rownames_to_column("model") %>% 
  mutate(dist = 'IME detect'),
  loo2$diffs %>%
    as.data.frame() %>%
    rownames_to_column("model") %>% 
    mutate(dist = 'IME strength')
  ) %>% 
  write.csv('results/loo_ime_probability.csv', row.names=FALSE)

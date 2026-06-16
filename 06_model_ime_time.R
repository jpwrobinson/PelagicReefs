source('0_loads/00_oceanographic_load.R')
## This script models temporal trends in the IME% (chl enhancement).
## It is not ideal because lots of year - month - island combinations are NA. They do have Chl_max, but not always an IME detection.

mld_trend<-read.csv(file = 'results/mld_time_pred.csv') %>% mutate(date = as.Date(time), time=NULL, mld_pred = mld, mld=NULL)

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
  # matching mLD predictions
  left_join(mld_trend %>% mutate(date = as.Date(format(date, "%Y-%m-15")))) %>% 
  # filter(!is.na(MLD)) %>% 
  mutate(mld_s = scale(MLD)[,1]) %>% 
  group_by(island, month) %>% 
  mutate(mld_mean = mean(MLD, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(mld_anom = MLD - mld_mean, 
         mld_anom_s = scale(mld_anom),
         mld_mean_s = scale(mld_mean),
         mld_pred_s = scale(mld_pred),
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

m_detectMLDtrend <- brm(bf(
  has_IME ~ 
    s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(mld_pred_s, k = 3) + # MLD deepening trend
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

m_detectNoMLD <- brm(bf(
  has_IME ~ 
    # s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = bernoulli,
  data = focal,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)

save(ime_df, focal, m_detect, m_detectMLDtrend, m_detectNoMLD, file = 'results/mod_ime_time_binom.rds')

load('results/mod_ime_time_binom.rds')
checker<-m_detectMLDtrend
checker<-m_detect
summary(checker)
pp_check(checker)

conditional_effects(checker, effects = 'mld_mean_s')
conditional_effects(checker, effects = 'mld_anom_s')
conditional_effects(checker, effects = 'mld_pred_s')
conditional_effects(checker, effects = 'time_s')
bayes_R2(checker,  re.form=NA) # R2 = 0.085
loo(m_detect, m_detectMLDtrend) # MLD pred is supported

# smooth_estimates <- smooth_estimates(m_detect2) %>%
#   filter(smooth == "s(time_s):island")
# 
# # 2. Visualize island-specific trajectories
# draw_smooth <- draw(m_detect2, select = "s(time_s)")

## 2. Gamma on has_IME == 1
# n = 6132 , ~5020 obs dropped
focalCont<-focal %>% filter(has_IME == 1 & !is.na(Chl_increase_nearby))

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

m_hurdleMLDtrend<-brm(bf(
  Chl_increase_nearby ~ 
    s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(mld_pred_s, k = 3) + # MLD deepening
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = Gamma(link = 'log'),
  data = focalCont,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)


# MLD mediating time effect
m_hurdleNoMLD<-brm(bf(
  Chl_increase_nearby ~ 
    # s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability
  family = Gamma(link = 'log'),
  data = focalCont,
  backend = "cmdstanr",
  chains = 3,
  cores = 4
)


save(ime_df, focalCont, m_hurdle, m_hurdleMLDtrend, m_hurdleNoMLD, file = 'results/mod_ime_time_hurdle.rds')

load('results/mod_ime_time_hurdle.rds')

checker<-m_hurdleMLDtrend
summary(checker)
pp_check(checker)
bayes_R2(checker) # 38.1%
conditional_effects(checker, effects = 'mld_mean_s')
conditional_effects(checker, effects = 'mld_anom_s')
conditional_effects(checker, effects = 'time_s') # time marginalised over islands
ce<-conditional_effects(checker, effects = "time_s", 
                    conditions = distinct(focalCont, island))

plot(ce, plot = FALSE)[[1]] +
  coord_cartesian(ylim = c(0, 2))


# Extract LOO and save as table
loo1<-loo(m_detect, m_detectMLDtrend, m_detectNoMLD)
loo2<-loo(m_hurdle, m_hurdleMLDtrend, m_hurdleNoMLD)

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

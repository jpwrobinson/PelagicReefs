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
  # method = "fREML",
  # discrete = TRUE,
  # rho = 0.4,
  # AR.start = focal$new_series
)

save(ime_df, focal, m_detect, file = 'results/mod_ime_time_binom.rds')

load('results/mod_ime_time_binom.rds')

summary(m_detect)

# hist(resid(m_detect))
# acf(resid(m_detect))
# m_detect$AR1.rho
# intervals(m_detect$lme, which = "var-cov")  # 95% CI for rho
 
# gratia::draw(m_detect, select = 'mld_mean', partial_match=TRUE)
# gratia::draw(m_detect, select = 'mld_anom', partial_match=TRUE)
# gratia::draw(m_detect, select = 'time_s', partial_match=TRUE)

pp_check(m_detect)

conditional_effects(m_detect, effects = 'mld_mean_s')
conditional_effects(m_detect, effects = 'mld_anom_s')
conditional_effects(m_detect, effects = 'time_s')

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
        # method = "fREML",
        # discrete = TRUE,
        # rho = 0.4,
        # AR.start = focal$new_series
        )

save(ime_df, focal, m_hurdle, file = 'results/mod_ime_time_hurdle.rds')

# Dev. expl = 3.3%
load('results/mod_ime_time_hurdle.rds')
hist(resid(m_hurdle))
summary(m_hurdle)
acf(resid(m_hurdle))
gratia::draw(m_hurdle, select = 'mld_s', partial_match=TRUE)
gratia::draw(m_hurdle, select = 'mld_anom',  partial_match=TRUE)

conditional_effects(m_hurdle, effects = 'mld_mean_s')
conditional_effects(m_hurdle, effects = 'mld_anom_s')
conditional_effects(m_hurdle, effects = 'time_s')





ime_df %>% filter(mld_anom_s > 3)

ex_smooths <- grep("month|time|mean", smooths(m_detect), value = TRUE)

df2<-expand.grid(month = 1, time_s = 0, mld_mean_s = 0, island = unique(focal$island)[1], 
                 mld_anom_s = seq(min(focal$mld_anom_s), max(focal$mld_anom_s), length.out=100))
df2$pred<-predict(m_detect, newdata = df2, type='response', exclude=ex_smooths)
df2$se<-predict(m_detect, newdata = df2, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

df2<-df2 %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df2$mld_anom<-seq(min(focal$mld_anom), max(focal$mld_anom), length.out=100)

ggplot(df2, aes(mld_anom, pred, col=region.col, group=island, ymin = lower, ymax = upper)) + 
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  labs(x = '', y = 'Probability IME detection')


## 3. Fit hurdel version = on/off IME + strength
# focal<-ime_df %>% filter(!is.na(mld_anom)) %>% 
#   mutate(Chl_increase_nearby = ifelse(is.na(Chl_increase_nearby), 0, Chl_increase_nearby),
#          Chl_increase_nearby = ifelse(Chl_increase_nearby<0, 0, Chl_increase_nearby))
# 
# focal<-focal %>% filter(Chl_increase_nearby>0)
# 
# m_hurdle <- bam(
#   Chl_increase_nearby ~ 
#     s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
#     s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
#     s(time_s, by = island, bs = "cr", k = 10),   # island-level probability
#   family = Gamma(link = 'log'),
#   data = focal,
#   method = "fREML",
#   discrete = TRUE
# )

# n = 9724
# m_hurdleB <- brm(
#   bf(Chl_increase_nearby ~ 
#        s(mld_mean_s, k=3) + s(mld_anom_s, k=3) +
#        # s(month, bs = 'cc', k = 12, by = island) + 
#        s(time_s, by = island, bs = "cr", k = 10)),
#   family = hurdle_lognormal(),  # or hurdle_gamma()
#   data = focal,
#   chains = 1,
#   cores = 4,
#   backend = 'cmdstanr'
# )

# Tweedie models do not allow you to separately model the zero-generating process.
# Zeros are an inherent part of the continuous distribution. - this is not true for IME??
# m_tweedie <- bam(
#   Chl_increase_nearby ~ 
#        s(mld_mean_s, k=3) + s(mld_anom_s, k=3) +
#        s(month, bs = 'cc', k = 12, by = island) +
#        s(time_s, by = island, bs = "cr", k = 10),
#   family = tw(), 
#   method = "fREML",
#   discrete = TRUE,
#   data = focal
# )

# summary(m_hurdle) # dev. expl = 19.5%
# gratia::draw(m_hurdle, select = 'mld_mean', partial_match=TRUE)
# gratia::draw(m_hurdle, select = 'mld_anom', partial_match=TRUE)
# gratia::draw(m_hurdle, select = 'time_s', partial_match=TRUE)
# 
# summary(m_hurdle2)
# conditional_effects(m_hurdle2, c('mld_mean_s', 'mld_anom_s', 'time_s'))
# conditional_effects(m_hurdle2, c('mld_mean_s', 'mld_anom_s', 'time_s'), dpar='hu')
# 
# summary(m_tweedie) # dev. expl = 20.2%
# gratia::draw(m_tweedie, select = 'mld_mean', partial_match=TRUE)
# gratia::draw(m_tweedie, select = 'mld_anom', partial_match=TRUE)
# gratia::draw(m_tweedie, select = 'time_s', partial_match=TRUE)


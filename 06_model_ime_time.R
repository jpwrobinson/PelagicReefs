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

save(ime_df, focal, m_detect, file = 'results/mod_ime_time_binom.rds')
save(ime_df, focal, m_detectNoMLD, file = 'results/mod_ime_time_binom_mediator.rds')

load('results/mod_ime_time_binom.rds')

summary(m_detect)
pp_check(m_detect)

conditional_effects(m_detect, effects = 'mld_mean_s')
conditional_effects(m_detect, effects = 'mld_anom_s')
conditional_effects(m_detect, effects = 'time_s')
bayes_R2(m_detect,  re.form=NA) 

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

save(ime_df, focalCont, m_hurdle, file = 'results/mod_ime_time_hurdle.rds')

load('results/mod_ime_time_hurdle.rds')
summary(m_hurdle)
pp_check(m_hurdle)
conditional_effects(m_hurdle, effects = 'mld_mean_s')
conditional_effects(m_hurdle, effects = 'mld_anom_s')
conditional_effects(m_hurdle, effects = 'time_s') # time marginalised over islands
ce<-conditional_effects(m_hurdle, effects = "time_s", 
                    conditions = distinct(focalCont, island))

plot(ce, plot = FALSE)[[1]] +
  coord_cartesian(ylim = c(0, 2))


## 3. MLD mediating time effect
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

save(ime_df, focalCont, m_hurdleNoMLD, file = 'results/mod_ime_time_hurdle_mediator.rds')


## For IME detect (binom)
# Create a time grid for each island
time_grid <- with(focal, expand.grid(
                     island = unique(island),
                    time_s = seq(min(time_s), max(time_s), length.out = 20)) %>%
  mutate(mld_mean_s = 0,  # hold at mean 
         mld_anom_s = 0,
         month = 12))       # hold month constant

# Get posterior epred for both models
pred_no_mld <- time_grid %>%
  add_epred_draws(m_detectNoMLD, re_formula = NA, ndraws = 200)

pred_detect <- time_grid %>%
  add_epred_draws(m_detect, re_formula = NA, ndraws = 200)

# Estimate slope per island per draw as linear regression of prediction on time
slope_fn <- function(df) {
  lm(.epred ~ time_s, data = df)$coefficients["time_s"]
}

slopes_no_mld <- pred_no_mld %>%
  group_by(island, .draw) %>%
  summarise(slope = slope_fn(cur_data()), .groups = "drop") %>%
  mutate(model = "no_mld")

slopes_detect <- pred_detect %>%
  group_by(island, .draw) %>%
  summarise(slope = slope_fn(cur_data()), .groups = "drop") %>%
  mutate(model = "detect")

# Summarise and plot
bind_rows(slopes_detect) %>%
  group_by(island, model) %>%
  summarise(mean_slope = mean(slope),
            lower = quantile(slope, 0.025),
            upper = quantile(slope, 0.975),
            .groups = "drop") %>%
  ggplot(aes(x = reorder(island, mean_slope), y = mean_slope,
             ymin = lower, ymax = upper, colour = model)) +
  geom_pointrange(position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  lims(y = c(-.5, 1)) +
  labs(x = NULL, y = "Posterior slope of time effect",
       title = "Attenuation of temporal trend after including MLD")


## For IME Strength (hurdle)
# Create a time grid for each island
time_grid <- with(focalCont, expand.grid(
  island = unique(island),
  time_s = seq(min(time_s), max(time_s), length.out = 20)) %>%
    mutate(mld_mean_s = 0,  # hold at mean for hurdle model
           mld_anom_s = 0,
           month = 12))       # hold month constant

# Get posterior epred for both models
pred_no_mld <- time_grid %>%
  add_epred_draws(m_hurdleNoMLD, re_formula = NA, ndraws = 200)

pred_hurdle <- time_grid %>%
  add_epred_draws(m_hurdle, re_formula = NA, ndraws = 200)

# Estimate slope per island per draw as linear regression of prediction on time
slope_fn <- function(df) {
  lm(.epred ~ time_s, data = df)$coefficients["time_s"]
}

slopes_no_mld <- pred_no_mld %>%
  group_by(island, .draw) %>%
  summarise(slope = slope_fn(cur_data()), .groups = "drop") %>%
  mutate(model = "no_mld")

slopes_hurdle <- pred_hurdle %>%
  group_by(island, .draw) %>%3
summarise(slope = slope_fn(cur_data()), .groups = "drop") %>%
  mutate(model = "hurdle")

# Summarise and plot
bind_rows(slopes_no_mld, slopes_hurdle) %>%
  group_by(island, model) %>%
  summarise(mean_slope = mean(slope),
            lower = quantile(slope, 0.025),
            upper = quantile(slope, 0.975),
            .groups = "drop") %>%
  ggplot(aes(x = reorder(island, mean_slope), y = mean_slope,
             ymin = lower, ymax = upper, colour = model)) +
  geom_pointrange(position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  lims(y = c(-.5, 1)) +
  labs(x = NULL, y = "Posterior slope of time effect",
       title = "Attenuation of temporal trend after including MLD")

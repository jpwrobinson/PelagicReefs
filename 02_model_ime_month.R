source('0_loads/00_plot_theme.R')

options(mc.cores = 4)
rstan::rstan_options(auto_write = TRUE)
Sys.setenv(STAN_NUM_THREADS = "4")
stan_model_args <- list(
  cpp_options = list(STAN_THREADS = TRUE)
)
options(brms.backend = "cmdstanr")
cmdstanr::set_cmdstan_path()

# Is the strength of upwelling (IME) linked to MLD and tidal conversion?

# exp. vars = MLD + tidal conversion
source('0_loads/00_ime_dataframe.R')

# y distributions
hist(dat$median_chl_percent)
hist(dat_month$Chl_increase_nearby)

dat_scaled_month %>% filter(!is.na(Chl_increase_nearby)) %>% dim # N = 388, 35 islands
dat_scaled_month %>% filter(!is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% distinct(island) # N = 352, 32 islands

# basic model fitting Chl increase (%) by month and island

mod_dat<-dat_scaled_month %>% filter(!is.na(Chl_increase_nearby) & !is.na(bathymetric_slope))

m2_month<-brm(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12),
              data = mod_dat, family = lognormal())

summary(m2_month)
pp_check(m2_month, resp = 'Chlincreasenearby')
conditional_effects(m2_month)
bayes_R2(m2_month) # 27%

nd<-expand.grid(month_num = c(1:12), island = unique(mod_dat$island))

condo<-conditional_effects(m2_month, 'month_num', prob=0.95,
                           conditions = nd, re_formula=NA)[[1]] %>% 
  mutate(lower95 = lower__, upper95 = upper__) %>% 
  select(month_num, island, estimate__, lower95, upper95) %>% 
  left_join(mod_dat %>% distinct(island, REGION))

ggplot(condo, aes(month_num, estimate__, group=island)) + 
  geom_line() + 
  facet_wrap(~REGION)



save(mod_dat, m2_month, file = 'results/mod_ime_month.rds')

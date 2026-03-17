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
hist(dat_month$Chl_increase_nearby)
hist(dat_month$Chl_max)

dat_scaled_month %>% filter(!is.na(Chl_increase_nearby)) %>% dim # N = 388, 35 islands
dat_scaled_month %>% filter(!is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% distinct(island) # N = 352, 32 islands
dat_scaled_month %>% filter(is.na(ted_mean)) %>% distinct(island) # N=3, Johnston, Nihoa, Necker

# basic model fitting Chl increase (%) by island and biophysical covariates
## Linear model is marginally preferred to smooths (after including mld ~ island as random). 
## Month models work better but are captuing an unknown process.

# check vif
car::vif(glm(Chl_increase_nearby ~ 
              land_area_km2 + 
               # avg_monthly_mm_mean + 
               avg_monthly_mm_anom +
              reef_area_km2 + bathymetric_slope + 
              ted_mean +
              mld_mean + mld_anom +
              mean_chlorophyll,  data=dat_scaled_month))

mod_dat<-dat_scaled_month %>% filter(!is.na(Chl_increase_nearby) & !is.na(bathymetric_slope))

m_chl_inc<-brm(bf(Chl_increase_nearby ~ 
                    bathymetric_slope +
                    geomorphic_type * reef_area_km2 + land_area_km2 + 
                    avg_monthly_mm_anom +
                    mld_mean + mld_anom +
                    mean_chlorophyll + 
                    mi(ted_mean), 
                    # (1 + mld_anom + avg_monthly_mm_anom | island),
                  family = lognormal()
) +
  bf(ted_mean | mi() ~ reef_area_km2),
prior = c(
  prior(normal(0, 1), class = "b", resp = 'Chlincreasenearby')
  # prior(exponential(1), class = "sd", resp = 'Chlincreasenearby')
),
data = mod_dat,
# backend = "cmdstanr",
chains = 3, iter = 2000, warmup = 500, cores = 4)


## model 2 = chl_max (is it relevant for reef fish?)
mod_dat2<-dat_scaled_month %>% filter(!is.na(Chl_max) & !is.na(bathymetric_slope))

car::vif(glm(Chl_max ~ 
               land_area_km2 + #avg_monthly_mm +
               reef_area_km2 + bathymetric_slope + 
               # population_status + VIF = 5.68
               avg_monthly_mm_anom +
               mld_mean + mld_anom +
               ted_mean,
               # mean_chlorophyll + 
               , family = Gamma, data=mod_dat2))

m_chl_max<-brm(bf(Chl_max ~ 
                    bathymetric_slope +
                    geomorphic_type * reef_area_km2 + land_area_km2 + 
                    # avg_monthly_mm +
                    # mean_chlorophyll + 
                    avg_monthly_mm_anom +
                    mld_mean + mld_anom +
                    mi(ted_mean),
                    # (1 + mld | island),
                  family = lognormal()
) +
  bf(ted_mean | mi() ~ reef_area_km2),
prior = c(
  prior(normal(0, 1), class = "b", resp = 'Chlmax')
  # prior(exponential(1), class = "sd", resp = 'Chlmax')
),
data = mod_dat2,
# backend = "cmdstanr",
chains = 3, iter = 2000, warmup = 500, cores = 4)


# load(file = 'results/mod_ime.rds')
checker<-m_chl_inc
checker<-m_chl_max
summary(checker)
pp_check(checker, resp = 'Chlincreasenearby')
pp_check(checker, resp = 'Chlmax')
conditional_effects(checker)
bayes_R2(checker) # 54% for Chl-increase, 72% for chl-max

save(dat_month, dat_scaled_month, mod_dat, mod_dat2, m_chl_inc, m_chl_max, effects, file = 'results/mod_ime.rds')


# For linear model, extract posterior draws
effects <- m_chl_inc %>%
  gather_draws(b_Chlincreasenearby_geomorphic_typeIsland, b_Chlincreasenearby_reef_area_km2, b_Chlincreasenearby_land_area_km2,
               b_Chlincreasenearby_bathymetric_slope, 
               b_Chlincreasenearby_avg_monthly_mm_anom, b_Chlincreasenearby_mld_mean, b_Chlincreasenearby_mld_anom,
               b_Chlincreasenearby_mean_chlorophyll, bsp_Chlincreasenearby_mited_mean) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_Chlincreasenearby_', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlincreasenearby_mi', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','land_area_km2',
                                         'bathymetric_slope',
                                         'mean_chlorophyll','ted_mean', 'mld_mean',
                                         'mld_anom', 'avg_monthly_mm_anom' 
                                         ))))

effects2 <- m_chl_max %>%
  gather_draws(b_Chlmax_geomorphic_typeIsland, b_Chlmax_reef_area_km2, b_Chlmax_land_area_km2,
               b_Chlmax_bathymetric_slope,
               b_Chlmax_avg_monthly_mm_anom, b_Chlmax_mld_mean, b_Chlmax_mld_anom,
               # b_Chlmax_mean_chlorophyll,
               bsp_Chlmax_mited_mean) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_Chlmax_', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlmax_mi', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','land_area_km2',
                                         'bathymetric_slope',
                                         'mean_chlorophyll','ted_mean', 'mld_mean',
                                         'mld_anom', 'avg_monthly_mm_anom' 
                          ))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_month_crep_model.pdf', height=5, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "", subtitle = 'y = Chl increase nearby, %') 

ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "", subtitle = 'y = Chl max') 


dev.off()
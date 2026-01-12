source('loads/00_plot_theme.R')

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
hist(dat$mean_chl_percent)
hist(dat_month$Chl_increase_nearby)

dat_scaled_month %>% filter(!is.na(Chl_increase_nearby)) %>% dim # N = 388, 35 islands
dat_scaled_month %>% filter(!is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% distinct(island) # N = 352, 32 islands

# basic model fitting Chl increase (%) by island and biophysical covariates
## Linear model is marginally preferred to smooths (after including mld ~ island as random). 
## Month models work better but are captuing an unknown process.

# check vif
car::vif(lm(Chl_increase_nearby ~ 
              land_area_km2 + avg_monthly_mm +
              reef_area_km2 + bathymetric_slope + 
              # population_status + VIF = 5.68
              ted_mean +
              mean_chlorophyll + mld, data=dat_scaled_month))

mod_dat<-dat_scaled_month %>% filter(!is.na(Chl_increase_nearby) & !is.na(bathymetric_slope))

m2_linear<-brm(bf(Chl_increase_nearby ~ 
                    bathymetric_slope +
                    geomorphic_type * reef_area_km2 + land_area_km2 + avg_monthly_mm +
                    mean_chlorophyll + mld + 
                    mi(ted_mean) + 
                    (1 + mld | island),
                  family = lognormal()
) +
  bf(ted_mean | mi() ~ reef_area_km2),
prior = c(
  prior(normal(0, 1), class = "b", resp = 'Chlincreasenearby'),
  prior(exponential(1), class = "sd", resp = 'Chlincreasenearby')
),
data = mod_dat,
# backend = "cmdstanr",
chains = 3, iter = 2000, warmup = 500, cores = 4)

# load(file = 'results/mod_ime.rds)
checker<-m2_linear
summary(checker)
pp_check(checker, resp = 'Chlincreasenearby')
conditional_effects(checker)
bayes_R2(checker) # 65%

res <- residuals(checker, summary = FALSE)
fitted <- fitted(checker, summary = FALSE)
res_mean <- rowMeans(res)
acf(res_mean, main = "ACF of model residuals")

# For linear model, extract posterior draws
effects <- m2_linear %>%
  gather_draws(b_Chlincreasenearby_geomorphic_typeIsland, b_Chlincreasenearby_reef_area_km2, b_Chlincreasenearby_land_area_km2,
               b_Chlincreasenearby_bathymetric_slope, b_Chlincreasenearby_avg_monthly_mm,
               b_Chlincreasenearby_mean_chlorophyll, bsp_Chlincreasenearby_mited_mean, b_Chlincreasenearby_mld) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_Chlincreasenearby_', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlincreasenearby_mi', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','land_area_km2',
                                         'bathymetric_slope','avg_monthly_mm', 'mean_chlorophyll',
                                         'ted_mean', 'mld'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_month_crep_model.pdf', height=5, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 

dev.off()


save(dat_month, dat_scaled_month, mod_dat, m2_linear, effects, file = 'results/mod_ime.rds')


## checking bathymetry ~ reef area effects. 
# conclusion: bathymetry explains no extra variance after reef area, because reef area captures bathy and additional process
ep <- posterior_epred(checker, re_formula = NA)

# predictions removing one term at a time
ep_no_bathy <- posterior_epred(
  checker,
  newdata = transform(mod_dat %>% filter(!is.na(ted_mean)), bathymetric_slope = 0),
  re_formula = NA
)

ep_no_reef <- posterior_epred(
  checker,
  newdata = transform(mod_dat %>% filter(!is.na(ted_mean)), reef_area_km2 = 0),
  re_formula = NA
)

var_full     <- apply(ep, 1, var)
var_no_bathy <- apply(ep_no_bathy, 1, var)
var_no_reef  <- apply(ep_no_reef, 1, var)


unique_bathy = var_full - var_no_bathy
unique_reef = var_full - var_no_reef

prop_reef  <- unique_reef / var_full
prop_bathy <- unique_bathy / var_full

hist(posterior_summary(prop_reef)[,1])
hist(posterior_summary(prop_bathy)[,1])

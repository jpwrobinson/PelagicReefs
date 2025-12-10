source('00_plot_theme.R')

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
source('00_oceanographic_load.R')
source('00_ime_dataframe.R')

# y distributions
hist(dat$mean_chl_percent)
hist(dat_month$Chl_increase_nearby)

dat_scaled_month %>% filter(!is.na(Chl_increase_nearby)) %>% dim # N = 388, 35 islands
dat_scaled_month %>% filter(!is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% distinct(island) # N = 352, 32 islands

# basic model fitting Chl increase (%) by island and biophysical covariates
m2_linear<-brm(bf(Chl_increase_nearby ~ 
                 geomorphic_type * reef_area_km2 + island_area_km2 + avg_monthly_mm +
                 # bathymetric_slope + population_status + 
                 # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean + #rm for collinear reasons
                 chl_a_mg_m3_mean + mld + 
                 mi(ted_mean) + 
                 #ssh + 
                 (1 + mld | island / REGION),
                 family = lognormal()
                 ) +
                 bf(ted_mean | mi() ~ reef_area_km2),
               prior = c(
                 prior(normal(0, 1), class = "b", resp = 'Chlincreasenearby'),
                 prior(exponential(1), class = "sd", resp = 'Chlincreasenearby')
               ),
               data = dat_scaled_month,
               # backend = "cmdstanr",
               chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_smooth<-brm(bf(Chl_increase_nearby ~ 
          s(reef_area_km2, by = geomorphic_type, k=3) + s(island_area_km2, k=3) + s(avg_monthly_mm, k=3) +
          # s(bathymetric_slope, k=3) + population_status +
         # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
           # for mld by island, use factor-smooth that pools towards global smooth
         s(chl_a_mg_m3_mean, k=3) + s(mld, k=3) + s(mld, by = island, bs = 'cs', k=3) + #s(ted_mean, k=3) +
          mi(ted_mean) + 
         (1 | island / REGION),
         family = lognormal()
         ) +
       bf(ted_mean | mi() ~ reef_area_km2),
       prior = c(
         prior(normal(0, 1), class = "b", resp = 'Chlincreasenearby'),
         prior(exponential(1), class = "sd", resp = 'Chlincreasenearby')
       ),
       data = dat_scaled_month,
       chains = 3, iter = 2000, warmup = 500, cores = 4, backend = "cmdstanr", threads = threading(4))

# use month smoother instead of MLD
m2_linear_month<-brm(bf(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12) + 
          geomorphic_type * reef_area_km2 + island_area_km2 + avg_monthly_mm +
          # bathymetric_slope + population_status +
          # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
          chl_a_mg_m3_mean + mld +
            mi(ted_mean) + 
          (1 + mld | island / REGION),
          family = lognormal()
          ) +
            bf(ted_mean | mi() ~ reef_area_km2),
          prior = c(
            prior(normal(0, 1), class = "b", resp = 'Chlincreasenearby'),
            prior(exponential(1), class = "sd", resp = 'Chlincreasenearby')
          ),
        data = dat_scaled_month,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_smooth_month<-brm(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12) + 
                 s(reef_area_km2, by = geomorphic_type, k=3) + s(island_area_km2, k=3) + s(avg_monthly_mm, k=3) +
                 # s(bathymetric_slope, k=3) + population_status +
                 # sst_mean + wave_energy_mean_kw_m1 + irradiance_einsteins_m2_d1_mean +
                 s(chl_a_mg_m3_mean, k=3) + s(mld, k=3) + s(ted_mean, k=3) + 
                 (1 | island / REGION),
               family = lognormal(),
               data = dat_scaled_month,
               chains = 3, iter = 2000, warmup = 500, cores = 4)


save(dat_month, dat_scaled_month, m2_linear, m2_smooth, m2_linear_month,m2_smooth_month, file = 'results/mod_ime_month_crep_attributes.rds')
# load(file = 'results/mod_ime_month_crep_attributes.rds')

checker<-m2_linear
summary(checker)
pp_check(checker, resp = 'Chlincreasenearby')
conditional_effects(checker)
bayes_R2(checker)

res <- residuals(checker, summary = FALSE)
fitted <- fitted(checker, summary = FALSE)
res_mean <- rowMeans(res)
acf(res_mean, main = "ACF of model residuals")

# compare smooth vs linear
loo_linear <- loo(m2_linear, newdata = dat_scaled_month %>% na.omit())
loo_smooth <- loo(m2_smooth, newdata = dat_scaled_month %>% na.omit())
loo_month_full <- loo(m2_linear_month, newdata = dat_scaled_month %>% na.omit())
loo_mld_smoo <- loo(m2_smooth_month)
loo_compare(loo_linear, loo_smooth, loo_month_full)

## Linear model is marginally preferred to smooths (after including mld ~ island as random). 
## Month models work better but are captuing an unknown process.


# Covariate relative effects - doesn't seem to work with smooths
# mfx <- avg_slopes(m2_smooth, newdata = dat_scaled_month)
# mfx

# For linear model, extract posterior draws
effects2 <- m2_linear %>%
  gather_draws(b_Chlincreasenearby_geomorphic_typeIsland, b_Chlincreasenearby_reef_area_km2, b_Chlincreasenearby_island_area_km2,
               # b_Chlincreasenearby_bathymetric_slope, b_Chlincreasenearby_population_statusU,
               b_Chlincreasenearby_chl_a_mg_m3_mean, bsp_Chlincreasenearby_mited_mean, b_Chlincreasenearby_mld) %>%  
  mutate(.variable = str_replace_all(.variable, 'b_Chlincreasenearby_', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlincreasenearby_mi', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean'))))

effects3 <- m2_smooth %>%
  gather_draws( bs_Chlincreasenearby_sisland_area_km2_1,
               # bs_Chlincreasenearby_sbathymetric_slope_1, b_population_statusU,
               bs_Chlincreasenearby_savg_monthly_mm_1,
               bs_Chlincreasenearby_schl_a_mg_m3_mean_1, bsp_Chlincreasenearby_mited_mean, bs_Chlincreasenearby_smld_1) %>%  
  mutate(.variable = str_replace_all(.variable, 'bs_Chlincreasenearby_s', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlincreasenearby_mi', ''),
         .variable = str_replace_all(.variable, '_1', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU','avg_monthly_mm',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean'))))

# Plot effect sizes
pdf(file = 'fig/ime_db/ime_month_crep_model.pdf', height=5, width=6)
ggplot(effects2, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 

ggplot(effects3, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") 

dev.off()



## Variance explained = the variance across fitted effects, relative to total variance in the model fixed effects
## So this is an R2 within the total model R2?? Need to check.
# Get posterior draws of each smoother's fitted contribution
sm <- conditional_smooths(m2_smooth, summary = FALSE)

# For each smooth: compute variance across its fitted effect
smooth_var <- map_dbl(sm, ~{
  df <- .x
  var(df$estimate__, na.rm = TRUE)
})

# Scale to relative proportions
rel_var <- smooth_var / sum(smooth_var)

# Tidy result
var_exp<-data.frame(
  smooth = names(sm),
  var_explained = smooth_var,
  rel_var = rel_var
)

var_exp$var<-c('Reef area\n(by geomorphic)', 'Island area', 'Precipitation', 
               # 'Bathymetric slope', 
               'chl-a mean', 'Mixed layer depth', 'Mixed layer depth\n(by island)')#, 'Tidal energy')


ggplot(var_exp, aes(fct_reorder(var, -rel_var), rel_var)) + geom_col() + scale_y_continuous(labels=label_percent())

# What about lags [month is better than MLD...but not a process]
m2_mld0<-brm(Chl_increase_nearby ~ mld, family = lognormal(), data = dat_scaled_month,
              chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_mld1<-brm(Chl_increase_nearby ~ mld_lag1, family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_mld2<-brm(Chl_increase_nearby ~ mld_lag2, family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

m2_month<-brm(Chl_increase_nearby ~ s(month_num, by = island, bs = 'cc', k=12), family = lognormal(), data = dat_scaled_month,
             chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_mld0,m2_mld1, m2_mld2, m2_month, file = 'results/mod_ime_month_vs_mld_crep_loo.rds')


load(file = 'results/mod_ime_month_vs_mld_crep_loo.rds')
# compare mld lag with MLD model
loo0 <- loo(m2_mld0)
loo1 <- loo(m2_mld1)
loo2 <- loo(m2_mld2)
looM <- loo(m2_month)
loo_compare(loo0, loo1, loo2, looM) # No lag is supported

nd<-dat_scaled_month %>% na.omit() %>% select(month_num, island)
condo<-conditional_effects(m2_month, 'month_num', prob=0.95,
                           conditions = nd, re_formula=NULL)[[1]] %>% 
  mutate(lower95 = lower__, upper95 = upper__) %>% 
  left_join(dat_scaled_month %>% select(island, REGION))

ggplot(condo, aes(month_num, estimate__, ymax= upper95, ymin = lower95, group=island)) + 
  geom_ribbon(alpha=0.1) +
  geom_line() + facet_grid(~REGION, space='free_y') + lims(y = c(0, 500))

ggplot(dat_month, aes(month_num,mld, group=island)) + 
  geom_line(col='grey50') + 
  geom_point(aes(col=Chl_increase_nearby)) + 
  scale_colour_continuous(palette = c("#FEE0D2", "#FC9272", "#DE2D26")) +
  facet_grid(~REGION, space='free_y') + lims(y = c(0, 100))

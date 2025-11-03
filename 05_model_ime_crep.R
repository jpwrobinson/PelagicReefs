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

# ime = mean upwelling %
ime_island<-read.csv(file = 'island_ime_dat.csv') %>% select(-lon, -lat, -type)
ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% select(-lon, -lat, -type)

hist(ime_island$mean_ime_percent) # Gamma

# dim(dat) = 30 island (complexes)
dat<-ime_island %>% left_join(
  island_complex %>% ungroup() %>% 
    mutate(island = str_replace_all(island_group, '_C', '')) %>%
    select(island, island_group, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))


# missing islands
island$island[!island$island %in% ime_island$island]
# ime_island %>%  filter(str_detect(island, 'L')) %>% distinct(island) %>% data.frame

# missing CREP from modelled dataset
island %>% filter(!island %in% dat$island) %>% data.frame
# but note that we are using island complex, so this captures 
# 'Maui, Lanai, Molokai, Lanai, Kahoolawe' = 'Maui_C',
# 'Saipan, Tinian, Aguijan' = 'Saipan_C',
# 'Ofu, Olosega, Tau' = 'Tau_C'

# 6 missing CREP islands in modelled dataset
unique(island$island[!island$island %in% dat$island]) 
# "Ofu & Olosega" "Lanai"         "Molokai"         "Tinian"        "Aguijan"   
# but these are because IME dataset contains IME for 'lead' island (Maui, Saipan, Tau)
# Maro Reef is NA

# These islands are missing tidal energy values
island_complex %>% filter(island_group %in% c('Johnston', 'Necker', 'Nihoa')) %>% data.frame

# ime_island %>%  filter(str_detect(island, 'Laysan')) %>% 
#   distinct(island) %>% data.frame

# crep_depth<-read.csv('data/noaa-crep/crep_bathymetry_merged.csv')
# island %>% filter(!island %in% crep_depth$ISLAND) %>% distinct(island) %>% data.frame

# csv of modelled data
dat %>% distinct(island, lat, lon, REGION, geomorphic_type) %>% write.csv('ime_complex_crep_lat_lon.csv', row.names=FALSE)
island %>% distinct(island, latitude, longitude, REGION, geomorphic_type) %>% write.csv('ime_island_crep_lat_lon.csv', row.names=FALSE)

# dim(dat_month) = 420 (12 * 35)
dat_month<-ime_month %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  left_join(island_complex %>% ungroup() %>% 
              mutate(island = str_replace_all(island_group, '_C', '')) %>%
              select(island, REGION, region.col, sst_mean:ted_sum, -mld, -mld_sd, -avg_monthly_mm),
  by = 'island') %>% 
  left_join(mld_month %>% ungroup() %>% 
              mutate(month_num=month) %>% 
              select(month_num, island, mld)) %>% 
  left_join(ssh_vals_m %>% ungroup() %>% 
              select(island, month_num, ssh)) %>% 
  left_join(precip %>% ungroup() %>% 
              mutate(month_num=month) %>% 
              select(month_num, island, avg_monthly_mm), by = c('month_num', 'island')) %>% 
  mutate(avg_monthly_mm = ifelse(is.na(avg_monthly_mm), 0, avg_monthly_mm)) %>% 
  filter(!is.na(mld)) %>% 
  group_by(island) %>% 
  mutate(chl_anom = Chl_max - chl_a_mg_m3_mean,
    mld_lag1 = mld[c(12, 1:11)], mld_lag2 = mld[c(11:12, 1:10)]) %>% 
  ungroup()

pdf(file = 'fig/ime_db/ime_month_crep.pdf', height=5, width=12)
ggplot(dat_month, aes(month_num, Chl_increase_nearby, col=island)) + 
  geom_line() + facet_wrap(~REGION) +
  geom_text(data = dat_month %>% group_by(island) %>% 
              slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
  theme(legend.position = 'none') +
  scale_y_continuous(limits=c(0, 220))

ggplot(dat_month, aes(month_num, chl_anom, col=island)) + 
  geom_line() + facet_wrap(~REGION) +
  geom_text(data = dat_month %>% group_by(island) %>% 
              slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
  theme(legend.position = 'none') 
dev.off()


dat_scaled<-dat %>%  
  select(island:island_area_km2, mean_chl_percent, REGION:ted_sum) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, avg_monthly_mm, sst_mean:ted_sum, -geomorphic_type,-population_status, -mean_chl_percent), 
                ~scale(., center=TRUE, scale=TRUE))) %>% na.omit()

dat_scaled_month<-dat_month %>% 
  select(island:island_area_km2, month, month_num, Chl_increase_nearby, chl_anom, REGION:mld_lag2) %>% 
  mutate(reef_area_km2 = log10(reef_area), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(month_num, island_area_km2, reef_area_km2, avg_monthly_mm, sst_mean:irradiance_einsteins_m2_d1_mean, 
                  bathymetric_slope, ssh:mld_lag2), 
                ~terra::scale(., center=TRUE, scale=TRUE)[,1]),
         population_status_num = ifelse(population_status == 'U', 0, 1))

# Create pairs plot for IME covariates
pdf(file = 'fig/crep_island_month_correlations.pdf', height=7, width=15)
pairs2(
  dat_scaled_month %>% 
    filter(!is.na(bathymetric_slope) & !is.na(ted_mean) & !is.na(Chl_increase_nearby)) %>% 
    select(island_area_km2, reef_area_km2, bathymetric_slope,avg_monthly_mm,population_status_num,
             sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean,
             chl_a_mg_m3_mean, mld, ssh, ted_mean, ted_sum))
dev.off()

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

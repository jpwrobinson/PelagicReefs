library(brms)
library(modelr)
library(emmeans)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)

# LOAD 
source('00_crep_load.R')
source('00_oceanographic_load.R')
plank<-read.csv(file = 'data/metabolic/crep_site_metabolic_rates.csv') 

# use depth dataset from Laura as basis for model. join predictors from Gove/Williams
depth<- depth %>% 
  mutate(planktivore_metab=plank$PLANKTIVORE[match(depth$SITEVISITID, plank$SITEVISITID)],
         log10_planktivore_metab = log10(planktivore_metab),
         month = month(DATE_),
         date_ym = floor_date(DATE_, unit = "month")) %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% mutate(ISLAND = island) %>% select(ISLAND, island_code, sst_mean:ted_sum)) %>% 
  left_join(mld_recent %>% mutate(date_ym = Date, ISLAND = Island) %>% select(ISLAND, date_ym, mean_mld_3months))

#€# scale and center cont. covariates
depth_scaled <- depth %>% 
  mutate(reef_area_km2 = log10(reef_area_km2), 
         across(c(DEPTH_c, SITE_SLOPE_400m, sst_mean:reef_area_km2, mld:mean_mld_3months), 
                ~scale(., center=TRUE, scale=TRUE)))

# explanatory covariate structure
fix2 <- ~ DEPTH_c +
  atoll_island +
  reef_area_km2 +
  POP_STATUS +
  SITE_SLOPE_400m + 
  chl_a_mg_m3_mean +
  wave_energy_mean_kw_m1 + # wave energy at each island
  ted_sum + # sum of tidal energy to island (internal wave energy)
  ted_mean + # average tidal energy to island
  mld + # average mixed layer depth around island
  mean_mld_3months + # mixed layer depth 3 months prior to survey
  (1|OBS_YEAR) +
  (1|REGION/ISLAND)

# model formula with y-var of planktivore metabolism
pos_form <- formula(paste(c('planktivore_metab', fix2), collapse = " ")) # Formula for models with positive data only

hist(depth_scaled$planktivore_metab)
hist(depth_scaled$log10_planktivore_metab)

m1<-brm(bf(pos_form), family = lognormal(), data = depth_scaled %>% filter(planktivore_metab>0),
            chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m1, file = 'results/mod_planktivore_metabolic.rds')


load('results/mod_planktivore_metabolic.rds')
summary(m1)
conditional_effects(m1)

# Extract posterior draws
effects <- m1 %>%
  spread_draws(b_atoll_islandIsland, b_DEPTH_c, b_SITE_SLOPE_400m, b_reef_area_km2,
               b_chl_a_mg_m3_mean, b_wave_energy_mean_kw_m1, b_ted_sum, b_ted_mean, b_mld, b_mean_mld_3months) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect") %>% 
  mutate(Variable = str_replace_all(Variable, 'b_', ''),
         var_fac = factor(Variable, 
                             levels = rev(c('DEPTH_c', 'SITE_SLOPE_400m', 'atoll_islandIsland','reef_area_km2',
                                        'ted_mean', 'mld', 'mean_mld_3months','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1','ted_sum'))))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_planktivore_effects.pdf', height=3, width=5)
ggplot(effects %>% filter(!var_fac %in% c('ted_sum', 'wave_energy_mean_kw_m1')), aes(x = Effect, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  scale_x_continuous(breaks=seq(-1, 1, by = 0.5), limits=c(-1.1, .6), 
                     sec.axis = dup_axis()) 
dev.off()

# Multipanel of conditional effects
pdf(file = 'fig/ime_crep/crep_model_planktivore.pdf', height=7, width=12)
ce<-plot(conditional_effects(m1), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()

# Create pairs plot for CREP covariates
GGally::ggpairs(
  depth %>% 
    select(SITE_SLOPE_400m_c, wave_energy_mean_kw_m1, ted_sum, ted_mean, mld, mean_mld_3months),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal()  # Apply a clean theme

## Change in planktivore flux along MLD
mld_pred<-depth_scaled %>%  
  data_grid(atoll_island = depth_scaled$atoll_island[1],
            POP_STATUS = depth_scaled$POP_STATUS[1],
            DEPTH_c = 0,
            SITE_SLOPE_400m = 0,
            reef_area_km2 = 0,
            chl_a_mg_m3_mean = 0,
            wave_energy_mean_kw_m1 = 0,
            ted_sum = 0,
            ted_mean = 0,
            mld = 0,
            mean_mld_3months = seq_range(mean_mld_3months, n = 100),
            island=unique(depth_scaled$island)[1],
            region=unique(depth_scaled$region)[1]) %>%  
  mutate(mean_mld_3months_obs = seq_range(depth$mean_mld_3months, n = 100)) %>% 
  add_epred_draws(m1, ndraws = 100, re_formula = NA)
  

ggplot(mld_pred, aes(x = mean_mld_3months_obs)) +
  # geom_point(data = depth, aes(x = mean_mld_3months, y = planktivore_metab)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  labs(x = 'Mixed layer depth, m', y = 'Planktivore metabolic rate')

m1 %>% emmeans(~ mean_mld_3months, var = 'mean_mld_3months', 
               at = list(mean_mld_3months = c(min(depth_scaled$mean_mld_3months), 
                                              max(depth_scaled$mean_mld_3months))), 
               epred =TRUE)

mld_range<-(max(depth$mean_mld_3months) - min(depth$mean_mld_3months))
meta_range<-(0.978 - 0.416 )
change_per_m<- meta_range / mld_range
(change_per_m *10) / 0.978 * 100
# Metabolic rate decreases by 0.01 per metre of MLD
# Metabolic rate decreases by 0.01 per metre of MLD



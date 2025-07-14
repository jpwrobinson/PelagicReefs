library(brms)
library(modelr)
library(emmeans)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)

# LOAD 
# source('00_crep_load.R')
source('00_oceanographic_load.R')
depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv') %>% 
  mutate(DATE_ = as.Date(DATE_, "%d/%m/%Y"))
plank<-read.csv(file = 'data/metabolic/crep_site_metabolic_rates.csv') 

# use depth dataset from Laura as basis for model. join predictors from Gove/Williams
depth<- depth %>% 
  mutate(planktivore_metab=plank$PLANKTIVORE[match(depth$SITEVISITID, plank$SITEVISITID)],
         log10_planktivore_metab = log10(planktivore_metab),
         month = month(DATE_),
         date_ym = floor_date(DATE_, unit = "month")) %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% mutate(ISLAND = island) %>% select(ISLAND, island_code, sst_mean:ted_sum)) %>% 
  left_join(mld_recent %>% mutate(date_ym = Date, ISLAND = Island, mld_survey = MLD) %>% select(ISLAND, date_ym, mean_mld_3months, mld_survey))

# scale and center cont. covariates
depth_scaled <- depth %>% 
  mutate(reef_area_km2 = log10(reef_area_km2), 
         across(c(DEPTH_c, HARD_CORAL, SITE_SLOPE_400m, 
                  sst_mean:reef_area_km2, mld:mld_survey), 
                ~scale(., center=TRUE, scale=TRUE)))

# explanatory covariate structure
fix2 <- ~ DEPTH_c +
  HARD_CORAL +
  atoll_island +
  reef_area_km2 +
  POP_STATUS +
  SITE_SLOPE_400m + 
  chl_a_mg_m3_mean +
  sst_mean + # mean sea surface temp at island
  wave_energy_mean_kw_m1 + # mean wave energy at island
  ted_sum + # sum of tidal energy to island (internal wave energy)
  ted_mean + # mean of tidal energy to island (internal wave energy)
  mld_survey + # mixed layer depth at survey date (island-level)
  mld + # average mixed layer depth around island
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
  spread_draws(b_atoll_islandIsland, b_DEPTH_c, b_SITE_SLOPE_400m, b_HARD_CORAL, b_reef_area_km2,
               b_chl_a_mg_m3_mean, b_wave_energy_mean_kw_m1, b_ted_mean, b_ted_sum, b_mld, b_mld_survey) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect") %>% 
  mutate(Variable = str_replace_all(Variable, 'b_', ''),
         var_fac = factor(Variable, 
                             levels = rev(c('DEPTH_c', 'SITE_SLOPE_400m', 'atoll_islandIsland','reef_area_km2','HARD_CORAL',
                                        'ted_mean', 'mld', 'mld_survey','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1','ted_sum'))))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_planktivore_effects.pdf', height=3, width=5)
ggplot(effects, aes(x = Effect, y = var_fac)) +
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
    select(SITE_SLOPE_400m_c, wave_energy_mean_kw_m1, chl_a_mg_m3_mean, ted_sum, ted_mean, mld, mld_months_deep, mean_mld_3months, mld_survey),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) + theme_minimal()  

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
            mld_survey = seq_range(mld_survey, n = 100),
            island=unique(depth_scaled$island)[1],
            region=unique(depth_scaled$region)[1]) %>%  
  mutate(mld_survey_obs = seq_range(depth$mld_survey, n = 100)) %>% 
  add_epred_draws(m1, ndraws = 100, re_formula = NA)
  

ggplot(mld_pred, aes(x = mld_survey_obs)) +
  # geom_point(data = depth, aes(x = mld_survey, y = planktivore_metab)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  labs(x = 'Mixed layer depth, m', y = 'Planktivore metabolic rate')

m1 %>% emmeans(~ mld_survey, var = 'mld_survey', 
               at = list(mld_survey = c(min(depth_scaled$mld_survey), 
                                              max(depth_scaled$mld_survey))), 
               epred =TRUE)

mld_range<-(max(depth$mld_survey) - min(depth$mld_survey))
meta_range<-(1.179 - 0.318 )
change_per_m<- meta_range / mld_range
(change_per_m *10) / 1.179 * 100
# Metabolic rate decreases by 0.01 per metre of MLD



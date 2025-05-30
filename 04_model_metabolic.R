library(brms)
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
         month = month(DATE_),
         date_ym = floor_date(DATE_, unit = "month")) %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% mutate(ISLAND = island) %>% select(ISLAND, island_code, sst_mean:ted_sum)) %>% 
  left_join(mld_recent %>% mutate(date_ym = Date, ISLAND = Island) %>% select(ISLAND, date_ym, mean_mld_3months))

# scale and center cont. covariates
depth_scaled <- depth %>% 
  mutate(across(c(DEPTH_c, SITE_SLOPE_400m_c, sst_mean:mean_mld_3months), 
                ~scale(., center=TRUE, scale=TRUE)))

# explanatory covariate structure
fix2 <- ~ DEPTH_c +
  POP_STATUS +
  SITE_SLOPE_400m_c + 
  wave_energy_mean_kw_m1 + # wave energy at each island
  ted_sum + # sum of tidal energy to island (internal wave energy)
  ted_mean + # average tidal energy to island
  mld + # average mixed layer depth around island
  mean_mld_3months + # mixed layer depth 3 months prior to survey
  (1|OBS_YEAR) +
  (1|ISLAND/SITE)

# model formula with y-var of planktivore metabolism
pos_form <- formula(paste(c('planktivore_metab', fix2), collapse = " ")) # Formula for models with positive data only
bin_form <- formula(paste(c('hu', fix2), collapse = " "))

m1<-brm(bf(pos_form, bin_form), family = hurdle_gamma, data = depth_scaled,
            chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m1, file = 'results/mod_planktivore_metabolic.rds')

summary(m1)
conditional_effects(m1)

# Extract posterior draws
effects <- m1 %>%
  spread_draws(b_DEPTH_c, b_POP_STATUS, b_SITE_SLOPE_400m_c, 
               b_wave_energy_mean_kw_m1, b_ted_sum, b_ted_mean, b_mld, b_mean_mld_3months) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect")

# Plot effect sizes
ggplot(effects, aes(x = Effect, y = Variable)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  theme_minimal()

# Multipanel of conditional effects
pdf(file = 'fig/ime_crep/ime_model_planktivore.pdf', height=7, width=12)
ce<-plot(conditional_effects(m1), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()

# Create pairs plot for CREP covariates
GGally::ggpairs(
  depth_ime %>% 
    select(SITE_SLOPE_400m_c, wave_energy_mean_kw_m1, ted_sum, ted_mean, mld, mean_mld_3months),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal()  # Apply a clean theme




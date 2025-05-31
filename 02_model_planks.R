library(brms)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)
library(ggrepel)


source('00_crep_load.R')
source('00_oceanographic_load.R')

# read ime and change island names
ime<-read.csv(file = 'island_ime_dat.csv') %>% 
  mutate(reef_area_km2_log10 = log10(reef_area_km2),
          island2 = trimws(str_replace_all(island, 'Atoll', '')),
         island2 = trimws(str_replace_all(island2, 'Island', '')),
         island2 = trimws(str_replace_all(island2, 'Reef', '')),
         island2 = trimws(str_replace_all(island2, '\\ and', '\\ &')),
         island2 = case_match(island2, 
                              'Hawai’i' ~ 'Hawaii',
                              'French Frigate Shoals' ~ 'French Frigate',
                              'Kaua’i' ~ 'Kauai',
                              'Ni’ihau' ~ 'Niihau',
                              'O’ahu' ~ 'Oahu',
                              'Swains  (Olohega)' ~ 'Swains',
                              'Ta’u' ~ 'Tau', .default = island2))

# 5 missing islands in depth
unique(depth$ISLAND[!depth$ISLAND %in% ime$island2]) 
# "Ofu & Olosega" "Lanai"         "Molokai"         "Tinian"        "Aguijan"     
# islands %>%  filter(str_detect(island_name, 'kai')) %>% distinct(island_name) %>% data.frame

pdf(file = 'fig/ime_db/noaa_months_max_chl.pdf', height=5, width=7)
ime %>% filter(island2 %in% depth$ISLAND) %>% 
  ggplot(aes(months_ime, chl_island, col=lat_neg*-1)) + 
  geom_point(alpha=1) +
  geom_text_repel(aes(label=island2), size=2) +
  labs(x = 'Number of months IME present', y = 'Climatology: mean maximum chl-a, mg/m3') +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_color_gradientn(colors = rev(c("#043061", "#4475B4",'#FE9928', "#B2182B", "#670A1F")), name = 'Dist.to\nEquator')
dev.off()

## Creating island-level database of slope, IME variables, and oceanographic from Gove/Williams
depth_ime<-depth %>% 
  filter(ISLAND %in% ime$island2) %>% 
  group_by(ISLAND, REGION, POP_STATUS) %>% 
  summarise(SITE_SLOPE_400m_c = mean(SITE_SLOPE_400m_c)) %>% 
  mutate(island2 = ISLAND) %>% 
  ungroup() %>% 
  # Bring IME variables
  left_join(ime, by = 'island2') %>% 
  # Bring Gove, MLD and TD variables
  left_join(island %>% mutate(ISLAND = island) %>% select(ISLAND, island_code, sst_mean:ted_sum), by = 'ISLAND') 
  # left_join(mld_recent %>% mutate(date_ym = Date, ISLAND = Island) %>% select(ISLAND, date_ym, mean_mld_3months)) 

depth_ime_scaled <- depth_ime %>% 
  filter(!is.na(ted_mean)) %>% 
  mutate(across(c(reef_area_km2_log10, island_area_km2, SITE_SLOPE_400m_c, mean_ime_percent, 
                  cv_chl, chl_island, months_ime, sst_mean, wave_energy_mean_kw_m1, irradiance_einsteins_m2_d1_mean:ted_sum), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

# 29 islands, excluding 6 islands, mostly large MHI or Marianas
pdf(file = 'fig/crep_island_ime_oceanography.pdf', height=7, width=15)
ime %>% 
  left_join(island %>% mutate(island2 = island) %>% select(island_code, island2, region)) %>% 
  filter(island2 %in% depth_ime$island2) %>% 
  mutate(island_code = factor(island_code, levels = rev(levs))) %>% 
  select(island_code, region, reef_area_km2, island_area_km2, chl_island, cv_ime, mean_ime_percent, months_ime) %>% 
  pivot_longer(-c(island_code, region), names_to = 'cov', values_to = 'val') %>% 
  ggplot(aes(island_code, val, fill=region)) + geom_col() +
  facet_grid(~cov, scales='free') + 
  coord_flip() +
  theme(legend.position = 'none') +
  labs( x= '', y = '')
dev.off()


fix <- ~
  reef_area_km2_log10 +
  # island_area_km2 + [correlated with reef area]
  POP_STATUS +
  SITE_SLOPE_400m_c + 
  wave_energy_mean_kw_m1 + # wave energy at each island
  # ted_sum + # sum of tidal energy to island (internal wave energy)
  ted_mean + # average tidal energy to island
  mld + # average mixed layer depth around island
  # mld_sd + # mixed layer depth 3 months prior to survey [correlated with MLD]
  months_ime + ## duration of IME in months
  # cv_chl + ## annual variation in nearby chl-a
  mean_ime_percent  ## average increase in chl-a during IME months [relative to non-IME REF]
  # (1|REGION)

# with Richardson covariates
pos_form <- formula(paste(c('chl_a_mg_m3_mean', fix), collapse = " ")) # Formula for models with positive data only

hist(depth_ime_scaled$chl_a_mg_m3_mean)

m1<-brm(bf(pos_form), family = Gamma(), data = depth_ime_scaled,
    chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m1, file = 'results/mod_crep_chl_oceangr.rds')

summary(m1)
conditional_effects(m1)

# Extract posterior draws
effects <- m1 %>%
  spread_draws(b_DEPTH_c, b_POP_STATUSU, b_SITE_SLOPE_400m_c, b_chl_island, b_months_ime, b_cv_chl) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect")

# Plot effect sizes
ggplot(effects, aes(x = Effect, y = Variable)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  theme_minimal()

# Multipanel of conditional effects
pdf(file = 'fig/ime_crep/crep_island_chl_model.pdf', height=7, width=12)
ce<-plot(conditional_effects(m1), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()

# Create pairs plot for IME ~ CREP covariates
ggpairs(
  depth_ime %>% 
    select(reef_area_km2, island_area_km2, POP_STATUS, SITE_SLOPE_400m_c, wave_energy_mean_kw_m1, 
           ted_sum, ted_mean, mld, mld_sd, months_ime, cv_chl, mean_ime_percent),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal()  # Apply a clean theme

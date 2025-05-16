library(brms)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)


source('00_crep_load.R')

# read ime and change island names
ime<-read.csv(file = 'island_ime_dat.csv') %>% 
  mutate(island2 = trimws(str_replace_all(island, 'Atoll', '')),
         island2 = trimws(str_replace_all(island2, 'Island', '')),
         island2 = trimws(str_replace_all(island2, 'Reef', '')),
         island2 = trimws(str_replace_all(island2, '\\ and', '\\ &')),
         island2 = case_match(island2, 
                              'Hawai’i' ~ 'Hawaii',
                              'French Frigate Shoals' ~ 'French Frigate',
                              'Kaua’i' ~ 'Kauai',
                              'Ni’ihau' ~ 'Niihau',
                              'Swains  (Olohega)' ~ 'Swains',
                              'Ta’u' ~ 'Tau', .default = island2))

ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  mutate(
         island2 = trimws(str_replace_all(island, 'Atoll', '')),
         island2 = trimws(str_replace_all(island2, 'Island', '')),
         island2 = trimws(str_replace_all(island2, 'Reef', '')),
         island2 = trimws(str_replace_all(island2, '\\ and', '\\ &')),
         island2 = case_match(island2, 
                              'Hawai’i' ~ 'Hawaii',
                              'French Frigate Shoals' ~ 'French Frigate',
                              'Kaua’i' ~ 'Kauai',
                              'Ni’ihau' ~ 'Niihau',
                              'Swains  (Olohega)' ~ 'Swains',
                              'Ta’u' ~ 'Tau', .default = island2))

# 6 missing islands in depth
unique(depth$ISLAND[!depth$ISLAND %in% ime$island2]) 

pdf(file = 'fig/ime_db/noaa_months_max_chl.pdf', height=5, width=7)
ime %>% filter(island2 %in% depth$ISLAND) %>% 
  ggplot(aes(months_ime, chl_island, col=lat_neg*-1)) + 
  geom_point(alpha=1) +
  geom_text_repel(aes(label=island2), size=2) +
  labs(x = 'Number of months IME present', y = 'Climatology: mean maximum chl-a, mg/m3') +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_color_gradientn(colors = rev(c("#043061", "#4475B4",'#FE9928', "#B2182B", "#670A1F")), name = 'Dist.to\nEquator')
dev.off()

depth_ime<-depth %>% 
  filter(ISLAND %in% ime$island2) %>% 
  mutate(island2 = ISLAND,
         month = as.numeric(str_split_fixed(DATE_, '\\/', 3)[,2])) %>% 
  left_join(ime, by = 'island2') %>% 
  left_join(ime_month %>% mutate(month = month_num, max_chl_month = Chl_max, ime_on = ifelse(is.na(keep_IME), 0, 1)) %>% 
              select(island2, month, max_chl_month, ime_on))

depth_ime_scaled <- depth_ime %>% 
  mutate(ime_on = factor(ime_on)) %>% 
  mutate(across(c(DEPTH_c, SITE_SLOPE_400m_c, mean_ime_percent, chl_island, max_chl_month, months_ime), 
                ~scale(., center=TRUE, scale=TRUE)))

# 29 islands, excluding 6 islands, mostly large MHI or Marianas

# From Richardson sup mat 
fix<- ~ DEPTH_c +
  POP_STATUS +
  SITE_SLOPE_400m_c +
  # DEPTH_c:POP_STATUS +
  #s(DEPTH_c, by=POP_STATUS) +
  # s(DEPTH_c, SITE_SLOPE_400m_c)
  (1|OBS_YEAR) +
  # (1|OBS_YEAR:ECOREGION) +
  # (1|OBS_YEAR:ISLAND) +
  (1|ISLAND) +
  # (1|ECOREGION)  +
  (1|SITE) 
  # (1|DIVER)
  # (1+DEPTH_c||ISLAND)

fix2 <- ~DEPTH_c +
  POP_STATUS +
  SITE_SLOPE_400m_c + 
  chl_island + ## island avg max nearby chl-a
  # max_chl_month + ## survey aligned nearby chl-a [monthly] [correlated with chl_island]
  months_ime + ## duration of IME in months
  cv_chl + ## annual variation in nearby chl-a
  ime_on + ## was the IME pumping during fish survey
  mean_ime_percent + ## average increase in chl-a during IME months [relative to non-IME REF]
  (1|OBS_YEAR) +
  (1|ISLAND) +
  (1|SITE) 

# with Richardson covariates
pos_form <- formula(paste(c('PLANKTIVORE', fix), collapse = " ")) # Formula for models with positive data only
bin_form <- formula(paste(c('hu', fix), collapse = " "))

# with IME covariates
pos_form2 <- formula(paste(c('PLANKTIVORE', fix2), collapse = " ")) # Formula for models with positive data only
bin_form2 <- formula(paste(c('hu', fix2), collapse = " "))


m1<-brm(bf(pos_form, bin_form), family = hurdle_gamma, data = depth_ime_scaled,
    chains = 3, iter = 2000, warmup = 500, cores = 4)

m1_ime<-brm(bf(pos_form2, bin_form2), family = hurdle_gamma, data = depth_ime_scaled,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m1_ime, file = 'results/mod_ime_planktivore.rds')
save(m1, file = 'results/mod_depth_planktivore.rds')

summary(m1_ime)
loo(m1, m1_ime)
conditional_effects(m1_ime)

# Extract posterior draws
effects <- m1_ime %>%
  spread_draws(b_DEPTH_c, b_POP_STATUSU, b_SITE_SLOPE_400m_c, b_chl_island, b_months_ime, b_cv_chl) %>%  
  pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Effect")

# Plot effect sizes
ggplot(effects, aes(x = Effect, y = Variable)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  theme_minimal()

# Multipanel of conditional effects
pdf(file = 'fig/ime_crep/ime_model_planktivore.pdf', height=7, width=12)
ce<-plot(conditional_effects(m1_ime), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()

# Create pairs plot for IME ~ CREP covariates
GGally::ggpairs(
  depth_ime %>% 
    select(SITE_SLOPE_400m_c, chl_island, max_chl_month, months_ime, cv_chl, ime_on, mean_ime_percent),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal()  # Apply a clean theme

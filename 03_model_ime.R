source('00_plot_theme.R')

ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) 

ime_month_scaled<-ime_month %>% 
  filter(!is.na(Chl_increase_nearby)) %>% 
  mutate(type = factor(type), reef_area_km2 = log10(reef_area_km2+1), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, Chl_max, month_num), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

ime_island<-read.csv(file = 'island_ime_dat.csv') %>% 
  mutate(type = factor(type), reef_area_km2 = log10(reef_area_km2+1), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, chl_island, months_ime, mean_ime_percent), 
                ~scale(., center=TRUE, scale=TRUE)[,1]))

# basic model fitting Chl increase (%) by month / island and biophysical covariates
m<-brm(Chl_increase_nearby ~ type + island_area_km2 + reef_area_km2 + Chl_max +
         s(month_num, bs = 'cc') +
         (1 | island) + s(lat,lon), 
       family = lognormal, data = ime_month_scaled,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m, file = 'results/mod_ime_attributes.rds')

summary(m)
conditional_effects(m)

m2<-brm(ime_diff ~ type + island_area_km2 + reef_area_km2 + chl_island + mean_ime_percent + months_ime +
         (1 | island) + s(lat,lon), data = ime_island,
        family = student(),
       chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2, file = 'results/mod_ime_reef_attributes.rds')

summary(m2)
conditional_effects(m2)

hist(island_ime$ime_diff)

# Create pairs plot for IME covariates
GGally::ggpairs(
  ime_month %>% 
    select(type, island_area_km2, reef_area_km2),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal() 

GGally::ggpairs(
  ime_island %>% 
    select(type, island_area_km2, reef_area_km2, chl_island, months_ime, mean_ime_percent),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal() 

# Multipanel of conditional effects
pdf(file = 'fig/ime_db/ime_model_chl_increase_pct.pdf', height=7, width=12)
ce<-plot(conditional_effects(m), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()

pdf(file = 'fig/ime_db/ime_model_ime_vs_non_ime.pdf', height=7, width=12)
ce<-plot(conditional_effects(m2), plot=FALSE)
do.call(gridExtra::grid.arrange, c(ce, ncol = 2))
dev.off()


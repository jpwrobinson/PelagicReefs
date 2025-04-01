library(brms)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)


ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) 

ime_month_scaled<-ime_month %>% 
  filter(!is.na(Chl_increase_nearby)) %>% 
  mutate(type = factor(type), reef_area_km2 = log10(reef_area_km2+1), island_area_km2 = log10(island_area_km2+1)) %>% 
  mutate(across(c(island_area_km2, reef_area_km2, month_num), 
                ~scale(., center=TRUE, scale=TRUE)))


# basic model fitting Chl increase (%) by month / island and biophysical covariates
m<-brm(Chl_increase_nearby ~ type + island_area_km2 + reef_area_km2 + s(month_num, bs = 'cc') +
         (1 | island) + s(lat,lon), 
       family = lognormal, data = ime_month_scaled,
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m, file = 'results/mod_ime_attributes.rds')

summary(m)
conditional_effects(m)



# Create pairs plot for IME covariates
GGally::ggpairs(
  ime_month %>% 
    select(type, island_area_km2, reef_area_km2),
  lower = list(continuous = wrap("points", alpha = 0.5, size=.5)),  # Scatterplots in lower panels
  diag = list(continuous = wrap("barDiag", bins = 20)),    # Histograms on the diagonal
  upper = list(continuous = wrap("cor", size = 3))         # Correlations in upper panels
) +
  theme_minimal()  # Apply a clean theme

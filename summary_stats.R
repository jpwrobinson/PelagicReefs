source('0_loads/00_plot_theme.R')
source('0_loads/00_ime_dataframe.R')

## Fig 1 - IME dataset
focs<-c('Necker', "Sarigan", 'Guam', 'Jarvis', 'Kauai', 'Agrihan', 'Lisianski')

dat_month %>% filter(island %in% focs) %>% group_by(island) %>%  reframe(range(Chl_increase_nearby, na.rm=T))
dat_month %>% filter(island %in% focs) %>% group_by(island) %>%  summarise(month[which.max(Chl_increase_nearby)])

ime_dat %>% reframe(range(median_chl_percent, na.rm=T))
ime_dat %>% group_by(REGION) %>% filter(!is.na(avg_monthly_mm)) %>% reframe(range(avg_monthly_mm))
ime_dat %>% filter(is.na(avg_monthly_mm)) %>% distinct(island)


# Fig 2 - posterior slopes
source('Figure2.R')

plot_data %>% group_by(lab) %>% slice_max(estimate__)
plot_data %>% group_by(lab) %>% slice_min(estimate__)

mld_pred %>% slice_max(estimate__)
mld_pred %>% slice_min(estimate__)

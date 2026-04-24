load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')


## Effect sizes
effects <- rbind(
  m2_plank %>%
    gather_draws(`b_.*`, regex=TRUE) %>%  
    mutate(fg = 'Planktivore'),
  m2_herb %>%
    gather_draws(`b_.*`, regex=TRUE) %>%  
    mutate(fg = 'Herbivore')) %>% 
  filter(.variable != 'Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2','site_bathy_400m',
                                         'hard_coral', 'depth_m',
                                         'avg_monthly_mm','mld_amp'
                          )))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value))) 

# What is the expected change in MLD at each island and 
#  corresponding expected change in plank flux?\
# also see 04A_model_plank.R for emmeans version
load(file = 'results/mld_anomaly_time_mod.rds')
df2<-expand.grid(island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df2$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))
df2$MLD_pred<-predict(m2, newdata = df2, type='response')
diffs<-df2 %>% group_by(island) %>% summarise(mld_pred_change = diff(range(MLD_pred)))

# plankton flux effect per year
eff<-effects %>% filter(.variable == 'mld_amp') %>% 
  group_by(fg) %>% 
  summarise(med = median(.value))

b_mld<-eff$med[eff$fg=='Planktivore']/sd(plank$mld_amp) # -0.04 per m MLD


# Extract posterior samples - flux at each island, given geomorphic but excluding site habitat
nd<-plank_scaled %>%  
  group_by(region.col, region, island, geomorphic_type, mld_amp, avg_monthly_mm, reef_area_km2, island_area_km2) %>% 
  summarise(site_bathy_400m = mean(site_bathy_400m)) %>% 
  mutate(depth_m = 0, hard_coral = 0) %>%
  add_epred_draws(m2_plank, ndraws = 1000, re_formula = NA) %>% 
  group_by(island, region, region.col) %>% summarise(base_flux = median(.epred)) %>% 
  left_join(diffs) %>% ungroup() %>% 
  mutate(flux_diff = mld_pred_change * b_mld, 
         new_flux = base_flux + flux_diff,
         flux_diff_prop = flux_diff / base_flux)

g1<-ggplot(nd, aes(fct_reorder(island, flux_diff_prop), flux_diff_prop, col=region.col)) + 
  geom_point() +
  coord_flip() +
  scale_color_identity() +
  scale_y_continuous(labels=scales::label_percent()) +
  labs(y = 'Change in planktivore flux with MLD', x = '')

pdf(file = 'fig/FigureSX_MLD_to_flux.pdf', height=7, width=5)
g1
dev.off()


ggplot(nd, aes(fct_reorder(region, flux_diff_prop), flux_diff_prop, col=region.col)) + 
  geom_boxplot() +
  geom_point()

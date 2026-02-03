
# Trophic group effect plots
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
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth',
                                         'mld_amp', 'chl_a_mg_m3_mean')))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value)))


library(tidybayes)

# partial residual estimates - the observed value minus prediction with model excluding focal covariate
# note this refits model with update
pr <- plank_scaled %>%
  add_epred_draws(m2_plank) %>%             # expected value on data scale
  add_epred_draws(update(m2_plank, . ~ . - mld_amp, init=0, control = list(adapt_delta = 0.99)),
                  .draw = "draw2",
                  value = "epred_nox")

pr <- pr %>%
  mutate(partial = planktivore_metab - epred_nox)

ggplot(pr, aes(mld_amp, partial)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "loess") +
  labs(y = "Partial residual (x)")



# Plot effect sizes
pdf(file = 'fig/Figure2.pdf', height=3, width=6)
ggplot(effects %>% filter(!var_fac=='Intercept'), aes(x = .value, y = var_fac, col = fg)) +
  stat_pointinterval(.width = c(0.5, 0.95), pch=19, 
                     position = position_dodge(width=0.65)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  scale_color_manual(values = fg_cols) +
  labs(x = "Effect on metabolic flux", y = "") +
  xlim(c(-1, 0.75)) +
  guides(color='none') +
  scale_y_discrete(labels = c('Mixed layer depth', 'Depth', 'Hard coral',
                              'Bathymetric slope', 'Precipitation', 'Island area', 'Reef area'))
dev.off()

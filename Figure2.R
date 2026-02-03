## CREP metabolic
source('loads/00_crep_metabolic.R')

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


## Planktivore flux
plank_mean_is<-plank_scaled %>% group_by(region.col, region, island) %>% summarise(planktivore_metab = median(planktivore_metab),
                                                                                   herbivore_metab = median(herbivore_metab))

ggplot(plank_mean_is) + 
  aes(x = fct_reorder(island, planktivore_metab), y = planktivore_metab, fill = region.col) +
  geom_jitter(data = plank_scaled, width = 0.15, alpha = 0.2, aes(col=region.col)) +
  geom_point(size = 2.5, pch=21, col='black') +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_y_log10(
    minor_breaks = NULL,
    labels=scales::trans_format("log10", scales::math_format(10^.x)),
    sec.axis = dup_axis(labels=scales::trans_format("log10", scales::math_format(10^.x)))
  ) +
  coord_flip() +
  labs(x = '', y=expression(Planktivore~metabolic~rates~(kJ~m^-2~d^-1))) +
  theme(legend.position= 'none')





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


# Trophic group effect plots
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

bayes<-data.frame(b = c(bayes_R2(m2_plank)[1,'Estimate'],
                        bayes_R2(m2_herb)[1,'Estimate']),
                  x = 0.9, y = c(6.5, 6.5), fg = c('Planktivore', 'Herbivore'))

direc<-data.frame(b = c('Wetter', 'Deeper'),
                  x = c(0.3, -0.5), 
                  y = c(3.3, 4.3), fg = c('Herbivore', 'Planktivore'))

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
                          levels = rev(c('population_statusU', 'geomorphic_typeIsland','reef_area_km2','island_area_km2','site_bathy_400m', # geomorphic
                                        'ted_mean', 'mld_mean', 'avg_monthly_mm', #oceanographic
                                         'hard_coral', 'depth_m' #habitat
                                         )))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac, fg) %>% mutate(medi = abs(median(.value)),
                               hpdi_lower = HDInterval::hdi(.value, credMass = 0.95)[1],
                               hpdi_upper = HDInterval::hdi(.value, credMass = 0.95)[2],
                               median = median(.value),
                               excludes_zero = !(hpdi_lower < 0 & hpdi_upper > 0)) 


# Plot effect sizes
labs<-data.frame(x = Inf, y = c(2.4, 4.4, 8.4), 
                 label = c('Habitat', 'Oceanographic', 'Geomorphic'), 
                 fg='Herbivore')

gA<-ggplot(effects, aes(x = .value, y = var_fac, col = fg)) +
    # geom_text(data = labs, aes(x, y, label = label), size=3.5, fontface=1, hjust=1, col='black') +
    # annotate('rect', xmin = -Inf, xmax=Inf, ymin = -Inf, ymax = 2.5, fill='grey', alpha=0.1) +
    # annotate('rect', xmin = -Inf, xmax=Inf, ymin = 4.5, ymax = 8.5, fill='grey', alpha=0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
    stat_pointinterval(data = effects, .width = c(0.5, 0.95), pch=19, 
                       position = position_dodge(width=0.65)) +  
    stat_slabinterval(data = effects %>% filter(excludes_zero == TRUE), .width = c(0.5, 0.95), pch=19, 
                     position = position_dodge(width=0.65)) +  
    geom_text(data = bayes, aes(x = x, y = y, label = paste0('R² = ', round(b*100,1),'% ')), size=2.5) +
    geom_text(data = direc, aes(x = x, y = y, label = b), size=2.5) +
    facet_grid(~fct_rev(fg)) +
    scale_color_manual(values = fg_cols) +
    labs(x = "Effect on metabolic flux", y = "") +
    scale_x_continuous(limits=c(-.9, 1.3), expand=c(0,0)) +
    guides(color='none') +
    scale_y_discrete(labels = c('Depth', 'Hard coral',
                                'Precipitation', 'Mixed layer depth','Tidal energy',
                                'Bathymetric slope','Island area', 'Reef area', 'Island [vs. atoll]', 'Unpopulated'), 
                     sec.axis = dup_axis(labels=NULL)) +
    theme(strip.text = element_text(face=2, hjust=0, size=11),
          strip.background = element_blank(),
          axis.text.y = element_text(size =10),
          axis.text.x = element_text(size =10))


# source('fig3C_fish_var_exp.R')

pdf(file = 'fig/Figure4.pdf', height=4, width=7.5)
print(gA)
# print(
#   plot_grid(gA, 
#             gC + theme(plot.margin = unit(c(.9,.19, .19, -.5), 'cm')), 
#             nrow=1, labels=c('a', 'b'), align='hv', rel_widths=c(1, 0.5))
# )
dev.off()


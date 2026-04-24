
# Trophic group effect plots
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

bayes<-data.frame(b = c(bayes_R2(m2_plank)[1,'Estimate'],
                        bayes_R2(m2_herb)[1,'Estimate']),
                  x = 0.5, y = c(6.5, 6.5), fg = c('Planktivore', 'Herbivore'))

direc<-data.frame(b = c('Wetter', 'Drier', 'Deeper', 'Shallower'),
                  x = c(0.2, -0.2,0.2, -0.2), 
                  y = c(2.3, 2.3, 1.3, 1.3), fg = 'Herbivore')

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


# Plot effect sizes
labs<-data.frame(x = Inf, y = c(2.4, 4.4, 8.4), label = c('Habitat', 'Oceanographic', 'Geomorphic'), fg='Herbivore')

gA<-ggplot(effects, aes(x = .value, y = var_fac, col = fg)) +
    # geom_text(data = labs, aes(x, y, label = label), size=3.5, fontface=1, hjust=1, col='black') +
    annotate('rect', xmin = -Inf, xmax=Inf, ymin = -Inf, ymax = 2.5, fill='grey', alpha=0.1) +
    annotate('rect', xmin = -Inf, xmax=Inf, ymin = 4.5, ymax = Inf, fill='grey', alpha=0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
    stat_pointinterval(.width = c(0.5, 0.95), pch=19, 
                       position = position_dodge(width=0.65)) +  
    geom_text(data = bayes, aes(x = x, y = y, label = paste0('R² = ', round(b*100,1),'% ')), size=2.5) +
    geom_text(data = direc, aes(x = x, y = y, label = b), size=2.5) +
    facet_grid(~fg) +
    scale_color_manual(values = fg_cols) +
    labs(x = "Effect on metabolic flux", y = "") +
    scale_x_continuous(limits=c(-.9, 0.7), expand=c(0,0)) +
    guides(color='none') +
    scale_y_discrete(labels = c('Mixed layer depth', 'Precipitation', 
                                'Depth', 'Hard coral',
                                'Bathymetric slope','Island area', 'Reef area', 'Island'), 
                     sec.axis = dup_axis(labels=NULL)) +
    theme(strip.text = element_text(face=2, hjust=0, size=11),
          strip.background = element_blank(),
          axis.text.y = element_text(size =10),
          axis.text.x = element_text(size =10))


source('fig3C_fish_var_exp.R')

pdf(file = 'fig/Figure4.pdf', height=3.5, width=9)
print(
  plot_grid(gA, 
            gC + theme(plot.margin = unit(c(.9,.19, .19, -.5), 'cm')), 
            nrow=1, labels=c('a', 'b'), align='hv', rel_widths=c(1, 0.5))
)
dev.off()


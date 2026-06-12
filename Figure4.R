
# Trophic group effect plots
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

bayes<-data.frame(b = c(bayes_R2(m2_plank, re.form=NA)[1,'Estimate'],
                        bayes_R2(m2_herb, re.form=NA)[1,'Estimate']),
                  x = 0.9, y = 11, fg = c('Planktivore', 'Herbivore'))

direc<-data.frame(b = c('Wetter', 'Deeper'),
                  x = c(0.5, -0.6), 
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
gA<-ggplot(effects, aes(x = .value, y = var_fac, col = fg)) +
    # geom_text(data = labs, aes(x, y, label = label), size=3.5, fontface=1, hjust=1, col='black') +
    # annotate('rect', xmin = -Inf, xmax=Inf, ymin = -Inf, ymax = 2.5, fill='grey', alpha=0.1) +
    # annotate('rect', xmin = -Inf, xmax=Inf, ymin = 4.5, ymax = 8.5, fill='grey', alpha=0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
    stat_pointinterval(data = effects, .width = c(0.5, 0.95), pch=19, alpha=0.5,
                       position = position_dodge(width=0.65)) +  
    stat_pointinterval(data = effects %>% filter(excludes_zero == TRUE), 
                      .width = c(0.5, 0.95), pch=19, 
                     position = position_dodge(width=0.65)) +  
    geom_text(data = bayes, aes(x = x, y = y, label = paste0('R² = ', round(b*100,1),'% ')), size=3.5) +
    geom_text(data = direc, aes(x = x, y = y, label = b), size=2.5) +
    facet_grid(~fct_rev(fg)) +
    scale_color_manual(values = fg_cols) +
    labs(x = "Effect on fish-assemblage respiration rate", y = "") +
    scale_x_continuous(limits=c(-.9, 1.3), expand=c(0,0)) +
    guides(color='none') +
    coord_cartesian(clip='off') +
    scale_y_discrete(labels = c('Depth', 'Hard coral',
                                'Precipitation', 'Mixed layer depth','Tidal energy',
                                'Bathymetric slope','Island area', 'Reef area', 'Island [vs. atoll]', 'Unpopulated'), 
                     sec.axis = dup_axis(labels=NULL)) +
    theme(strip.text = element_text(face=1, hjust=0, vjust=3, size=10),
          strip.background = element_blank(),
          axis.text.y = element_text(size =10),
          axis.text.x = element_text(size =10)) 

## SHAP panels
th<-theme(legend.position='none',
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          strip.text = element_text(face=2, hjust=0, size=11),
          strip.background = element_blank(),
          axis.text.x = element_text(size=10),
          plot.margin = unit(c(0.1, 1.75, 0, .5), 'cm'))

labs<-data.frame(label = c('Depth', 'Hard coral',
                 'Precipitation', 'Mixed layer depth','Tidal energy',
                 'Bathymetric slope','Island area', 'Reef area', 'Island [vs. atoll]', 'Unpopulated'),
                 feature = c('depth_m','hard_coral',
                   'avg_monthly_mm', 'mld_mean','ted_mean',
                   'site_bathy_400m','island_area_km2','reef_area_km2', 'geomorphic_type',
                   'population_status'))

sigs<-effects %>% mutate(response = fg, 
                         feature = str_replace_all(.variable, 'Island', ''),
                         feature = str_replace_all(feature, 'statusU', 'status')) %>% 
  distinct(response, feature, excludes_zero)

vp_by_var<-read.csv('results/fish_SHAP_byvar.csv') %>% 
  left_join(labs) %>% 
  left_join(sigs)


gB1<-ggplot(vp_by_var %>% filter(response=='Planktivore') %>% 
              mutate(feature = fct_reorder(feature, mean_abs_shap)),
                                 aes(x = mean_abs_shap, y = feature, alpha=excludes_zero)) +
  geom_col(fill=fg_cols[2]) +
  geom_text(aes(label = label), hjust = -0.05, size=3) +
  scale_x_continuous(expand=c(0,0), limits=c(0, 0.65)) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_cartesian(clip='off') +
  labs(x = "", y = NULL, fill  = NULL) + th

gB2<-ggplot(vp_by_var %>% filter(response=='Herbivore') %>% 
              mutate(feature = fct_reorder(feature, mean_abs_shap)),
            aes(x = mean_abs_shap, y = feature, alpha=excludes_zero)) +
  geom_col(fill=fg_cols[1]) +
  geom_text(aes(label = label), hjust = -0.05, size=3) +
  scale_x_continuous(expand=c(0,0), limits=c(0, 0.65)) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_cartesian(clip='off') +
  labs(x  = "Variable importance (mean SHAP)", y = NULL, fill  = NULL) + th

gB<-plot_grid(gB1, gB2, nrow=2)

pdf(file = 'fig/Figure4.pdf', height=4.5, width=10)
print(
  plot_grid(gA + theme(plot.margin=unit(c(1.15, 0, 1.25, 0), 'cm')),
            gB,
            nrow=1, labels=c('a', 'b'), align='hv', rel_widths=c(1, 1))
)
dev.off()

# fish icons
pdf(file = 'fig/Fig_FishIcon.pdf', height=4.5, width=10)

ggplot() + add_fishape(family = 'Caesionidae', option = "Caesio_cuning",
            fill = fg_cols[2])  +
  theme_void() + labs(subtitle = 'Caesio cuning')

ggplot() + add_fishape(family = 'Acanthuridae', option = "Naso_unicornis",
                       fill = fg_cols[1])  +
  theme_void() + labs(subtitle = 'Naso unicornis')

dev.off()

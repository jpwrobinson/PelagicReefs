library(tidybayes)

# Trophic group effect plots
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

bayes<-data.frame(b = c(bayes_R2(m2_plank)[1,'Estimate'],
                        bayes_R2(m2_herb)[1,'Estimate']),
                  x = 0.5, y = c(6.5, 6.5), fg = c('Planktivore', 'Herbivore'))

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
                                         'avg_monthly_mm','mld_amp', 
                                         'hard_coral', 'depth_m'
                                         )))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value))) 


# Plot effect sizes
labs<-data.frame(x = Inf, y = c(2.4, 4.4, 8.4), label = c('Habitat', 'Oceanographic', 'Geomorphic'), fg='Herbivore')

pdf(file = 'fig/Figure2.pdf', height=5, width=11)
print(
  ggplot(effects, aes(x = .value, y = var_fac, col = fg)) +
    geom_text(data = labs, aes(x, y, label = label), size=3.5, fontface=1, hjust=1, col='black') +
    annotate('rect', xmin = -Inf, xmax=Inf, ymin = -Inf, ymax = 2.5, fill='grey', alpha=0.1) +
    annotate('rect', xmin = -Inf, xmax=Inf, ymin = 4.5, ymax = Inf, fill='grey', alpha=0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
    stat_pointinterval(.width = c(0.5, 0.95), pch=19, 
                       position = position_dodge(width=0.65)) +  
    geom_text(data = bayes, aes(x = x, y = y, label = paste0('R2 = ', round(b*100,1),'% ')), size=4) +
    facet_grid(~fg) +
    scale_color_manual(values = fg_cols) +
    labs(x = "Effect on metabolic flux", y = "") +
    scale_x_continuous(limits=c(-.9, 0.7), expand=c(0,0)) +
    guides(color='none') +
    scale_y_discrete(labels = c('Depth', 'Hard coral',
                                'Mixed layer depth', 'Precipitation', 
                                'Bathymetric slope','Island area', 'Reef area', 'Geomorphic [island]'), sec.axis = dup_axis()) +
    theme(strip.text = element_text(face=2, hjust=0, size=11),
          strip.background = element_blank(),
          axis.text.y = element_text(size =10),
          axis.text.x = element_text(size =10))
)
dev.off()


# # partial residual estimates - the observed value minus prediction with model excluding focal covariate
# epred_no_nox <- posterior_linpred(
#   m2_plank,
#   newdata = plank_scaled,
#   re_formula = NA,
#   terms = ~ . - mld_amp      # remove MLD term
# )
# 
# plank_scaled<-plank_scaled %>% 
#   mutate(drop_mld = colMeans(epred_no_nox), partial = log(planktivore_metab) - colMeans(epred_no_nox))
# 
# ggplot(plank_scaled, aes(mld_amp, drop_mld)) +
#   geom_point(alpha = 0.4) +
#   labs(y = "Partial residual (x)")
# 





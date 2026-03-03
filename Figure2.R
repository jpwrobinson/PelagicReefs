library(tidybayes)

# Trophic group effect plots
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

bayes<-data.frame(b = c(bayes_R2(m2_plank)[1,'Estimate'],
                        bayes_R2(m2_herb)[1,'Estimate']),
                  x = 0.5, y = c(6.5, 6.1), fg = c('Planktivore', 'Herbivore'))

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
                                         'avg_monthly_mm', #'population_statusU',
                                         'site_bathy_400m','mld_amp', 'hard_coral', 'depth_m'
                                         )))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value)))


# Plot effect sizes
pdf(file = 'fig/Figure2.pdf', height=5, width=7)
print(
  ggplot(effects %>% filter(!var_fac=='Intercept'), aes(x = .value, y = var_fac, col = fg)) +
    stat_pointinterval(.width = c(0.5, 0.95), pch=19, 
                       position = position_dodge(width=0.65)) +  
    geom_text(data = bayes, aes(x = x, y = y, label = paste0('R2 = ', round(b*100,1),'% ')), size=4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
    scale_color_manual(values = fg_cols) +
    labs(x = "Effect on metabolic flux", y = "") +
    scale_x_continuous(limits=c(-.9, 0.7), expand=c(0,0)) +
    guides(color='none') +
    scale_y_discrete(labels = c('Depth', 'Hard coral','Mixed layer depth',
                                'Bathymetric slope', 'Precipitation', 'Island area', 'Reef area', 'Geomorphic [island]')) 
    # theme(panel.grid.major.x = element_line(color='grey')
          # panel.grid.major.y = element_line(color='grey'))
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

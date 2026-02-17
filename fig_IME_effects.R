
blanker<-data.frame(.chain = 1, .iteration = 1, .draw = 1, 
                    .variable = c('hard_coral', 'depth'), .value = -100,
                    var_fac = c('hard_coral', 'depth'))

# load in posterior objects
source('Figure1.R')
# edit order of covariates
levs<-c( 'hard_coral', 'depth', levels(effects$var_fac))

# join dummy covariates hc and depth
ime_effects<-rbind(effects, blanker) %>% 
  mutate(var_fac = factor(var_fac, levels = levs))



source('Figure2.R')
fish_effects<-effects %>% filter(!var_fac=='Intercept') %>% 
  mutate(var_fac = recode(var_fac, 'island_area_km2' = 'land_area_km2',
                          'chl_a_mg_m3_mean' = 'mean_chlorophyll',
                          'site_bathy_400m' = 'bathymetric_slope',
                          'mld_amp' = 'mld')) %>% 
  filter(!var_fac %in% c('population_statusU')) %>% 
  mutate(var_fac = factor(var_fac, levels = levs))

fish_effects %>% distinct(var_fac)

gL<-ggplot(ime_effects %>% filter(var_fac != 'ted_mean'),
           aes(x = .value, y = var_fac)) +
  # geom_text(data = bayes, aes(x = x, y = y, label = paste0('R2 = ', round(b*100,1),'% ')), size=2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  stat_pointinterval(.width = c(0.5, 0.95), position = position_dodge(width=0.5)) +  
  labs(x = "Effect on chl-a enhancement", y = "") +
  scale_colour_manual(values = c('#737373', '#d94801'), guide=NULL) +
  scale_x_continuous(limits=c(-.75, 1.25), sec.axis = dup_axis()) +
  scale_y_discrete(labels =c('Hard coral', 'Survey depth',
                             'Mixed layer depth', #'Tidal energy',
                              'Chlorophyll a',
                              'Precipitation', 'Bathymetric slope', 
                              'Island area', 'Reef area', 'Island'), position = 'right') +
  theme(legend.position = c(.9,.9), legend.title = element_blank())



gR<-ggplot(fish_effects %>% filter(fg == 'Planktivore'), aes(x = .value, y = var_fac, col = fg)) +
  stat_pointinterval(.width = c(0.5, 0.95), pch=19, 
                     position = position_dodge(width=0.65)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  scale_color_manual(values = fg_cols) +
  labs(x = "Effect on metabolic flux", y = "") +
  scale_x_continuous(limits=c(-.75, 1.25), sec.axis = dup_axis()) +
  guides(color='none') +
  scale_y_discrete(labels =c('Hard coral', 'Survey depth',
    'Mixed layer depth', #'Tidal energy',
                              'Chlorophyll a',
                              'Precipitation', 'Bathymetric slope', 
                              'Island area', 'Reef area', 'Island'), position = 'left') 

plot_grid(gL, gR, align = 'hv')

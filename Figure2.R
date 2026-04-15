
load(file = 'results/mod_ime.rds')
source('fig_IME_seasonal.R')

# Panel a = drivers of IME (monthly + spatial)
bayes<-data.frame(b = bayes_R2(m_chl_inc)[1,'Estimate'], x = 0.9, y = c(9.3))
# r2(m_chl_inc, by_component = TRUE) # 51% fixed effects. 65% full model.

# For each draw, compute group contribution as sum of |coefficient|
# weighted by predictor variance in the data
coef_draws <- as_draws_df(m_chl_inc) %>%
  select(starts_with("b")) %>% 
  mutate(
    geomorphic = abs(b_Chlincreasenearby_geomorphic_typeIsland) + abs(b_Chlincreasenearby_reef_area_km2) + abs(b_Chlincreasenearby_land_area_km2) +
      abs(b_Chlincreasenearby_bathymetric_slope),
    oceanographic = abs(b_Chlincreasenearby_mean_chlorophyll) + abs(b_Chlincreasenearby_mld_mean) + abs(bsp_Chlincreasenearby_mited_mean),
    seasonal = abs(b_Chlincreasenearby_avg_monthly_mm_anom) + abs(b_Chlincreasenearby_mld_anom),
    total = geomorphic + oceanographic + seasonal,
    prop_geomorphic    = geomorphic    / total,
    prop_oceanographic          = oceanographic          / total,
    prop_seasonal = seasonal / total
  ) %>%
  select(starts_with("prop_")) %>% 
  pivot_longer(everything(), names_to = "group",values_to = "proportion", names_prefix = "prop_")


coef_draws %>% group_by(group) %>% summarise(median_hdi(proportion, 0.95))

# gInset<-ggplot(coef_draws, aes(x = proportion, y = group, col=group)) +
#   geom_text(data = data.frame(group = c('seasonal', 'oceanographic', 'geomorphic'),
#                               proportion = c(0.28, 0.5, 0.7),
#                               lab = c('Seasonal', 'Oceanographic', 'Geomorphic')),
#             aes(label = lab, col=group), vjust=-1.5, fontface=2, size=2) +
#   stat_pointinterval(.width = c(0.5, 0.95)) +
#   # stat_slabinterval(.width = c(0.5, 0.95), point_interval = median_qi,                   
#   #                   slab_alpha = 0.5, interval_alpha = 1, point_alpha = 1) +
#   # geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
#   scale_x_continuous(labels = scales::percent, limits = c(0, 0.8), expand=c(0,0)) +
#   scale_y_discrete(expand = expansion(mult = 0.1)) +
#   labs(x = "explained variance", y = NULL) +
#   theme(legend.position = "none", 
#         axis.text.y = element_blank(), axis.line.y = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.title.x = element_text(size = 8),
#         axis.ticks.y = element_blank(),
#         panel.border = element_rect(color='black'),
#         panel.grid.major.x = element_line(color='grey'))

gEff<-ggplot(effects,
           aes(x = .value, y = var_fac)) +
  geom_text(data = bayes, aes(x = x, y = y, label = paste0('R² = ', round(b*100,1),'% ')), size=3.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  stat_pointinterval(.width = c(0.5, 0.95), position = position_dodge(width=0.5)) +  
  labs(x = "Effect on IME strength", y = "") +
  scale_colour_manual(values = c('#737373', '#d94801'), guide=NULL) +
  scale_x_continuous(limits=c(-.75, 1.25)) +
  scale_y_discrete(labels =c('Precipitation [anomaly]', 'Mixed layer depth [anomaly]',
                             'Mixed layer depth [mean]', 'Tidal energy','Chlorophyll-a [mean]', 
                             'Bathymetric slope', 'Island area', 'Reef area', 'Island [vs. atoll]')) +
  theme(legend.position = c(.9,.9), legend.title = element_blank(),
        plot.margin=unit(c(0,1,0.01, .1), 'cm'))

# gEff<-gEff + inset_element(gInset, 
#                        left = 0.6, bottom = 0.001, 
#                        right = 1.15, top = 0.5)

# Get conditional effects
source('func_mod_conditional.R')
bathy_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'bathymetric_slope')
isl_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'land_area_km2')
reef_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'reef_area_km2')

ted_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'ted_mean')
chl_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'mean_chlorophyll')
mld_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'mld_mean')

mldA_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'mld_anom')
precip_pred<-mod_post(mod = m_chl_inc, dat_raw = dat_month, var = 'avg_monthly_mm_anom')


dd<-rbind(bathy_pred %>% select(-bathymetric_slope) %>% mutate(var = 'Bathymetry', unit='slope~degree'), 
          isl_pred %>% select(-land_area_km2) %>% mutate(var = 'Land area', unit='km^2'),
          reef_pred %>% select(-reef_area_km2) %>% mutate(var = 'Reef area', unit="km^2"),
          ted_pred %>% select(-ted_mean) %>% mutate(var = 'Tidal energy flux', unit='W~m^2'),
          chl_pred %>% select(-mean_chlorophyll) %>% mutate(var = 'Mean chl-a', unit='mg~m^3'),
          mld_pred %>% select(-mld_mean) %>% mutate(var = 'Mixed layer depth ', unit='m') , 
          mldA_pred %>% select(-mld_anom) %>% mutate(var = 'Mixed layer depth anomaly', unit='m') , 
          precip_pred %>% select(-avg_monthly_mm_anom) %>% mutate(var = 'Precipitation anomaly',unit='mm')) %>% 
  mutate(g = ifelse(var %in% c('Bathymetry', 'Land area', 'Reef area'), 0, 1),
         g = ifelse(var %in% c('Mixed layer depth anomaly', 'Precipitation anomaly'), 2, g))

labels<-dd %>% distinct(var, unit)

gGeo<-ggplot(dd %>% filter(g == 0)) + 
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100), alpha=0.1) +
  geom_line(aes(raw, estimate__/100)) + 
  geom_text(data = dd %>% filter(g == 0) %>% distinct(var), 
            aes(y = Inf,x = -Inf, label = var), size = 3, vjust=2, hjust=-.05) +
  guides(fill = 'none') +
  labs(y = 'IME strength', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free', switch = 'x', 
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank(),
        strip.text = element_text(vjust=1.5))

gOce<-ggplot(dd %>% filter(g == 1)) + 
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100), alpha=0.1) +
  geom_line(aes(raw, estimate__/100)) + 
  geom_text(data = dd %>% filter(g == 1) %>% distinct(var), 
            aes(y = Inf,x = -Inf, label = var), size = 3, vjust=2, hjust=-.05) +
  guides(fill = 'none') +
  labs(y = 'IME strength', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free', switch = 'x',
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank(),
        strip.text = element_text(vjust=1.5)) 

gSea<-ggplot(dd %>% filter(g == 2) %>% 
               mutate(lab = paste0(var, ', ', unit))) + 
  geom_vline(xintercept = 0, linetype=2, col='black') +
  geom_text(data = data.frame(raw = c(-4,4), estimate = Inf, label = c('Shallower', 'Deeper'), lab = 'Mixed layer depth anomaly, m'),
            aes(x = raw, y = estimate, label = label), size = 3, vjust=2, hjust=c(1,0)) +
  geom_text(data = data.frame(raw = c(-50,50), estimate = Inf, label = c('Drier', 'Wetter'), lab = 'Precipitation anomaly, mm'),
            aes(x = raw, y = estimate, label = label), size = 3, vjust=2, hjust=c(1, 0)) +
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100, 
                  fill = estimate__/100
                  ), alpha=0.9) +
  geom_line(aes(raw, estimate__/100), linewidth=1.4) + 
  guides(fill = 'none') +
  labs(y = 'IME strength', x = '') +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_gradientn(
    colors = chl_grad_cols,
    labels = label_percent(),
    limits=c(0, .25),
    name = 'IME strength') +
  facet_grid(~lab, scales='free', switch = 'x',
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank(),
        plot.margin=unit(c(.5,1,0.01, .1), 'cm')) 


pdf(file = 'fig/Figure2.pdf', height=5, width=11)
lh<-plot_grid(gEff, gSea, nrow=2, labels=c('a', 'b'), rel_heights=c(1, 1))
plot_grid(lh, 
          gMLD + scale_y_discrete(position = 'right', limits=rev(island_order)) + 
            labs(subtitle = 'IME seasonality predicted by mixed layer depth') +
            theme(plot.subtitle = element_text(hjust=0.5, vjust=-1, size=10)), labels=c('', 'c'))
dev.off()

pdf(file = 'fig/FigureSX_IME_pred.pdf', height=4, width=6)
plot_grid(gGeo, gOce, nrow=2, rel_heights=c(1, 1))
dev.off()

pdf(file = 'fig/FigureSX_IME_seasonal_pred.pdf', height=9, width=6)
plot_grid(gMLD, gPrecip, gCombo, nrow=3, labels=c('a', 'b', 'c'))
dev.off()

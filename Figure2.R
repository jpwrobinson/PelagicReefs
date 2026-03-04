
load(file = 'results/mod_ime.rds')

# Panel C = drivers of IME [monthly and time-averaged]
bayes<-data.frame(b = bayes_R2(m_chl_inc)[1,'Estimate'], x = 1, y = c(9.3))
# r2(m_chl_inc, by_component = TRUE) # 51% fixed effects. 65% full model.

gD<-ggplot(effects,
           aes(x = .value, y = var_fac)) +
  geom_text(data = bayes, aes(x = x, y = y, label = paste0('R2 = ', round(b*100,1),'% ')), size=2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  stat_pointinterval(.width = c(0.5, 0.95), position = position_dodge(width=0.5)) +  
  labs(x = "Effect on chl-a enhancement", y = "") +
  scale_colour_manual(values = c('#737373', '#d94801'), guide=NULL) +
  scale_x_continuous(limits=c(-.75, 1.25)) +
  scale_y_discrete(labels =c('Monthly precipitation', 'Monthly mixed layer depth',
                             'Mean mixed layer depth', 'Tidal energy','Chlorophyll a', 
                             'Bathymetric slope', 'Island area', 'Reef area', 'Island')) +
  theme(legend.position = c(.9,.9), legend.title = element_blank())

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
          mld_pred %>% select(-mld_mean) %>% mutate(var = 'Mixed layer depth', unit='m') , 
          mldA_pred %>% select(-mld_anom) %>% mutate(var = 'Mixed layer depth ', unit='m') , 
          precip_pred %>% select(-avg_monthly_mm_anom) %>% mutate(var = 'Precipitation ',unit='mm')) %>% 
  mutate(g = ifelse(var %in% c('Bathymetry', 'Land area', 'Reef area'), 0, 1),
         g = ifelse(var %in% c('Mixed layer depth ', 'Precipitation '), 2, g))

labels<-dd %>% distinct(var, unit)

gE<-ggplot(dd %>% filter(g == 0)) + 
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100), alpha=0.1) +
  geom_line(aes(raw, estimate__/100)) + 
  geom_text(data = dd %>% filter(g == 0) %>% distinct(var), 
            aes(y = Inf,x = -Inf, label = var), size = 3, vjust=2, hjust=-.05) +
  guides(fill = 'none') +
  labs(y = 'chl-a enhancement', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free', switch = 'x', 
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank())

gF<-ggplot(dd %>% filter(g == 1)) + 
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100), alpha=0.1) +
  geom_line(aes(raw, estimate__/100)) + 
  geom_text(data = dd %>% filter(g == 1) %>% distinct(var), 
            aes(y = Inf,x = -Inf, label = var), size = 3, vjust=2, hjust=-.05) +
  guides(fill = 'none') +
  labs(y = '', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free', switch = 'x',
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank()) 

gG<-ggplot(dd %>% filter(g == 2)) + 
  geom_ribbon(aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100), alpha=0.1) +
  geom_line(aes(raw, estimate__/100)) + 
  geom_text(data = dd %>% filter(g == 2) %>% distinct(var), 
            aes(y = Inf,x = -Inf, label = var), size = 3, vjust=2, hjust=-.05) +
  guides(fill = 'none') +
  labs(y = '', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free', switch = 'x',
             labeller = labeller(var = as_labeller(setNames(labels$unit, labels$var), label_parsed))) +
  theme(strip.placement = 'bottom', strip.background = element_blank()) 


pdf(file = 'fig/Figure3.pdf', height=5.5, width=12)
top<-plot_grid(gD, gE, nrow=1, labels=c('a', 'b'), rel_widths=c(1, 1))
bot<-plot_grid(gF, gG, nrow=1, labels=c('a', 'b'), rel_widths=c(1, 1.2, 1))
print(
  plot_grid(top, bot, nrow=2)
)
dev.off()


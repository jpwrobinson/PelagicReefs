# Load Messi IME and Gove drivers with statistical model objects
source('0_loads/00_ime_dataframe.R')
load(file = 'results/mod_ime.rds')

# Panel a = island seasonal IME, 3 panels
gA<-ggplot(dat_month, aes(Chl_increase_nearby/100)) + 
  geom_histogram(col='black', fill='grey50', binwidth=0.1) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0), labels=label_percent()) +
  labs(y = 'Number of island-months', x = 'chl-a enhancement')


# Panel b = island seasonal IME, 3 panels
# fac_level<-levels(with(dat, fct_reorder(island, -median_chl_percent)))
# dat_month$island_fac<-factor(dat_month$island, levels=fac_level)

focs<-c('Laysan', 'Kauai', 'Jarvis', 'Palmyra', 'Agrihan', 'Guam')
pp<-dat_month %>% filter(island %in% focs) %>% 
  mutate(island_fac = factor(island, levels=focs))

gC<-ggplot(pp, aes(month_num, Chl_increase_nearby/100, col=region.col,fill=region.col, group=island)) + 
    geom_area(alpha=0.5) + 
    # geom_text(data = dat_month %>% filter(island %in% focs) %>% slice_max(Chl_increase_nearby), size=2, vjust=3, aes(label=island)) +
    theme(legend.position = 'none', plot.margin=unit( c(0,0,0,0), 'cm')) +
    labs(x = '', y = 'chl-a enhancement') +
    facet_wrap(~island_fac, scales='fixed', nrow=1) +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(expand=c(0,0), labels = label_percent()) +
    scale_x_continuous(breaks=c(1,  6, 11), labels=month.abb[c(1, 6, 11)]) +
    theme(strip.background = element_blank(), strip.text = element_text(hjust=0))
  
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


pdf(file = 'fig/Figure1.pdf', height=5.5, width=12)
top<-plot_grid(gA, gC,gD, nrow=1, labels=c('a', 'b', 'c'))
bot<-plot_grid(gE, gF, gG, nrow=1, labels=c('d', 'e', 'f'), rel_widths=c(1, 1.2, 1))
print(
  plot_grid(top, bot, nrow=2)
)
dev.off()


#### Supplementary figs from IME model

# predict mld by island
mld_pred<-mod_post_island(mod = m2_linear, dat_raw = dat_month, var = 'mld') %>% group_by(island) %>% 
  mutate(y1 = estimate__[which.min(mld)], y2 = estimate__[which.max(mld)], diff = y2 - y1, 
         col = ifelse(diff < 0, 'red3', 'blue4'))


pdf(file = 'fig/ime_mld_by_island.pdf', height = 7, width=12)
ggplot(mld_pred, aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100, fill=col)) + 
  geom_ribbon(alpha=0.1) +
  geom_line() + 
  labs(y = 'chl-a enhancement, %', x = 'mixed layer depth') +
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(limits=c(1, 70)) +
  scale_fill_identity() +
  facet_wrap(.~island, scales='free') +
  theme(strip.placement = 'bottom', strip.background = element_blank())
dev.off()

# Predict IME by month for each island [no longer in main model]
# month_pred<-mod_post_island(mod = m2_linear_month, dat_raw = dat_month, var = 'month_num') 
# 
# pdf(file = 'fig/ime_month_by_island.pdf', height = 7, width=12)
# ggplot(month_pred, aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100)) + 
#   geom_ribbon(alpha=0.1) +
#   geom_line() + 
#   labs(y = 'chl-a enhancement, %', x = 'month') +
#   scale_y_continuous(labels = label_percent()) +
#   scale_x_continuous(limits=c(1, 12)) +
#   scale_fill_identity() +
#   facet_wrap(.~island, scales='free') +
#   theme(strip.placement = 'bottom', strip.background = element_blank())
# dev.off()
# 

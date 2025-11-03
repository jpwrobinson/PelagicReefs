
# Load Messi IME and Gove drivers with statistical model objects
load(file = 'results/mod_ime_month_crep_attributes.rds')
load(file = 'results/mod_ime_crep_attributes.rds')

min_max<-dat_month %>% group_by(island) %>% 
  summarise(min = min(Chl_increase_nearby, na.rm=TRUE), max = max(Chl_increase_nearby, na.rm=TRUE))

dat<-dat %>% left_join(min_max, by = 'island')
labs<-dat %>% filter(island=='Lisianski') %>% select(min, mean_chl_percent, max) 

# Panel A = spatial + temporal variability in IME at CREP islands
gA<-ggplot(dat, aes(mean_chl_percent, fct_reorder(island, max))) +
  geom_pointrange(aes(xmin = min, xmax =max, col=region.col), fatten=0) +
  geom_point(aes(fill=region.col), pch=21, size=2) +
  # geom_point(aes(x = max), col='black', size=1) +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     sec.axis = sec_axis(~ ., labels=c('Min', 'Mean', 'Max'), 
                                         breaks=c(labs$min[1], labs$mean[1],labs$max[1]))) +
  labs(x = 'nearshore chl-a enhancement, %', y = '') +
  guides(fill='none', colour='none') +
  theme(axis.line.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_text(size=8))

ggplot(dat_month, aes(month_num, Chl_increase_nearby, group=island, col=REGION)) + 
  geom_line() + 
  facet_grid(~REGION)

# Panel B = drivers of IME [monthly and time-averaged]
bayes<-data.frame(b = c(bayes_R2(m2_linear)[1,'Estimate'], bayes_R2(m2_linear_month)[1,'Estimate']),
                  mod = c('Linear', 'Linear with month'),
                  x = 7, y = c(2,1))

# Extract posterior draws
effects <- rbind(m2_linear %>%
  gather_draws(b_Chlincreasenearby_geomorphic_typeIsland, b_Chlincreasenearby_reef_area_km2, b_Chlincreasenearby_island_area_km2,
               # b_Chlincreasenearby_bathymetric_slope, b_Chlincreasenearby_population_statusU,
               b_Chlincreasenearby_avg_monthly_mm,
               b_Chlincreasenearby_chl_a_mg_m3_mean, bsp_Chlincreasenearby_mited_mean, b_Chlincreasenearby_mld) %>% 
  mutate(mod = 'Linear'),
  m2_linear_month %>%
  gather_draws(b_Chlincreasenearby_geomorphic_typeIsland, b_Chlincreasenearby_reef_area_km2, b_Chlincreasenearby_island_area_km2,
               # b_Chlincreasenearby_bathymetric_slope, b_Chlincreasenearby_population_statusU,
               b_Chlincreasenearby_avg_monthly_mm,
               b_Chlincreasenearby_chl_a_mg_m3_mean, bsp_Chlincreasenearby_mited_mean, b_Chlincreasenearby_mld) %>% 
  mutate(mod = 'Linear with month')) %>% 
  mutate(.variable = str_replace_all(.variable, 'b_Chlincreasenearby_', ''),
         .variable = str_replace_all(.variable, 'bsp_Chlincreasenearby_mi', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2','avg_monthly_mm',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean'))))

gB<-ggplot(effects, 
           aes(x = .value, y = var_fac, col=mod)) +
  # geom_text(data = bayes, 
            # aes(x = x, y = y, label = paste0(round(b*100,1),'% ', mod)), size=3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  stat_pointinterval(.width = c(0.5, 0.95), position = position_dodge(width=0.5)) +  
  labs(x = "Effect on chl-a enhancement", y = "") +
  scale_colour_manual(values = c('#737373', '#d94801'), guide=NULL) +
  scale_x_continuous(limits=c(-.75, 1.25)) +
  scale_y_discrete(labels =c('chl-a conc.', 'Mixed layer depth', 'Tidal energy',
                            # 'Uninhabited', 'Bathymetric slope', 
                            'Precipitation',
                            'Island area', 'Reef area', 'Island')) +
  theme(legend.position = c(.9,.9), legend.title = element_blank())

# Get conditional effects
source('func_mod_conditional.R')
mld_pred<-mod_post(mod = m2_linear, dat_raw = dat_month, var = 'mld')
ted_pred<-mod_post(mod = m2_linear, dat_raw = dat_month, var = 'ted_mean')
chl_pred<-mod_post(mod = m2_linear, dat_raw = dat_month, var = 'chl_a_mg_m3_mean')
dd<-rbind(mld_pred %>% select(-mld) %>% mutate(var = 'Mixed layer depth, m') , 
          ted_pred %>% select(-ted_mean) %>% mutate(var = 'Tidal energy, ?'), 
          chl_pred %>% select(-chl_a_mg_m3_mean) %>% mutate(var = 'chl-a, mg_m3'))

gC<-ggplot(dd, aes(raw, estimate__/100, ymax= upper95/100, ymin = lower95/100)) + 
  geom_ribbon(alpha=0.1) +
  geom_line() + 
  labs(y = 'chl-a enhancement, %', x = '') +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(~var, scales='free_x', switch = 'x') +
  theme(strip.placement = 'bottom', strip.background = element_blank())

# predict covariate by island
mld_pred<-mod_post_island(mod = m2_linear, dat_raw = dat_month, var = 'mld')

mld_pred <- mld_pred %>% group_by(island) %>% 
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


pdf(file = 'fig/Figure1.pdf', height=5, width=9)
rhs<-plot_grid(gB, gC, nrow=2, labels=c('b', 'c'))
plot_grid(gA, rhs, nrow=1, labels='a')
dev.off()

r2(m2_linear, by_component = TRUE) # 52% fixed effects. 63% full model.



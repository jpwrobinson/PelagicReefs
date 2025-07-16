
# Load Messi IME and Gove drivers with statistical model objects
load(file = 'results/mod_ime_month_crep_attributes.rds')
load(file = 'results/mod_ime_crep_attributes.rds')

# Panel A = spatial + temporal variability in IME at CREP islands
gA<-ggplot(dat, aes(mean_chl_percent, fct_reorder(island, mean_chl_percent), fill=region.col)) +
  geom_col() +
  scale_fill_identity() + 
  scale_x_continuous(expand=c(0,0)) +
  labs(x = 'nearshore chl-a enhancement, %', y = '') 

fac_level<-levels(with(dat, fct_reorder(island, -mean_chl_percent)))
dat_month$island_fac<-factor(dat_month$island, levels=fac_level)
dat_month<-dat_month %>% group_by(REGION) %>% 
  mutate(limmer = max(Chl_increase_nearby, na.rm=TRUE))

plots<-lapply(split(dat_month, dat_month$island), function(subdf) {
  ylim<-subdf$limmer[1]
  ggplot(subdf, aes(month_num, Chl_increase_nearby, col=region.col, group=island)) + 
    geom_line() + 
    geom_text(data = subdf %>%
                slice_max(Chl_increase_nearby), size=3, vjust=-1, aes(label=island)) +
    theme(legend.position = 'none', strip.text = element_blank(),
          plot.margin=unit('cm', c(0,0,0,0))) +
    labs(x = '', y = '') +
    scale_colour_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(limits=c(0, ylim)) +
    scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11), labels=month.abb[c(1, 3, 5, 7, 9, 11)])
})

row1<-plot_grid(plots[[26]], plots[[27]], plots[[7]], plots[[8]], plots[[2]], plots[[1]], nrow=1)
row2<-plot_grid(plots[[21]], plots[[25]], plots[[5]], plots[[3]],plots[[17]], plot_spacer(), nrow=1)
row3<-plot_grid(plots[[16]], plots[[6]], plots[[23]], plots[[15]], plot_spacer(),plot_spacer(), nrow=1)
row4<-plot_grid(plots[[22]], plots[[14]], plots[[11]], plots[[10]], plots[[4]], plot_spacer(), nrow=1)
row5<-plot_grid(plots[[30]], plots[[29]], plots[[24]], plots[[28]], plot_spacer(),plot_spacer(), nrow=1)

gB<-plot_grid(row1, row2, row3, row4, row5, nrow=5, 
              axis = 'tblr', align='none')


# Panel C = drivers of IME [monthly and time-averaged]
bayes<-data.frame(b = c(bayes_R2(m)[,'Estimate'], bayes_R2(m2)[,'Estimate']),
                  mod = c('Annual mean', 'Monthly mean'),
                  x = 7, y = c(2,1))

# Extract posterior draws
effects <- rbind(
  m %>%
  gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
               b_bathymetric_slope, b_population_statusU,
               b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>% mutate(mod = 'Annual mean'),
  m2 %>%
    gather_draws(b_geomorphic_typeIsland, b_reef_area_km2, b_island_area_km2,
                 b_bathymetric_slope, b_population_statusU,
                 b_chl_a_mg_m3_mean, b_ted_mean, b_mld) %>% mutate(mod = 'Monthly mean')) %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'bathymetric_slope', 'population_statusU',
                                         'ted_mean', 'mld','chl_a_mg_m3_mean', 'wave_energy_mean_kw_m1'))))

gC<-ggplot(effects, aes(x = .value, y = var_fac, col=mod)) +
  geom_text(data = bayes, aes(x = x, y = y, label = paste0(round(b*100,1),'% ', mod)), size=3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  stat_pointinterval(.width = c(0.5, 0.95), position = position_dodge(width=0.5)) +  
  labs(x = "Posterior effect on chl-a enhancement", y = "") +
  scale_colour_manual(values = c('#737373', '#d94801')) +
  scale_x_continuous(limits=c(-8, 8)) +
  scale_y_discrete(labels =c('chl-a conc.', 'Mixed layer depth', 'Tidal energy',
                            'Uninhabited', 'Bathymetric slope', 'Island area', 'Reef area', 'Island')) +
  theme(legend.position = c(.9,.9), legend.title = element_blank())


pdf(file = 'Figure1.pdf', height=7, width=12)
plot_grid(gA, gB, nrow=1)
dev.off()

pdf(file = 'Figure2.pdf', height=5, width=9)
print(gC)
dev.off()


bot

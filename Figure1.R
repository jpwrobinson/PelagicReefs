# Load Messi IME and Gove drivers with statistical model objects
source('0_loads/00_ime_dataframe.R')

# dim(dat) = 35 islands
dat<-ime_island %>% left_join(
  island_complex %>% ungroup() %>% 
    mutate(island = str_replace_all(island_group, '_C', '')) %>%
    select(island, island_group, region, region.col, sst_mean:ted_sum),
  by = 'island') %>% filter(!is.na(lat))

# Panel a = island seasonal IME, 3 panels
gInset<-ggplot(dat_month, aes(Chl_increase_nearby/100)) + 
  geom_histogram(col='black', fill='grey50', binwidth=0.1) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0), labels=label_percent()) +
  labs(y = 'N island-months', x = 'chl-a enhancement') +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 9))

gA<-ggplot(dat, aes(mean_chlorophyll, median_chl_percent/100, fill=region.col)) + 
  # geom_point(data = dat_month %>% mutate(median_chl_percent = Chl_increase_nearby/100), alpha=0.5, size=1, aes(col=region.col)) +
  geom_point(alpha=1, size=2.5, pch=21, col='black') +
  geom_text_repel(aes(label=island), size=2.5) +
  labs(y = 'chl-a enhancement', x = 'chl-a, mg/m3') +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_identity() +
  scale_color_identity()

gA<-gA + inset_element(gInset, 
                   left = 0.5, bottom = 0.5, 
                   right = 1, top = 1)

# Panel b = island seasonal IME, 3 panels
# fac_level<-levels(with(dat, fct_reorder(island, -median_chl_percent)))
# dat_month$island_fac<-factor(dat_month$island, levels=fac_level)

focs<-c('Laysan', 'Necker', 'Kauai','Hawaii', 'Jarvis', 'Palmyra', 'Agrihan', 'Guam', 'Tutuila', 'Tau')
pp<-dat_month %>% filter(island %in% focs) %>% 
  mutate(island_fac = factor(island, levels=focs))

gB<-ggplot(pp, aes(month_num, Chl_increase_nearby/100)) + 
    geom_area(alpha=0.5, aes(col=region.col,fill=region.col, group=island)) + 
    # geom_text(data = dat_month %>% filter(island %in% focs) %>% slice_max(Chl_increase_nearby), size=2, vjust=3, aes(label=island)) +
    geom_text(data = data.frame(island_fac=focs, month_num=11, Chl_increase_nearby = 110),
            size=2.5, hjust=1, aes(label=island_fac)) +
    theme(legend.position = 'none', plot.margin=unit( c(.5,.5,0.5,0), 'cm')) +
    labs(x = '', y = 'chl-a enhancement') +
    facet_wrap(~island_fac, scales='fixed', nrow=2) +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(expand=c(0,0), labels = label_percent()) +
    scale_x_continuous(breaks=c(1,  6, 11), labels=month.abb[c(1, 6, 11)]) +
    theme(strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(size=8))
  
pdf(file = 'fig/Figure1.pdf', height=3, width=9)
print(
  plot_grid(gA, gB, nrow=1, labels=c('a', 'b'), align='v')
)
dev.off()


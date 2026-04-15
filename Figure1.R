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

focs<-c('Laysan', 'Necker', 'Kauai','Hawaii',
        'Jarvis', 'Kingman', 'Guam', 'Tau')

pp<-dat_month %>% filter(island %in% focs) %>% 
  mutate(island_fac = factor(island, levels=focs),
         row = ifelse(island_fac %in% focs[1:4], 'a', 'z'))

labber<-data.frame(island_fac=factor(levels(pp$island_fac)), month_num=11, 
                   Chl_increase_nearby = c(rep(115, 4), rep(43,4))) %>% 
  left_join(pp %>% distinct(island_fac, region.col, row))

gBtemp<-ggplot(pp, aes(month_num, Chl_increase_nearby/100)) + 
    geom_area(alpha=0.5, aes(col=region.col,fill=region.col, group=island)) + 
    theme(legend.position = 'none', plot.margin=unit( c(.5,.5,0.5,0), 'cm')) +
    labs(x = '', y = 'IME strength') +
    facet_wrap(~island_fac, scales='fixed', nrow=1) +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(expand=c(0,0), labels = label_percent()) +
    scale_x_continuous(breaks=c(1,  6, 11), labels=month.abb[c(1, 6, 11)]) +
    theme(strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(size=8),
          plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), 'cm'))

th_top<-theme(strip.background = element_blank(), strip.text = element_blank(),
              axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              plot.margin=unit(c(0.4, 0.1, 0, 0.1), 'cm'))
  
gB<-plot_grid(gBtemp %+% pp[pp$row=='a',] + 
                th_top +
                geom_text(data = labber %>% filter(row=='a'),size=2.5, hjust=1, aes(label=island_fac, colour=region.col)),
              gBtemp %+% pp[pp$row=='z',] + 
                geom_text(data = labber %>% filter(row=='z'),size=2.5, hjust=1, aes(label=island_fac, colour=region.col)),
        nrow = 2)
                
pdf(file = 'fig/Figure1.pdf', height=3, width=9)
print(
  plot_grid(gA, gB, nrow=1, labels=c('a', 'b'), align='v')
)
dev.off()


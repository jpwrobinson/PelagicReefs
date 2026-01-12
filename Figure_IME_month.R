source('0_loads/00_plot_theme.R')

# Load Messi IME and Gove drivers
ime_island<-read.csv(file = 'island_ime_dat.csv') %>% select(-lon, -lat, -type)
ime_month<-read.csv(file = 'island_ime_month_dat.csv') %>% select(-lon, -lat, -type)

fac_level<-levels(with(dat, fct_reorder(island, -median_chl_percent)))
dat_month$island_fac<-factor(dat_month$island, levels=fac_level)
dat_month<-dat_month %>% group_by(REGION) %>% 
  mutate(limmer = max(Chl_increase_nearby, na.rm=TRUE),
         Chl_increase_nearby = ifelse(is.na(Chl_increase_nearby), 0, Chl_increase_nearby)) %>% 
  filter(!is.na(bathymetric_slope))

plots<-lapply(split(dat_month, dat_month$island), function(subdf) {
  ylim<-subdf$limmer[1]
  ggplot(subdf, aes(month_num, Chl_increase_nearby, col=region.col,fill=region.col, group=island)) + 
    geom_area(alpha=0.5) + 
    geom_text(data = subdf %>%
                slice_max(Chl_increase_nearby), size=2, vjust=-1, aes(label=island)) +
    theme(legend.position = 'none', strip.text = element_blank(),
          plot.margin=unit( c(0,0,0,0), 'cm')) +
    labs(x = '', y = '') +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(limits=c(0, ylim)) +
    scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11), labels=month.abb[c(1, 3, 5, 7, 9, 11)]) +
    theme(axis.text.x = element_blank())
})


row1<-plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[5]], plots[[7]], plots[[8]],plots[[18]],plots[[24]],plots[[28]],plots[[29]],plots[[30]],plots[[34]], ncol=1)
row2<-plot_grid(plots[[6]], plots[[15]],plots[[16]],plots[[17]],plots[[20]],plots[[21]],plots[[22]],plots[[26]], plots[[9]], plots[[12]], plots[[13]], plots[[19]],plots[[23]], ncol=1)
row3<-plot_grid(plots[[4]], plots[[10]], plots[[11]], plots[[14]],plots[[25]],plots[[27]],plots[[31]],plots[[32]],plots[[33]], plot_spacer(),plot_spacer(),plot_spacer(), ncol=1)

gMonth<-plot_grid(row1, row2, row3, ncol=3, 
                  axis = 'tblr', align='hv')

pdf(file = 'fig/FigureSX.pdf', height=9, width=5)
print(gMonth)
dev.off()


# ggplot(dat_month, aes(month, Chl_increase_nearby, col=region.col, group=island)) + geom_line() +
#          facet_grid(island~.) + theme_void()
# 
# ggplot(dat_month, aes(month, Chl_increase_nearby, col=region.col, fill=region.col, group=island)) + 
#   geom_area(alpha=0.5)  +
#   facet_wrap(REGION~island, drop=TRUE)

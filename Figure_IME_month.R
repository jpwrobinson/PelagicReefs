
# Load Messi IME and Gove drivers with statistical model objects
load(file = 'results/mod_ime_month_crep_attributes.rds')
load(file = 'results/mod_ime_crep_attributes.rds')

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

gMonth<-plot_grid(row1, row2, row3, row4, row5, nrow=5, 
                  axis = 'tblr', align='none')

pdf(file = 'fig/FigureSX.pdf', height=7, width=12)
print(gMonth)
dev.off()

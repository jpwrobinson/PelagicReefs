source('0_loads/00_plot_theme.R')
source('0_loads/00_ime_dataframe.R')

island_order <- dat_month |>
  filter(!is.na(bathymetric_slope)) %>% 
  distinct(island, lat, REGION) |>
  arrange(REGION, -lat) |>
  pull(island)

dat_month<-dat_month %>% group_by(REGION) %>% 
  mutate(limmer = max(Chl_increase_nearby, na.rm=TRUE),
         Chl_increase_nearby = ifelse(is.na(Chl_increase_nearby), 0, Chl_increase_nearby)) %>% 
  filter(!is.na(bathymetric_slope)) %>% 
  mutate(island = factor(island, levels = island_order))

plots<-lapply(split(dat_month, dat_month$island), function(subdf) {
  ylim<-subdf$limmer[1]
  ggplot(subdf, aes(month_num, Chl_increase_nearby, col=region.col,fill=region.col, group=island)) + 
    geom_area(alpha=0.5) + 
    geom_text(data = subdf %>%
                slice_max(Chl_increase_nearby), size=2, vjust=-1, aes(label=island)) +
    theme(legend.position = 'none', strip.text = element_blank(),
          plot.margin=unit( c(0.1,.1,0,-.5), 'cm')) +
    labs(x = '', y = '') +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_cartesian(clip='off') +
    scale_y_continuous(limits=c(0, ylim)) +
    scale_x_continuous(breaks=c(1,3,6,9,12), labels=substr(month.abb, 1, 1)[c(1,3,6,9,12)]) +
    theme(axis.text.x = element_text(size=7), axis.text.y = element_text(size = 8))
})


y_label <- ggdraw() + 
  draw_label("chl-a enhancement, %", angle = 90, vjust = 0.5, size = 10)
y2_label <- ggdraw() + 
  draw_label("", angle = 90, vjust = 0.5, size = 10)

# main panel grid
gMonth<-plot_grid(plotlist = plots[island_order], ncol = 6, axis = 'tblr', align='hv')


pdf(file = 'fig/FigureSX_IME.pdf', height=6, width=7)
plot_grid(y_label, gMonth, y2_label, ncol = 3, rel_widths = c(0.03, 1, 0.03))
dev.off()
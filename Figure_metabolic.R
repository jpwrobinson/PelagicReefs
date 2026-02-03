## CREP metabolic
source('loads/00_crep_metabolic.R')


## Planktivore flux
plank_mean_is<-plank_scaled %>% group_by(region.col, region, island) %>% summarise(planktivore_metab = median(planktivore_metab),
                                                                                   herbivore_metab = median(herbivore_metab))

gA<-ggplot(plank_mean_is) + 
  aes(x = fct_reorder(island, planktivore_metab), y = planktivore_metab, fill = region.col) +
  geom_jitter(data = plank_scaled, width = 0.15, alpha = 0.2, aes(col=region.col)) +
  geom_point(size = 2.5, pch=21, col='black') +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_y_log10(
    minor_breaks = NULL,
    limits=c(10^-3, 10^1),
    labels=scales::trans_format("log10", scales::math_format(10^.x)),
    sec.axis = dup_axis(name = 'Planktivore', labels=scales::trans_format("log10", scales::math_format(10^.x)))
  ) +
  coord_flip() +
  labs(x = '', y=expression(Planktivore~metabolic~rates~(kJ~m^-2~d^-1))) +
  theme(legend.position= 'none',
        panel.grid.major.x = element_line(colour='grey'))


## Hebivore flux
gB<-ggplot(plank_mean_is) + 
  aes(x = fct_reorder(island, planktivore_metab), y = herbivore_metab, fill = region.col) +
  geom_jitter(data = plank_scaled, width = 0.15, alpha = 0.2, aes(col=region.col)) +
  geom_point(size = 2.5, pch=21, col='black') +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_y_log10(
    minor_breaks = NULL,
    limits=c(10^-3, 10^1),
    labels=scales::trans_format("log10", scales::math_format(10^.x)),
    sec.axis = dup_axis(name = 'Herbivore', labels=scales::trans_format("log10", scales::math_format(10^.x)))
  ) +
  scale_x_discrete(position = 'top') + 
  coord_flip() +
  labs(x = '', y=expression(Planktivore~metabolic~rates~(kJ~m^-2~d^-1))) +
  theme(legend.position= 'none', 
        panel.grid.major.x = element_line(colour='grey'))


pdf(file = 'fig/FigureSX_metab.pdf', height=10, width=9)
plot_grid(gA, gB, labels = c('a', 'b'))
dev.off()
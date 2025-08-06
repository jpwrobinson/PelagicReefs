source('00_oceanographic_load.R')

# ime = mean upwelling %
ime_island<-read.csv(file = 'island_ime_dat.csv') 
ime_month<-read.csv(file = 'island_ime_month_dat.csv')

hist(ime_island$mean_ime_percent) # Gamma

# dim(dat) = 30 islands
dat<-ime_island %>% left_join(
  island %>% select(island, island_code, REGION, region.col, sst_mean:ted_sum),
  by = 'island') %>% 
  filter(!is.na(mld))

g1<-ggplot(dat, aes(months_ime, chl_a_mg_m3_mean, col=region.col)) + 
  geom_point(alpha=1) +
  geom_text_repel(aes(label=island), size=2) +
  labs(x = 'Number of months IME present', y = 'Mean chl-a, mg/m3') +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_color_identity()

g2<-ggplot(dat, aes(chl_a_mg_m3_mean, mean_ime_percent, col=region.col)) + 
  geom_point(alpha=1) +
  geom_text_repel(aes(label=island), size=2) +
  labs(y = 'chl-a enhancement, %', x = 'Mean chl-a, mg/m3') +
  # scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_color_identity()

pdf(file = 'fig/FigureSX_IME.pdf', height=3, width=9)
plot_grid(g1, g2, nrow=1, labels=c('a', 'b'))
dev.off()

library(tidyverse)

df<-read.csv('results/mld_seasonal_pred.csv')

survey_dates<-read.csv('data/noaa-crep/crep_for_analysis.csv') %>% 
  distinct(OBS_YEAR, DATE_, ISLAND) %>% 
  mutate(DATE_ = as.Date(DATE_, "%m/%d/%Y"), month = month(DATE_)) %>% 
  left_join(island %>% rename(ISLAND = island) %>% select(ISLAND, region)) %>%
  distinct(region, month) %>% 
  left_join(df %>% group_by(region, month) %>% summarise(MLD_pred = mean(MLD_pred)))

amp<-df %>% group_by(island, region, region.col) %>%
  summarise(mld_amp = max(MLD_pred) - min(MLD_pred))

regs<-unique(df$region)

for(i in 1:length(regs)){
  
  gg<-ggplot(df %>% filter(region %in% regs[i]), aes(month, MLD_pred, col=region.col)) + 
    geom_line(aes(group=island)) + 
    geom_text_repel(data = df %>% filter(region %in% regs[i] & month == 12), aes(label = island), nudge_x = 0.5, size=3) +
    geom_point(data = survey_dates %>% filter(region %in% regs[i]), pch=21, col='white', fill = 'black', size=3) +
    scale_x_continuous(breaks=c(1,3,6,9, 12), labels=c('Jan', 'Mar', 'Jun', 'Sept', 'Dec')) +
    scale_y_continuous(limits=c(10, 60)) +
    scale_colour_identity() +
    labs(x = '', y = 'Mixed layer depth', subtitle = regs[i]) +
    theme(legend.position = 'none', 
          axis.text = element_text(size=11),
          axis.title = element_text(size=11)) 
  
  assign(paste0('gg', str_replace_all(regs[i], ' island', '')), gg)
  assign(paste0('gg', str_replace_all(regs[i], ' Hawaiian', '')), gg)
}

gg2<-ggplot(amp, aes(fct_reorder2(island, region, mld_amp),mld_amp, fill = region.col)) + geom_col() +
  geom_text(aes(label = str_wrap(island, 12)), vjust=-.5, size=2.2) +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), limits=c(0,47)) +
  labs(x = '', y = 'Mixed layer amplitude, m') +
  theme(axis.text.x = element_blank())

pdf(file = 'fig/FigureSX_MLD_amp.pdf', height=6, width=16)
top<-plot_grid(ggMariana, ggNorthwestern, ggHawaii,ggEquatorial, ggSamoa, nrow=1)
plot_grid(top, gg2, nrow=2, labels=c('a', 'b'))
dev.off()
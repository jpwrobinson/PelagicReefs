library(mgcv)
library(gratia)

# LOAD 
source('00_oceanographic_load.R')
# source('00_crep_load.R')
mld<-mld %>% group_by(Island) %>% mutate(time_num = scale(time)[,1])

# What is change in MLD over time? seasonality and long-term trend
# 
# m1<-gam(MLD ~ s(time_num, by = Island) + s(month, bs = 'cc', k = 12, by = Island), 
#           correlation = corAR1(form = ~ time_num | Island),
#           data=mld, family = Gamma)
# 
# save(m1, file = 'results/mld_time_mod.rds')

load(file = 'results/mld_time_mod.rds')
hist(resid(m1))
# draw(m1)
# plot(m1)
summary(m1)
  
# get predicted monthly MLD holding time constant
df<-expand.grid(month = c(1:12), Island = unique(mld$Island), time_num = 0)
df$MLD_pred<-predict(m1, newdata = df, type='response')

# get predicted temporal MLD holding month constant
df2<-expand.grid(month = 6, Island = unique(mld$Island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df2$MLD_pred<-predict(m1, newdata = df2, type='response')
df2<-df2 %>% left_join(island %>% rename(Island = island) %>% select(Island, region)) 

ggplot(df2, aes(time_num, MLD_pred, col=Island)) + geom_line() + facet_wrap(~region)

# add categorical vars and survey dates
depth_survey<-read.csv('data/richardson_2023/Depth_study_fish_data.csv') %>% 
  mutate(DATE_ = as.Date(DATE_, "%d/%m/%Y"), month = month(DATE_)) %>% 
  left_join(island %>% rename(ISLAND = island) %>% select(ISLAND, region)) %>% 
  distinct(region, month) %>% 
  left_join(df %>% group_by(region, month) %>% summarise(MLD_pred = mean(MLD_pred)))

df<-df %>% left_join(island %>% rename(Island = island) %>% select(Island, region)) %>% 
  mutate(month_name = month.abb[month])

regs<-unique(df$region)
for(i in 1:length(regs)){

  gg<-ggplot(df %>% filter(region %in% regs[i]), aes(month, MLD_pred, col=Island)) + 
    geom_line() + 
    geom_text_repel(data = df %>% filter(region %in% regs[i] & month == 12), aes(label = Island), nudge_x = 0.5, size=3) +
    geom_point(data = depth_survey %>% filter(region %in% regs[i]), pch=21, col='white', fill = 'black', size=3) +
    scale_x_continuous(breaks=c(1,3,6,9, 12), labels=c('Jan', 'Mar', 'Jun', 'Sept', 'Dec')) +
    scale_y_continuous(limits=c(15, 60)) +
    labs(x = '', y = 'Mixed layer depth (predicted)', subtitle = regs[i]) +
    theme(legend.position = 'none') 
  
  assign(paste0('gg', regs[i]), gg)
}

pdf(file = 'fig/mld_season.pdf', height=5, width=16)
plot_grid(ggMariana, ggHawaii, `ggNorthwestern Hawaiian`, ggEquatorial, ggSamoa, nrow=1)
dev.off()
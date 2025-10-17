library(mgcv)
library(marginaleffects)
library(gratia)

# LOAD 
source('00_oceanographic_load.R')
# source('00_crep_load.R')
mld<-mld %>% group_by(Island) %>% 
  mutate(time_num = scale(time)[,1]) %>% 
  group_by(Island, month) %>% 
  mutate(month_mean = mean(MLD)) %>% 
  ungroup() %>% 
  mutate(anomaly = MLD - month_mean) %>% 
  group_by(Island) %>% mutate(anomaly_s = scale(anomaly)[,1]) %>% 
  left_join(island %>% rename(Island = island) %>% select(Island, region)) 

# What is change in MLD over time? seasonality and long-term trend
# 
# m1<-gam(MLD ~ s(time_num, by = Island) + s(month, bs = 'cc', k = 12, by = Island), 
#           correlation = corAR1(form = ~ time_num | Island),
#           data=mld, family = Gamma)
# 
# save(m1, file = 'results/mld_time_mod.rds')

# m2<-gam(anomaly ~ s(time_num, by = factor(Island)) + s(time_num, by = factor(region)),
#           correlation = corAR1(form = ~ time_num | Island), data=mld)
# 
# save(m2, file = 'results/mld_anomaly_time_mod.rds')


# 1. Explore MLD with season model (m1)
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


# 2. Explore MLD with anomaly model (m2)
load(file = 'results/mld_anomaly_time_mod.rds')
hist(resid(m2))
# plot(m2)
summary(m2)

# get predicted temporal MLD anomaly
df2<-expand.grid(Island = unique(mld$Island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df2$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$Island)))
df2<-df2 %>% left_join(island %>% rename(Island = island) %>% select(Island, region)) 
df2$MLD_pred<-predict(m2, newdata = df2, type='response')
df2$se<-predict(m2, newdata = df2, type='response', se.fit = TRUE)$se.fit
df2$MLD_lower<-with(df2, MLD_pred - 2*se)
df2$MLD_upper<-with(df2, MLD_pred + 2*se)

ggplot(df2, aes(date, MLD_pred, ymin = MLD_lower, ymax = MLD_upper, fill=Island)) + 
  geom_line(aes(col=Island)) + geom_ribbon(alpha=0.5) + facet_wrap(~region)


df3<-expand.grid(Island = unique(mld$Island)[1], region = unique(mld$region), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df3$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$region)))
df3$MLD_pred<-predict(m2, newdata = df3, type='response')
df3$se<-predict(m2, newdata = df3, type='response', se.fit = TRUE)$se.fit
df3$MLD_lower<-with(df3, MLD_pred - 2*se)
df3$MLD_upper<-with(df3, MLD_pred + 2*se)

ggplot(df3, aes(date, MLD_pred, ymin = MLD_lower, ymax = MLD_upper, fill=region)) + 
  geom_line(aes(col=region)) + geom_ribbon(alpha=0.5) + facet_wrap(~region)

# MLD is getting deeper over time?

# add categorical vars 
df2<-df2 %>% left_join(island %>% rename(Island = island) %>% select(Island, region))

regs<-unique(df2$region)
for(i in 1:length(regs)){
  
  gg<-ggplot(df2 %>% filter(region %in% regs[i]), aes(date, MLD_pred, col=Island)) + 
    geom_line() + 
    geom_text_repel(data = df2 %>% filter(region %in% regs[i] & date == max(df2$date)), aes(label = Island), nudge_x = 0.25, size=3, segment.colour = NA) +
    labs(x = '', y = 'Mixed layer depth anomaly (predicted)', subtitle = regs[i]) +
    scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
    theme(legend.position = 'none') 
  
  assign(paste0('gg', regs[i]), gg)
}

pdf(file = 'fig/mld_anomaly_trend.pdf', height=5, width=20)
plot_grid(ggMariana, ggHawaii, `ggNorthwestern Hawaiian`, ggEquatorial, ggSamoa, nrow=1)

ggplot(df3, aes(date, MLD_pred, ymin = MLD_lower, ymax = MLD_upper, fill=region)) + 
  geom_line(aes(col=region)) + geom_ribbon(alpha=0.5) + facet_wrap(~region, nrow=1) +
  labs(x = '', y = 'Mixed layer depth anomaly (predicted)', subtitle = 'MLD anomaly by region') +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  theme(legend.position = 'none')
  

dev.off()

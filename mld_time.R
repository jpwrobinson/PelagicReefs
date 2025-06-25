library(mgcv)
library(gratia)

# LOAD 
source('00_oceanographic_load.R')
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
draw(m1)
plot(m1)
summary(m1)

df<-expand.grid(month = c(1:12), Island = unique(mld$Island), time_num = 0)
df$MLD_pred<-predict(m1, newdata = df, type='response')

df<-df %>% left_join(island %>% rename(Island = island) %>% select(Island, region)) %>% 
  mutate(month_name = month.abb[month])

ggplot(df, aes(month, MLD_pred)) + geom_line() + 
  # facet_grid(region~Island, as.table = FALSE) +
  ggh4x::facet_nested(
    rows = vars(region),
    cols = vars(Island),
    scales = "fixed"  # Shared axes
  ) +
  scale_x_continuous(breaks=c(1,4,7,10), labels=c('Jan', 'Apr', 'Jul', 'Oct')) +
  labs(x = '', y = 'Mixed layer depth (predicted)')

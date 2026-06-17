# LOAD 
source('0_loads/00_oceanographic_load.R')

# What is change in MLD over time? seasonality and long-term trend

# pdf(file = 'fig/mld_obs_trend.pdf', height=9, width=20)
# ggplot(mld, aes(Date, MLD, col=REGION)) + geom_line() +
#   facet_wrap(~ island, scales='fixed') +
#   labs(x = '', y = 'Mixed layer depth, m') +
#   guides(colour='none')
# dev.off()

# convert to factor so smoother by term works
mld<-mld %>% 
  mutate(island = factor(island), region = factor(region)) %>% 
  arrange(island, time_num) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))
                    

m1<-bam(log(MLD) ~ 
          s(time_num, island, k=12) +
          s(month, bs = 'cc', k = 12, by = island),
          rho = 0.35,
          AR.start = mld$new_series,
          data=mld)

save(m1, file = 'results/mld_time_mod.rds')

# estimate rho using acf residuals - the lag 1 acf value. this is island smooths only.
# tested regional smoother with island fs smooths that penalize towards 0, this only predicted regional smooth (islands follow region)
# model is too big to fit in brms
m2 <- bam(
  anomaly ~ s(time_num, island, k = 12),
  data = mld,
  rho = 0.35,
  AR.start = mld$new_series
)

save(mld, m2, file = 'results/mld_anomaly_time_mod.rds')


# 1. Explore MLD with season model (m1)
load(file = 'results/mld_time_mod.rds')
hist(resid(m1))
draw(m1)
# plot(m1)
acf(resid(m1))
summary(m1) # 77.1% dev. explained. much of this is month. (how much?)
  
# get predicted monthly MLD holding time constant
ex_smooths <- grep("month", smooths(m1), value = TRUE)

df<-expand.grid(month = c(1:12), island = unique(mld$island), time_num = 0)
df$MLD_pred<-exp(predict(m1, newdata = df, type='response'))

# get predicted temporal MLD holding month constant
df2<-expand.grid(month = 1, island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
# df2$MLD_pred<-exp(predict(m1, newdata = df2, type='response',exclude=ex_smooths))
# df2<-df2 %>% left_join(island %>% rename(island = island) %>% select(island, region)) 
# ggplot(df2, aes(time_num, MLD_pred, col=island)) + geom_line() + facet_wrap(~region)

mld_trend<-data.frame(mld = exp(predict(m1, newdata = mld, type='response',exclude=ex_smooths)),
                      time = mld$Date,island = mld$island, region = mld$region)

ggplot(mld_trend, aes(time, mld, col=island)) + geom_line() + facet_wrap(~region)


# add categorical vars and survey dates
df<-df %>% 
  left_join(island %>% rename(island = island) %>% select(island, region)) %>% 
  left_join(region_df %>% select(island, region2)) %>% 
  left_join(island_cols) %>% 
  mutate(month_name = month.abb[month])

write.csv(df, file = 'results/mld_seasonal_pred.csv', row.names=FALSE) # this is mld amplitude
write.csv(mld_trend, file = 'results/mld_time_pred.csv', row.names=FALSE) # this is mld trend

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


# 2. Explore MLD with anomaly model (m2), which cancels out monthly variation.
load(file = 'results/mld_anomaly_time_mod.rds')
hist(resid(m2))
# plot(m2)
summary(m2) # dev exp. 4.5%
acf(resid(m2, type = "pearson"))
gratia::draw(m2)

## 3. Fit deep events anomaly
focal <- mld |> mutate(
  mld_category = case_when(
    anomaly > 20 ~ "deep",      # >20m deeper than usual
    anomaly < -20 ~ "shallow",  # >20m shallower than usual
    TRUE ~ "normal"),
  year_s = scale(year)[,1],
  mld_category = factor(mld_category, levels = c("normal", "shallow", "deep")))

priors <- c(
  prior(normal(0, 2), class = "Intercept", dpar = "mushallow"),
  prior(normal(0, 2), class = "Intercept", dpar = "mudeep"),
  prior(normal(0, 1), class = "b", dpar = "mushallow"),
  prior(normal(0, 1), class = "b", dpar = "mudeep"),
  prior(exponential(1), class = "sd", dpar = "mushallow"),
  prior(exponential(1), class = "sd", dpar = "mudeep")
)

m_deep <- brm(
  mld_category ~ year_s + (1 + year_s | island),
  family = categorical(link = 'logit'),
  data = focal,
  prior =priors, chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m_deep, focal, file = 'results/mld_time_extreme.rds')

summary(m_deep) 
# - mudeep: log-odds of deep vs normal
# - mushallow: log-odds of shallow vs normal

pp_check(m_deep, resp = 'deep_event')
conditional_effects(m_deep, categorical=TRUE)
mcmc_plot(m_deep)
ranef(m_deep,dpar = "mudeep")$island

# bayes_R2(m_deep) # 1%



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

## Derive MLD changes

## island derivatives: mean slope of MLD over time [exclude month]
dS <- gratia::derivatives(m1, select = "s(time_num,island)")
dS2 <- gratia::derivatives(m2)

mld_slopes <- rbind(dS %>% mutate(model = 'Obs'),
                    dS2 %>% mutate(model = 'Anom')) %>% 
  group_by(island, model) %>%
  summarise(mld_slope = mean(.derivative),
            mld_slope_se = sqrt(mean(.se^2)),  # root mean square of SEs
            mld_slope_lower = mld_slope - 1.96 * mld_slope_se,
            mld_slope_upper = mld_slope + 1.96 * mld_slope_se) %>% 
  left_join(ime_df %>% distinct(island, region, region.col))

# Anom and Obs have similar signals (r = 0.962)
mld_slopes %>% select(model, mld_slope) %>% pivot_wider(names_from = model, values_from = mld_slope) %>% ggplot() +geom_point(aes(Anom, Obs))

# 3rd var is the change in anomaly over time (ie. in metres)
anom_time<-expand.grid(island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
anom_time$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))

anom_time$MLD_pred<-predict(m2, newdata = anom_time, type='response')
anom_time$se<-predict(m2, newdata = anom_time, type='response', se.fit = TRUE)$se.fit
anom_time$MLD_lower<-with(anom_time, MLD_pred - 2*se)
anom_time$MLD_upper<-with(anom_time, MLD_pred + 2*se)

delta_anom <- anom_time %>% group_by(island) %>% 
  summarise(
    change = MLD_pred[which.max(time_num)] - MLD_pred[which.min(time_num)],
    change_lower = MLD_lower[which.max(time_num)] - MLD_upper[which.min(time_num)],
    change_upper = MLD_upper[which.max(time_num)] - MLD_lower[which.min(time_num)]
  )

write.csv(delta_anom, file = 'results/MLD_anom_change.csv', row.names=FALSE)
write.csv(mld_slopes, file = 'results/MLD_slopes.csv', row.names=FALSE)

# slope and predicted change have same signal (strongest in anom - near perfect cor())  
# ggplot(mld_slopes %>% filter(model =='Anom'), 
#        aes(mld_slope, change)) + geom_point()

ggplot(mld_slopes, aes(fct_reorder(island, mld_slope), 
                       mld_slope, ymin = mld_slope_lower, ymax = mld_slope_upper, col=region.col)) +
  geom_pointrange() +
  geom_point() +
  coord_flip() +
  facet_wrap(~model, scales='free')

ggplot(delta_anom, aes(fct_reorder(island, change), 
  change, ymin = change_lower, ymax = change_upper)) +
  geom_pointrange() +
  geom_point() +
  coord_flip()



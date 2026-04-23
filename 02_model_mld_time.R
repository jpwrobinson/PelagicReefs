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
          s(time_num, island, k=12, bs = 'fs') +
          s(month, bs = 'cc', k = 12, by = island),
          rho = 0.35,
          AR.start = mld$new_series,
          data=mld)

save(m1, file = 'results/mld_time_mod.rds')

# estimate rho using acf residuals - the lag 1 acf value. this is island smooths only.
# tested regional smoother with island fs smooths that penalize towards 0, this only predicted regional smooth (islands follow region)
m2 <- bam(
  anomaly ~ s(time_num, by = island, k = 12),
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
df2$MLD_pred<-exp(predict(m1, newdata = df2, type='response',exclude=ex_smooths))
df2<-df2 %>% left_join(island %>% rename(island = island) %>% select(island, region)) 

ggplot(df2, aes(time_num, MLD_pred, col=island)) + geom_line() + facet_wrap(~region)

# add categorical vars and survey dates
df<-df %>% 
  left_join(island %>% rename(island = island) %>% select(island, region)) %>% 
  left_join(region_df %>% select(island, region2)) %>% 
  left_join(island_cols) %>% 
  mutate(month_name = month.abb[month])

write.csv(df, file = 'results/mld_seasonal_pred.csv', row.names=FALSE) # this is mld amplitude

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

# is there a larger climatic process driving unexplained variation?

# get predicted temporal MLD anomaly
df2<-expand.grid(island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df2$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))
df2<-df2 %>% left_join(island %>% rename(island = island) %>% select(island, region, region.col)) 

df2$MLD_pred<-predict(m2, newdata = df2, type='response')
df2$se<-predict(m2, newdata = df2, type='response', se.fit = TRUE)$se.fit
df2$MLD_lower<-with(df2, MLD_pred - 2*se)
df2$MLD_upper<-with(df2, MLD_pred + 2*se)

ggplot(df2, aes(date, MLD_pred, ymin = MLD_lower, ymax = MLD_upper, fill=island)) + 
  geom_line(aes(col=island)) + geom_ribbon(alpha=0.5) + facet_wrap(~region)

ggplot(df2, aes(date, MLD_pred, ymin = MLD_lower, ymax = MLD_upper, fill=region)) + 
  geom_line(aes(col=region, group=island))  + facet_grid(~region) +
  guides(col='none')

## this is regional smooth - but need to have island + region to understand the full model prediction
region_smooth <- df2 %>%
  group_by(region, date, time_num) %>%
  summarize(
    # mean of island predictions
    region_fit = mean(MLD_pred),
    # SE of the mean across islands
    region_se = sqrt(sum(se^2)/n()),  # assumes island predictions are independent
    .groups = "drop"
  ) %>% mutate(
    ymin = region_fit - 2*region_se,
    ymax = region_fit + 2*region_se,
  )

ggplot(region_smooth, aes(date, region_fit, ymin = ymin, ymax = ymax, fill=region)) + 
  geom_line(aes(col=region)) + 
  geom_ribbon(alpha=0.5) + facet_wrap(~region) 

# MLD is getting deeper over time?

# add categorical vars 
df2<-df2 %>% left_join(island %>% rename(island = island) %>% select(island, region))

regs<-unique(df2$region)
for(i in 1:length(regs)){
  
  gg<-ggplot(df2 %>% filter(region %in% regs[i]), aes(date, MLD_pred, col=island)) + 
    geom_line() + 
    geom_text_repel(data = df2 %>% filter(region %in% regs[i] & date == max(df2$date)), aes(label = island), nudge_x = 0.25, size=3, segment.colour = NA) +
    labs(x = '', y = 'Mixed layer depth anomaly (predicted)', subtitle = regs[i]) +
    scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
    theme(legend.position = 'none') 
  
  assign(paste0('gg', regs[i]), gg)
}

pdf(file = 'fig/mld_anomaly_trend.pdf', height=5, width=20)
plot_grid(ggMariana, `ggNorthwestern Hawaiian`, ggHawaii, ggEquatorial, ggSamoa, nrow=1)

ggplot(region_smooth, aes(date, region_fit, ymin = ymin, ymax = ymax, fill=region)) + 
  geom_line(aes(col=region)) + 
  geom_ribbon(alpha=0.5) + facet_wrap(~region) +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  theme(legend.position = 'none') + 
  labs(y = 'MLD anomaly (predicted)', x ='')
  

dev.off()


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


ggplot(focal %>% filter(deep_event ==1), aes(year)) + 
  geom_histogram() + facet_wrap(~island)

summary(m_deep) 
# - mudeep: log-odds of deep vs normal
# - mushallow: log-odds of shallow vs normal

pp_check(m_deep, resp = 'deep_event')
conditional_effects(m_deep, categorical=TRUE)
mcmc_plot(m_deep)
ranef(m_deep,dpar = "mudeep")$island

# bayes_R2(m_deep) # 1%

preds <- tibble(year_s = seq(min(focal$year_s), max(focal$year_s), length.out = 100)) %>%
  left_join(focal %>% distinct(year, year_s)) %>% 
  add_epred_draws(m_deep, ndraws = 1000, re_formula = NA) %>%  # population-level only
  ungroup() %>%
  filter(.category != "normal")

# Plot with uncertainty ribbons
ggplot(preds, aes(x = year, y = .epred, color = .category, fill = .category)) +
  stat_lineribbon(.width = c(0.50, 0.95), alpha = 0.3) +
  scale_fill_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  scale_color_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  labs(
    x = "",
    y = "Probability"
  ) +
  annotate('text', x = 2020, y = 0.03, label = 'Deep event', col = '#de2d26') +
  annotate('text', x = 1996, y = 0.014, label = 'Shallow event', col = "#3182bd") +
  theme(legend.position = "none")



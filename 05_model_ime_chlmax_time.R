source('0_loads/00_oceanographic_load.R')

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region)
         ) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0)) %>% 
  # matching MLD, but note this is for the 1st of the month, whereas IME is 15th
  left_join(mld %>% mutate(date = as.Date(format(Date, "%Y-%m-15"))) %>% select(date, island, MLD)) %>% 
  # filter(!is.na(MLD)) %>% 
  mutate(mld_s = scale(MLD)[,1]) %>% 
  group_by(island) %>% 
  mutate(mld_mean = mean(MLD, na.rm=TRUE),
         mld_anom = MLD - mld_mean, island=factor(island)) %>% ungroup() %>% 
  mutate(mld_anom_s = scale(mld_anom))

## 1. Examining temopral trends in Chl_max by island, accounting for seasonality. High variance explained by month.
focal<-ime_df %>% filter(!is.na(Chl_max) & !is.na(MLD)) # n = 11489. 3 NA from Swains dropped.

m1<-bam(log(Chl_max) ~
          # s(time_s, island, k=12, bs = 'fs') + # dropped factor smooth version because pooling info not needed
          s(time_s, by = island, k=12) +
          s(month, bs = 'cc', k = 12, by = island),
        rho = 0.35,
        AR.start = focal$new_series,
        # family = Gamma(link = 'log'),
        data=focal)

# Dev. expl = 88.6% [fs version] or 8% [by = island, version]
load('results/mod_ime_time_chl_max.rds')
hist(resid(m1))
summary(m1)
acf(resid(m1))
plot(m1)
month_smooths <- grep("month", smooths(m1), value = TRUE)

df1<-expand.grid(month = 1, island = unique(ime_df$island), time_s = seq(min(ime_df$time_s), max(ime_df$time_s), length.out=100))
df1$pred<-predict(m1, newdata = df1, type='response', exclude=month_smooths)
df1<-df1 %>% left_join(ime_df %>% distinct(island, region, region.col)) 
df1$date<-rep(seq(min(ime_df$date), max(ime_df$date), length.out=100), each = length(unique(ime_df$island)))

g1<-ggplot(df1, aes(date, exp(pred), col=region.col, group=island)) + geom_line() + facet_wrap(~region) +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  scale_colour_identity() +
  labs(x = '', y = 'Chl_max (model prediction)', subtitle = 'Chl_max: time-series removing seasonal (month) cycles')

save(ime_df, m1, file = 'results/mod_ime_time_chl_max.rds')

## 2. Examining temopral trends in chl_max_anom by island, accounting for seasonality (y = diff from the average island-month). High variance explained by month.
# n = 338 obs per island

m2<-bam(chl_max_month_anom ~
          s(mld_s, k=3) + s(mld_anom_s, k=3) +
          s(time_s, by = island, k=12),
          # s(month, bs = 'cc', k = 12, by = island), # don't need cyclic month because it is part of anomaly
        rho = 0.35,
        AR.start = focal$new_series,
        nthreads = 4, # parallel
        data=focal)

# Dev. expl = 11.5%
load('results/mod_ime_time_chl_max_month_anom.rds')
hist(resid(m2))
summary(m2)
acf(resid(m2))
gratia::draw(m2)

time_smooths <- grep("time_s", smooths(m2), value = TRUE)

df1<-expand.grid(time_s = 0, island = unique(ime_df$island)[3], mld_s = 0,
                 mld_anom_s = seq(min(focal$mld_anom_s), max(focal$mld_anom_s), length.out=100))
df1$pred<-predict(m2, newdata = df1, type='response', exclude=time_smooths)
df1$mld_anom<-seq(min(focal$mld_anom), max(focal$mld_anom), length.out=100)

ggplot(df1, aes(mld_anom, pred)) + geom_line() + 
  labs(x = '', y = 'Chl_max anomaly (model prediction)', subtitle = 'Chl_max anomaly: association with MLD anomaly') +
  annotate('text', x = -20, y = 0, label = 'Less Chl_max than expected \n MLD shallower') +
  annotate('text', x = 50, y = .15, label = 'More Chl_max than expected \n MLD deeper')

save(ime_df, m2, file = 'results/mod_ime_time_chl_max_month_anom.rds')

# MLD anomaly has opposite effect to seasonal MLD effect on IME strength.
# Positive (more) Chl_max anomaly when MLD is deeper


draw(m2)
ed<-edf(m2) %>% 
  mutate(island = str_extract(.smooth, "(?<=island).*$")) 

g2<-ggplot(focal, aes(date, chl_max_month_anom)) + 
  geom_hline(yintercept = 0, linetype=1, col='grey') +
  geom_line(aes(col=region)) +
  geom_text(data = ed %>% filter(.edf > 1), aes(x = Inf ,  y = Inf, label = paste0('EDF=', round(.edf, 1))), hjust = 1, vjust=1, size=3) +
  facet_wrap(~island, scales='fixed') +
  scale_y_continuous(labels=scales::label_percent()) +
  labs(y = 'Chl_max climtaological anomaly', x = '', subtitle = 'Chl_max climatological anomaly: observed time-series, sig. curves with EDF > 1') +
  guides(colour='none') +
  theme(strip.background = element_blank(), strip.text.x = element_text(hjust=0, face=2))

pdf(file = 'fig/ime_ts_trends.pdf', height=7, width=12)
g1
g2
dev.off()

## 3. Examining temporal & seasonal trends in chl_max_anom by island, accounting for seasonality (y = diff from the average island-month). High variance explained by month.
# n = 338 obs per island

# This model estimates temporal variability accounting for island means and seasonal cycles (anomaly version) 
m3<-bam(chl_max_month_anom ~ ti(month, year_s, by = island, bs = c("cc", "tp"), k = c(12, 4)) +
          s(mld_s, k=3) + s(mld_anom_s, k=3),
        rho = 0.35,
        AR.start = focal$new_series,
        nthreads = 4, # parallel
        data=focal)

# Dev. expl = 4%
load('results/mod_ime_time_chl_max_month_anom_ti.rds')
hist(resid(m3))
summary(m3)
acf(resid(m3))
gratia::draw(m3)

save(ime_df, m3, file = 'results/mod_ime_time_chl_max_month_anom_ti.rds')

# testing seasonal variation for Jarvis
new_dat <- expand.grid(
  month   = 1:12,
  year_s = seq(min(focal$year_s), max(focal$year_s), length.out = 10),
  island  = "Jarvis"
)

new_dat$fit <- predict(m3, newdata = new_dat, type = "response")

ggplot(new_dat, aes(month, fit, colour = factor(year_s), group = year_s)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~island)


# save islands with strong ti() terms
sig_islands <-edf(m3) %>% 
  filter(.edf > 1) %>% 
  mutate(island = str_extract(.smooth, "(?<=island).*$")) %>% 
  left_join(ime_df %>% distinct(island,region))

# get smooth estimates for significant islands only
sm_data <- smooth_estimates(m3, 
                            select = sig_islands$.smooth) %>%
  mutate(island = str_extract(.smooth, "(?<=island).*$"))

# plot
sm_data %>%
  ggplot(aes(month, year_s, fill = .estimate)) +
  geom_tile() +
  scale_fill_distiller(
    palette  = "RdBu",
    limits   = c(-max(abs(sm_data$.estimate)), 
                 max(abs(sm_data$.estimate))),  # symmetric around 0
    name     = "Anomaly\nchange"
  ) +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  facet_wrap(~island) +
  labs(x = NULL, y = "Year") +
  theme_minimal() +
  theme(legend.position = "right")

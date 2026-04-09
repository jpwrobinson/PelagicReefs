

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region)
         ) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))

## 1. Examining temopral trends in Chl_max by island, accounting for seasonality. High variance explained by month.
focal<-ime_df %>% filter(!is.na(Chl_max)) # n = 11489. 3 NA from Swains dropped.

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

ggplot(df1, aes(date, exp(pred), col=region.col, group=island)) + geom_line() + facet_wrap(~region) +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  scale_colour_identity() +
  labs(x = '', y = 'Predicted Chl_max')

save(ime_df, m1, file = 'results/mod_ime_time_chl_max.rds')

## 2. Examining temopral trends in chl_max_anom by island, accounting for seasonality (y = diff from the average island-month). High variance explained by month.
# n = 338 obs per island

m2<-bam(chl_max_month_anom ~
          s(time_s, by = island, k=12),
          # s(month, bs = 'cc', k = 12, by = island), # don't need cyclic month because it is part of anomaly
        rho = 0.35,
        AR.start = focal$new_series,
        nthreads = 4, # parallel
        data=focal)

# Dev. expl = 14.1%
load('results/mod_ime_time_chl_max_month_anom.rds')
hist(resid(m2))
summary(m2)
acf(resid(m2))
plot(m2)

save(ime_df, m2, file = 'results/mod_ime_time_chl_max_month_anom.rds')


draw(m2)
ed<-edf(m2) %>% 
  mutate(island = str_extract(.smooth, "(?<=island).*$")) 

ggplot(focal %>% filter(island %in% sig_islands$island), aes(date, chl_max_month_anom)) + geom_line(aes(col=region)) +
  geom_text(data = ed %>% filter(.edf > 1), aes(x = Inf ,  y = Inf, label = round(.edf, 1)), hjust = 1, vjust=1) +
  facet_wrap(~island)



## 3. Examining temporal & seasonal trends in chl_max_anom by island, accounting for seasonality (y = diff from the average island-month). High variance explained by month.
# n = 338 obs per island

# This model estimates temporal variability accounting for island means and seasonal cycles (anomaly version) 
m3<-bam(chl_max_month_anom ~ ti(month, year_s, by = island, bs = c("cc", "tp"), k = c(12, 4)),
        rho = 0.35,
        AR.start = focal$new_series,
        nthreads = 4, # parallel
        data=focal)

# Dev. expl = 8.6%
load('results/mod_ime_time_chl_max_month_anom_ti.rds')
hist(resid(m3))
summary(m3)
acf(resid(m3))
plot(m3)

save(ime_df, m3, file = 'results/mod_ime_time_chl_max_month_anom_ti.rds')

sig_islands <-edf(m3) %>% 
  filter(.edf > 0.1) %>% 
  mutate(island = str_extract(.smooth, "(?<=island).*$")) 

draw(m3, select = sig_islands$.smooth)


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

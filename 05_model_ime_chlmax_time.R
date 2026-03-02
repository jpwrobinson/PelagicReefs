

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         island = factor(island), region = factor(region)
         ) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))


## 1. Examining temopral trends in Chl_max by island, accounting for seasonality. High variance explained by month.
focal<-ime_df %>% filter(!is.na(Chl_max)) # n = 11489. 3 NA from Swains dropped.

m1<-bam(log(Chl_max) ~
          s(time_s, island, k=12, bs = 'fs') +
          s(month, bs = 'cc', k = 12, by = island),
        rho = 0.35,
        AR.start = focal$new_series,
        # family = Gamma(link = 'log'),
        data=focal)

# Dev. expl = 88.6%
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

## 2. Examining temopral trends in chl_max_anom by island, accounting for seasonality. High variance explained by month.
focal<-ime_df %>% filter(!is.na(chl_max_anom)) # n = 11489. 3 NA from Swains dropped.

m2<-bam(chl_max_anom ~
          s(time_s, island, k=12, bs = 'fs') +
          s(month, bs = 'cc', k = 12, by = island),
        rho = 0.35,
        AR.start = focal$new_series,
        data=focal)

# Dev. expl = 37.2%
hist(resid(m2))
summary(m2)
acf(resid(m2))
plot(m2)

df2<-expand.grid(month = seq(1, 12, by = 1), island = unique(ime_df$island), time_s = 0)
df2$pred<-predict(m2, newdata = df2, type='response',exclude='time_s')
df2<-df2 %>% left_join(ime_df %>% distinct(island, region,region.col))

ggplot(df2, aes(month, pred, group=island, col=region.col)) + geom_line() + facet_wrap(~region) +
  geom_hline(yintercept = 0, col='grey') +
  scale_colour_identity() +
  labs(x = '', y = 'Predicted Chl_max anomaly')


## temporal smooth excluding month
month_smooths <- grep("month", smooths(m2), value = TRUE)

df3<-expand.grid(month = 6, island = unique(ime_df$island), time_s = seq(min(ime_df$time_s), max(ime_df$time_s), length.out=100))
df3$pred<-predict(m2, newdata = df3, type='response',exclude=month_smooths)
df3<-df3 %>% left_join(ime_df %>% distinct(island, region,region.col))
df3$date<-rep(seq(min(ime_df$date), max(ime_df$date), length.out=100), each = length(unique(ime_df$island)))

ggplot(df3, aes(date, pred, group=island, col=region.col)) + geom_line() + facet_wrap(~region) +
  geom_hline(yintercept = 0, col='grey') +
  scale_colour_identity() +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  labs(x = '', y = 'Predicted Chl_max anomaly')
  
 

## 3. Test if seasonality is changing over time
m3 <- bam(
  chl_max_anom ~ 
    s(time_s, island, k = 12, bs = "fs") +
    s(month, by = island, bs = "cc", k = 12) +
    ti(month, time_s, by = island,
       bs = c("cc", "tp"), k = c(12, 6)),
  data = ime_df,
  rho = 0.35,
  AR.start = ime_df$new_series
)

hist(resid(m3))
summary(m3)
acf(resid(m3))

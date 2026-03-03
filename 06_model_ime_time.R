

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         island = factor(island), region = factor(region)
  ) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))


## 1. Examining temopral trends in Chl_max by island, accounting for seasonality. High variance explained by month.
focal<-ime_df %>% filter(!is.na(Chl_increase_nearby)) # n = 8066 , ~3000 obs dropped

m1<-bam(log(Chl_increase_nearby) ~
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

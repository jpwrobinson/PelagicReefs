## This script models temporal trends in the IME% (chl enhancement).
## It is not ideal because lots of year - month - island combinations are NA. They do have Chl_max, but not always an IME detection.

ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region)
  ) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0),
         detected = as.integer(!is.na(Chl_increase_nearby))) %>% 
  # matching MLD, but note this is for the 1st of the month, whereas IME is 15th
  left_join(mld %>% mutate(date = as.Date(format(Date, "%Y-%m-15"))) %>% select(date, island, MLD)) %>% 
  # filter(!is.na(MLD)) %>% 
  mutate(mld_s = scale(MLD)[,1]) %>% 
  group_by(island) %>% 
  mutate(mld_mean = mean(MLD, na.rm=TRUE),
         mld_anom = MLD - mld_mean, island=factor(island)) %>% ungroup() %>% 
  mutate(mld_anom_s = scale(mld_anom),
         mld_mean_s = scale(mld_mean))


## 1. Examining temopral trends in Chl_% by island, accounting for seasonality. High
focal<-ime_df %>% filter(!is.na(Chl_increase_nearby)) # n = 8066 , ~3000 obs dropped

m1<-bam(log(Chl_increase_nearby) ~
          # s(time_s, island, k=12, bs = 'fs') +. # not using factor-smooth because dataset is balanced
          s(time_s, by = island, k=12) +
          s(mld_s, k=3) + s(mld_anom_s, k=3) +
          s(month, bs = 'cc', k = 12, by = island),
        rho = 0.35,
        AR.start = focal$new_series,
        # family = Gamma(link = 'log'),
        data=focal)

# Dev. expl = 10.7%
load('results/mod_ime_time.rds')
hist(resid(m1))
summary(m1)
acf(resid(m1))
gratia::draw(m1)
month_smooths <- grep("month", smooths(m1), value = TRUE)

df1<-expand.grid(month = 1, island = unique(ime_df$island), time_s = seq(min(ime_df$time_s), max(ime_df$time_s), length.out=100))
df1$pred<-predict(m1, newdata = df1, type='response', exclude=month_smooths)
df1<-df1 %>% left_join(ime_df %>% distinct(island, region, region.col)) 
df1$date<-rep(seq(min(ime_df$date), max(ime_df$date), length.out=100), each = length(unique(ime_df$island)))

ggplot(df1, aes(date, exp(pred), col=region.col, group=island)) + geom_line() + facet_wrap(~region) +
  scale_x_date(date_breaks = '5 years', date_labels = '%Y') +
  scale_colour_identity() +
  labs(x = '', y = 'Predicted Chl enhancement, %')

save(ime_df, focal, m1, file = 'results/mod_ime_time.rds')


## Is IME seasonality changing?
m2 <- bam(
  log(Chl_increase_nearby) ~ 
    # s(time_s, by = island, k = 12) +
    # s(month, by = island, bs = "cc", k = 12) +
    ti(month, time_s, by = island,
       bs = c("cc", "tp"), k = c(12, 6)),
  data = ime_df,
  rho = 0.35,
  AR.start = ime_df$new_series
)

# Dev. expl. = %
hist(resid(m2))
summary(m2)
overview(m2)
acf(resid(m2))

save(ime_df, focal, m2, file = 'results/mod_ime_time_seasonality.rds')

load(file = 'results/mod_ime_time_seasonality.rds')

## pull out edf values summed across smoother to understand where seasonality is time-variant
data.frame(
  term = names(m2$edf),
  edf  = m2$edf
) |>
  dplyr::filter(grepl("ti\\(", term)) |>
  dplyr::mutate(island = stringr::str_extract(term, "(?<=island)\\w+")) |>
  dplyr::group_by(island) |>
  dplyr::summarise(total_edf = sum(edf)) |>
  dplyr::arrange(desc(total_edf)) %>% data.frame


## Try binomial version = on/off IME
focal<-ime_df %>% filter(!is.na(mld_anom))

m_detect <- bam(
  detected ~ 
    s(mld_mean_s, k=3) + s(mld_anom_s, k=3) + # MLD effects
    s(month, bs = 'cc', k = 12, by = island) + # island-level seasonal probability
    s(time_s, by = island, bs = "cr", k = 10),   # island-level probability
  family = binomial,
  data = focal,
  method = "fREML",
  discrete = TRUE
)

summary(m_detect)
gratia::draw(m_detect, select = 'mld_mean', partial_match=TRUE)
gratia::draw(m_detect, select = 'mld_anom', partial_match=TRUE)
gratia::draw(m_detect, select = 'time_s', partial_match=TRUE)

ime_df %>% filter(mld_anom_s > 3)

ex_smooths <- grep("month|time|mean", smooths(m_detect), value = TRUE)

df2<-expand.grid(month = 1, time_s = 0, mld_mean_s = 0, island = unique(focal$island)[1], 
                 mld_anom_s = seq(min(focal$mld_anom_s), max(focal$mld_anom_s), length.out=100))
df2$pred<-predict(m_detect, newdata = df2, type='response', exclude=ex_smooths)
df2$se<-predict(m_detect, newdata = df2, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

df2<-df2 %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df2$mld_anom<-seq(min(focal$mld_anom), max(focal$mld_anom), length.out=100)

ggplot(df2, aes(mld_anom, pred, col=region.col, group=island, ymin = lower, ymax = upper)) + 
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  labs(x = '', y = 'Probability IME detection')


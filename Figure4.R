

## IME plots

# Panel A = 06_ime_time. m_detect. code is ready.
load(file = 'results/mod_ime_time_binom.rds')

# A = MLD anomaly preds
ex_smooths <- grep("month|time|mean", smooths(m_detect), value = TRUE)

df_mldanom<-expand.grid(month = 1, time_s = 0, mld_mean_s = 0, island = unique(focal$island)[1], 
                 mld_anom_s = seq(min(focal$mld_anom_s), max(focal$mld_anom_s), length.out=100))
df_mldanom$pred<-predict(m_detect, newdata = df_mldanom, type='response', exclude=ex_smooths)
df_mldanom$se<-predict(m_detect, newdata = df_mldanom, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

# B = MLD anomaly preds
ex_smooths <- grep("month|time|anom", smooths(m_detect), value = TRUE)
df_mldmean<-expand.grid(month = 1, time_s = 0, mld_anom_s = 0, island = unique(focal$island)[1], 
                 mld_mean_s = seq(min(focal$mld_mean_s), max(focal$mld_mean_s), length.out=100))
df_mldmean$pred<-predict(m_detect, newdata = df_mldmean, type='response', exclude=ex_smooths)
df_mldmean$se<-predict(m_detect, newdata = df_mldmean, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

# Match with observed predictor scales
df_mldanom<-df_mldanom %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df_mldanom$mld_anom<-seq(min(focal$mld_anom), max(focal$mld_anom), length.out=100)

df_mldmean<-df_mldmean %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df_mldmean$mld_mean<-seq(min(focal$mld_mean), max(focal$mld_mean), length.out=100)

# Plot and multipanel
gA<-ggplot(df_mldmean, aes(mld_mean, pred, col=region.col, group=island, ymin = lower, ymax = upper)) + 
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  labs(x = 'Mixed layer depth [mean], m', y = 'Probability of IME detection')

gB<-ggplot(df_mldanom, aes(mld_anom, pred, col=region.col, group=island, ymin = lower, ymax = upper)) + 
  geom_vline(xintercept = 0, linetype=5) +
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  labs(x = 'Mixed layer depth [anomaly], m', y = 'Probability of IME detection')

gIME<-plot_grid(gA, gB, nrow=1, labels=c('a', 'b'))

# Panel C = 02_model_mld_time. m2. MLD anomaly by island over time
load(file = 'results/mld_anomaly_time_mod.rds')
levs<-c('Northwestern Hawaiian', 'Hawaii', 'Mariana', 'Equatorial', 'Samoa')
  
# get predicted temporal MLD anomaly
df2<-expand.grid(island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
df2$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))
df2<-df2 %>% left_join(island %>% rename(island = island) %>% select(island, region, region.col)) 
df2$region<-factor(df2$region, levels = levs)

df2$MLD_pred<-predict(m2, newdata = df2, type='response')
df2$se<-predict(m2, newdata = df2, type='response', se.fit = TRUE)$se.fit
df2$MLD_lower<-with(df2, MLD_pred - 2*se)
df2$MLD_upper<-with(df2, MLD_pred + 2*se)

## this is regional smooth - but need to have island + region to understand the full model prediction
region_smooth <- df2 %>%
  group_by(region.col,region, date, time_num) %>%
  summarize(MLD_pred = mean(MLD_pred), region_se = sqrt(sum(se^2)/n()),  # assumes island predictions are independent
    .groups = "drop") %>% 
  mutate(ymin = MLD_pred - 2*region_se, ymax = MLD_pred + 2*region_se)

gC<-ggplot(region_smooth, aes(date, MLD_pred)) + 
  geom_hline(yintercept = 0, linetype=5, alpha=0.5) +
  geom_ribbon(aes( ymin = ymin, ymax = ymax, fill=region.col), alpha=0.2) + 
  geom_line(col='black', linewidth=2) + 
  geom_line(data = df2, aes(col=region.col, group=island), alpha=0.5) +
  guides(col='none', fill='none') +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_date(date_breaks = '7 years', date_labels = paste0("'", '%y')) +
  facet_wrap(~region, nrow=1) +
  labs(x = '', y = 'Mixed layer depth anomaly, m') +
  theme(strip.text = element_text(hjust = 0, size = 10), strip.background = element_blank(),
      axis.text.x = element_text(size = 10))

pdf(file = 'fig/Figure4.pdf', height=6, width=8.5)
plot_grid(gIME, gC, nrow=2, labels=c('', 'c'))
dev.off()

# Panel D or Sup Fig. Raw IME strength over time
ime_df_Q<-ime_df %>% 
  mutate(quarter = case_when(
    month %in% c(12, 1, 2) ~ "DJF",
    month %in% c(3, 4, 5)  ~ "MAM",
    month %in% c(6, 7, 8)  ~ "JJA",
    month %in% c(9, 10, 11) ~ "SON"
  ), season_year = if_else(month == 12, year + 1L, year)) %>% 
  group_by(island, region, season_year, quarter) |>
  summarise(Chl_increase_nearby = mean(Chl_increase_nearby, na.rm=TRUE)) %>% 
  mutate(date = season_year + (match(quarter, c("DJF","MAM","JJA","SON")) - 1) / 4)

island_order <- ime_df |>
  left_join(island %>% select(island, latitude)) %>% 
  distinct(island, latitude, region.num) |>
  arrange(region.num, -latitude) |>
  pull(island)


gX<-ggplot(ime_df %>% mutate(Chl_increase_nearby = ifelse(Chl_increase_nearby > 1, 1, Chl_increase_nearby)), 
           aes(date, island, fill=Chl_increase_nearby, col=Chl_increase_nearby)) + 
  geom_tile() +
  scale_y_discrete(limits=rev(island_order), sec.axis = dup_axis()) +
  scale_fill_gradientn(
    colors = chl_grad_cols,
    na.value = 'white',
    labels = label_percent(),
    limits=c(0, 1),
    name = 'IME strength')  +
  scale_color_gradientn(
    colors = chl_grad_cols,
    na.value = 'white',
    labels = label_percent(),
    limits=c(0, 1),
    name = 'IME strength')  +
  labs(x = '', y = '') + guides(color='none') +
  theme(axis.text = element_text(size=7),
        legend.position = 'top', 
        plot.margin = unit(c(0,0,0,0), 'cm'))

pdf(file = 'fig/FigureSX_IME_tiles.pdf', height=4, width=20)
gX + scale_x_date(expand=c(0,0), date_breaks = '1 years', date_labels = '%Y', sec.axis = dup_axis()) 
dev.off()

pdf(file = 'fig/FigureSX_IME_tiles_3mo.pdf', height=4.5, width=10)
gX + scale_x_continuous(expand=c(0,0), breaks=seq(1997, 2025, by = 1), sec.axis = dup_axis()) + ime_df_Q
dev.off()

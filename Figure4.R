

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
gA<-ggplot(df_mldmean, aes(mld_mean, pred, ymin = lower, ymax = upper)) + 
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  scale_y_continuous(limits=c(0, 0.8), expand=c(0,0)) +
  labs(x = 'Mixed layer depth [mean], m', y = 'P (IME detection)')

gB<-ggplot(df_mldanom, aes(mld_anom, pred)) + 
  geom_text(data = data.frame(mld_anom = c(-25, 25), pred = 0.78, z = c('Shallower', 'Deeper')), aes(label = z), size=3) +
  geom_vline(xintercept = 0, linetype=5) +
  geom_ribbon(col='transparent', alpha=0.1, aes(ymin = lower, ymax = upper)) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  scale_y_continuous(limits=c(0, 0.8), expand=c(0,0)) +
  scale_x_continuous(breaks = c(-30, -15, 0, 15, 30, 45), 
                   sec.axis = dup_axis(breaks =0, labels = 'Monthly mean')) +
  labs(x = 'Mixed layer depth [anomaly], m', y = 'P (IME detection)') +
  theme(axis.title.x.top = element_blank(), axis.line.x.top = element_blank())

histA <- ggplot(focal, aes(mld_mean)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histB<-  ggplot(focal, aes(mld_anom)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

# inset histos
gA<-gA + annotation_custom(
    grob = ggplotGrob(histA),
    xmin = min(focal$mld_mean), max(focal$mld_mean),  # Adjust the x-axis placement of the inset
    ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
  )

gB<-gB + annotation_custom(
  grob = ggplotGrob(histB),
  xmin = min(focal$mld_anom), max(focal$mld_anom),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
)

# deep / shallow probs
load('results/mld_time_extreme.rds')

preds <- tibble(year_s = seq(min(focal$year_s), max(focal$year_s), length.out = 100)) %>%
  left_join(focal %>% distinct(year, year_s)) %>% 
  add_epred_draws(m_deep, ndraws = 1000, re_formula = NA) %>%  # population-level only
  ungroup() %>%
  filter(.category != "normal")

# Plot with uncertainty ribbons
gC<-ggplot(preds, aes(x = year, y = .epred, color = .category, fill = .category)) +
  stat_lineribbon(.width = c(0.50, 0.95), alpha = 0.3) +
  scale_fill_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  scale_color_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  labs(x = "",y = "P (MLD anomaly ± 20 m)") +
  scale_x_continuous(breaks=seq(1995, 2025, by = 5)) +
  annotate('text', x = 2020, y = 0.03, label = 'Deep', col = '#de2d26') +
  annotate('text', x = 1996, y = 0.014, label = 'Shallow', col = "#3182bd") +
  theme(legend.position = "none")

# Panel C = 02_model_mld_time. 
# main panel = MLD obs removing month effect (m1)
# sup fig = MLD anomaly by island over time (m2)
load(file = 'results/mld_time_mod.rds')
load(file = 'results/mld_anomaly_time_mod.rds')

levs<-c('Northwestern Hawaiian', 'Hawaii', 'Mariana', 'Equatorial', 'Samoa')
ex_smooths <- grep("month", smooths(m1), value = TRUE)

# get predicted temporal MLD anomaly
mld_time<-expand.grid(month = 1, island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
mld_time$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))
mld_time$MLD_pred<-exp(predict(m1, newdata = mld_time, type='response',exclude=ex_smooths))
mld_time$se<-predict(m1, newdata = mld_time, type='response', exclude=ex_smooths, se.fit = TRUE)$se.fit
mld_time$MLD_lower<-with(mld_time, MLD_pred - 2*se)
mld_time$MLD_upper<-with(mld_time, MLD_pred + 2*se)
  
anom_time<-expand.grid(island = unique(mld$island), time_num = seq(min(mld$time_num), max(mld$time_num), length.out=100))
anom_time$date<-rep(seq(min(mld$Date), max(mld$Date), length.out=100), each = length(unique(mld$island)))

anom_time$MLD_pred<-predict(m2, newdata = anom_time, type='response')
anom_time$se<-predict(m2, newdata = anom_time, type='response', se.fit = TRUE)$se.fit
anom_time$MLD_lower<-with(anom_time, MLD_pred - 2*se)
anom_time$MLD_upper<-with(anom_time, MLD_pred + 2*se)

preds<-rbind(mld_time %>% mutate(var = 'obs', month=NULL),
             anom_time %>% mutate(var = 'anom')) %>% 
  left_join(island %>% rename(island = island) %>% select(island, region, region.col)) %>% 
  mutate(region = factor(region, levels = levs))

## this is regional smooth - but need to have island + region to understand the full model prediction
region_smooth <- preds %>%
  group_by(var, region.col,region, date, time_num) %>%
  summarize(MLD_pred = mean(MLD_pred), region_se = sqrt(sum(se^2)/n()),  # assumes island predictions are independent
    .groups = "drop") %>% 
  mutate(ymin = MLD_pred - 2*region_se, ymax = MLD_pred + 2*region_se)

gBase<-ggplot(data = region_smooth %>% filter(var == 'obs'), aes(date, MLD_pred)) + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill=region.col), alpha=0.2) + 
  geom_line(col='black', linewidth=1) + 
  guides(col='none', fill='none') +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_date(date_breaks = '7 years', date_labels = paste0("'", '%y')) +
  facet_wrap(~region, nrow=1) +
  theme(strip.text = element_text(hjust = 0, size = 10), strip.background = element_blank(),
      axis.text.x = element_text(size = 10))

gD<-gBase %+% (region_smooth %>% filter(var == 'anom')) + 
  geom_hline(yintercept = 0, linetype=5, alpha=0.5) +
  geom_line(data = preds %>% filter(var == 'anom'), aes(col=region.col, group=island), alpha=0.5) +
  labs(x = '', y = 'Mixed layer depth, m') 

gSX <- gBase + 
  geom_ribbon(data = preds %>% filter(var == 'obs'), 
              aes(group=island, ymin = MLD_lower, ymax = MLD_upper), 
              fill='grey', alpha=0.5) +
  geom_line(data = preds %>% filter(var == 'obs'), 
              aes(group=island, col=region.col)) +
  scale_x_date(date_breaks = '5 years', date_labels = paste0('%Y')) +
  labs(x = '', y = 'Mixed layer depth, m') 


pdf(file = 'fig/Figure4.pdf', height=5, width=8.5)
gIME<-plot_grid(gA, gB, gC, nrow=1, labels=c('a', 'b', 'c'))
plot_grid(gIME, gD, nrow=2, labels=c('', 'd'))
dev.off()

pdf(file = 'fig/FigureSX_MLD_time_obs.pdf', height=3, width=12.5)
gSX
dev.off()

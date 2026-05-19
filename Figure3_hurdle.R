

## IME plots

# Panel d = 06_ime_time. m_hurdle. code is ready.
load(file = 'results/mod_ime_time_hurdle.rds')

# d = MLD anomaly preds
ex_smooths <- grep("month|time|mean", smooths(m_hurdle), value = TRUE)

df_mldanom<-expand.grid(month = 1, time_s = 0, mld_mean_s = 0, island = unique(focal$island)[1], 
                        mld_anom_s = seq(min(focal$mld_anom_s), max(focal$mld_anom_s), length.out=100))
df_mldanom$pred<-predict(m_hurdle, newdata = df_mldanom, type='response', exclude=ex_smooths)
df_mldanom$se<-predict(m_hurdle, newdata = df_mldanom, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

# e = MLD anomaly preds
ex_smooths <- grep("month|time|anom", smooths(m_hurdle), value = TRUE)
df_mldmean<-expand.grid(month = 1, time_s = 0, mld_anom_s = 0, island = unique(focal$island)[1], 
                        mld_mean_s = seq(min(focal$mld_mean_s), max(focal$mld_mean_s), length.out=100))
df_mldmean$pred<-predict(m_hurdle, newdata = df_mldmean, type='response', exclude=ex_smooths)
df_mldmean$se<-predict(m_hurdle, newdata = df_mldmean, type='response', exclude=ex_smooths, se.fit=TRUE)$se.fit

# Match with observed predictor scales
df_mldanom<-df_mldanom %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df_mldanom$mld_anom<-seq(min(focal$mld_anom), max(focal$mld_anom), length.out=100)

df_mldmean<-df_mldmean %>% left_join(focal %>% distinct(island, region, region.col)) %>% 
  mutate(lower = pred - 2*se, upper = pred + 2*se)
df_mldmean$mld_mean<-seq(min(focal$mld_mean), max(focal$mld_mean), length.out=100)

# Plot and multipanel
gD<-ggplot(df_mldmean, aes(mld_mean, pred, ymin = lower, ymax = upper)) + 
  geom_ribbon(col='transparent', alpha=0.1) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = 'Mixed layer depth [mean], m', y = 'IME strength')

gE<-ggplot(df_mldanom, aes(mld_anom, pred)) + 
  geom_text(data = data.frame(mld_anom = c(-25, 25), pred = 0.24, z = c('Shallower', 'Deeper')), aes(label = z), size=3) +
  geom_vline(xintercept = 0, linetype=5) +
  geom_ribbon(col='transparent', alpha=0.1, aes(ymin = lower, ymax = upper)) +
  geom_line(col = 'blue') + 
  scale_colour_identity() +
  scale_y_continuous(expand=c(0,0), limits=c(0.1, 0.25), labels=label_percent()) +
  scale_x_continuous(breaks = c(-30, -15, 0, 15, 30, 45), 
                     sec.axis = dup_axis(breaks =0, labels = 'Monthly mean')) +
  labs(x = 'Mixed layer depth [anomaly], m', y = 'IME strength') +
  theme(axis.title.x.top = element_blank(), axis.line.x.top = element_blank())

histA <- ggplot(focal, aes(mld_mean)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

histB<-  ggplot(focal, aes(mld_anom)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(expand=c(0,0)) + theme_void() 

# inset histos
gD<-gD + annotation_custom(
  grob = ggplotGrob(histA),
  xmin = min(focal$mld_mean), max(focal$mld_mean),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.05  # Adjust the y-axis placement of the inset
)

gE<-gE + annotation_custom(
  grob = ggplotGrob(histB),
  xmin = min(focal$mld_anom), max(focal$mld_anom),  # Adjust the x-axis placement of the inset
  ymin = -Inf, ymax = 0.115  # Adjust the y-axis placement of the inset
)

# plot_grid(gD, gE, ncol=2)


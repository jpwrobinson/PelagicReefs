source('func_mod_conditional.R')

th_marg<-theme(plot.margin=unit(c(0.1, .1, .1,.1), 'cm'),
               axis.title = element_text(size = 10),
               axis.text = element_text(size=9))

## IME plots

# Panel A = 06_ime_time. m_detect. code is ready.
load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')

# A = MLD anomaly preds, B = MLD anomaly preds
df_mldmean<-marginal_post(m_detect, focal, 'mld_mean_s', 'mld_mean')
df_mldanom<-marginal_post(m_detect, focal, 'mld_anom_s', 'mld_anom')
# df_time<-marginal_post(m_detect, focal, 'time_s', 'time')

df_mldmeanG<-marginal_post(m_hurdle, focal, 'mld_mean_s', 'mld_mean')
df_mldanomG<-marginal_post(m_hurdle, focal, 'mld_anom_s', 'mld_anom')
# df_timeG<-marginal_post(m_hurdle, focal, 'time_s', 'time')

# marginal_post_island(m_hurdle, focal, 'time_s', 'time')

# Plot and multipanel
gA<-ggplot(df_mldmean, aes(mld_mean, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  lims(y = c(0, 0.9)) +
  labs(x = "", y = "P(IME)") +
  th_marg

gB<-ggplot(df_mldanom, aes(mld_anom, .epred)) +
    geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
    lims(y = c(0, 0.9)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(x = "", y = "") +
    th_marg

gC<-ggplot(df_mldmeanG, aes(mld_mean, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(labels=label_percent(), limits=c(0, 0.55)) +
  labs(x = "Mixed layer depth [mean], m", y = "IME strength") +
  th_marg

gD<-ggplot(df_mldanomG, aes(mld_anom, .epred)) +
  geom_vline(xintercept = 0, linetype=5, alpha=0.5) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  scale_y_continuous(labels=label_percent(), limits=c(0, 0.55)) +
  labs(x = "Mixed layer depth [anomaly], m", y = "") +
  th_marg


# gE<-ggplot(df_time, aes(time, .epred)) +
#   geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
#               alpha = 0.2, fill = "steelblue") +
#   geom_line(colour = "steelblue", linewidth = 0.9) +
#   scale_y_continuous(labels=label_percent()) +
#   labs(x = "", y = "")
# 
# gF<-ggplot(df_timeG, aes(mld_anom, .epred)) +
#   geom_vline(xintercept = 0, linetype=5) +
#   geom_ribbon(aes(ymin = .lower, ymax = .upper, group = .width),
#               alpha = 0.2, fill = "steelblue") +
#   geom_line(colour = "steelblue", linewidth = 0.9) +
#   scale_y_continuous(labels=label_percent()) +
#   labs(x = "", y = "")

plot_grid(gA, gB, gC, gD, align='hv', labels=c('a', 'b', 'c', 'd'))
 
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
gE<-ggplot(preds, aes(x = year, y = .epred, color = .category, fill = .category)) +
  stat_lineribbon(.width = c(0.50, 0.95), alpha = 0.3) +
  scale_fill_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  scale_color_manual(values = c("shallow" = "#3182bd", "deep" = "#de2d26")) +
  labs(x = "",y = "P (MLD anomaly ± 20 m)") +
  scale_x_continuous(breaks=seq(1995, 2025, by = 5)) +
  annotate('text', x = 2020, y = 0.03, label = 'Deep', col = '#de2d26', size=3) +
  annotate('text', x = 1996, y = 0.014, label = 'Shallow', col = "#3182bd", size=3) +
  theme(legend.position = "none",
        axis.title = element_text(size=10),
        axis.text = element_text(size=9))

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
  scale_x_date(
    breaks=as.Date(c('1993', '1999', '2005', '2011', '2017', '2023'), '%Y'),
    # date_breaks = '4 years', 
    # minor_breaks = as.Date(c('1996', '2002', '2008', '2014', '2020', '2026'), '%Y'),
               date_labels = paste0('%Y')) +
  facet_wrap(~region, nrow=1) +
  theme(strip.text = element_text(hjust = 0, size = 9), 
        strip.background = element_blank(),
      axis.text = element_text(size = 9))

gF<-gBase %+% (region_smooth %>% filter(var == 'anom')) + 
  geom_hline(yintercept = 0, linetype=5, alpha=0.5) +
  geom_line(data = preds %>% filter(var == 'anom'), 
            aes(col=region.col, group=island), alpha=0.5) +
  labs(x = '', y = 'Mixed layer anomaly, m') +
  theme(axis.title = element_text(size = 10))


gSX <- gBase + 
  geom_ribbon(data = preds %>% filter(var == 'obs'), 
              aes(group=island, ymin = MLD_lower, ymax = MLD_upper), 
              fill='grey', alpha=0.5) +
  geom_line(data = preds %>% filter(var == 'obs'), 
              aes(group=island, col=region.col)) +
  scale_x_date(date_breaks = '5 years', date_labels = paste0('%Y')) +
  labs(x = '', y = 'Mixed layer depth, m') 

gIME<-plot_grid(gA, gB, gC, gD, nrow=2, labels=c('a', 'b', 'c', 'd'))
gIME2<-plot_grid(gIME, gE, nrow=1, labels=c('', 'e'), rel_widths=c(1, 0.4))

pdf(file = 'fig/Figure3.pdf', height=5, width=10)
plot_grid(gIME2, gF, nrow=2, labels=c('', 'f'), rel_heights=c(1, 0.8))
dev.off()

pdf(file = 'fig/FigureSX_MLD_time_obs.pdf', height=3, width=12.5)
gSX
dev.off()

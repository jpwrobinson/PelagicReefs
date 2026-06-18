source('0_loads/00_islands.R')

# deep / shallow probs
load('results/mld_time_extreme.rds')

preds <- tibble(year_s = seq(min(focal$year_s), max(focal$year_s), length.out = 100)) %>%
  left_join(focal %>% distinct(year, year_s)) %>% 
  add_epred_draws(m_deep, ndraws = 1000, re_formula = NA) %>%  # population-level only
  ungroup() %>%
  filter(.category != "normal")

# Plot with uncertainty ribbons
gExtreme<-ggplot(preds, aes(x = year, y = .epred, color = .category, fill = .category)) +
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

# sup fig = MLD obs removing month effect (m1)
# main fig = MLD anomaly by island over time (m2)
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
    breaks=as.Date(c('1993', '2003', '2013', '2023'), '%Y'),
    # date_breaks = '4 years', 
    # minor_breaks = as.Date(c('1996', '2002', '2008', '2014', '2020', '2026'), '%Y'),
    date_labels = paste0('%Y')) +
  facet_wrap(~region, nrow=1) +
  theme(strip.text = element_text(hjust = 0, size = 9), 
        strip.background = element_blank(),
        axis.text = element_text(size = 8))

# deep / shallow on first panel
labels<-data.frame(region = as.factor(c(levels(region_smooth$region)[1], levels(region_smooth$region)[1], levels(region_smooth$region)[3])), 
                   date = as.Date(c('2002-01-01', '2013-03-01', '2010-03-01')),  
                   lab = c('Shallower','Deeper', "Delta * ' Mixed layer anomaly'"), MLD_pred = c(-4, 2.8, -3.5))
# dashed lines to indicate ∆MLD
annot<-region_smooth %>% filter(var == 'anom' & region=='Mariana') %>% slice(c(1, n()))

liner<-data.frame(line = c('vert', 'horiz'), region = as.factor(levels(region_smooth$region)[3]),
                  ymin = c(annot$MLD_pred[1], annot$MLD_pred[1]), ymax = c(annot$MLD_pred[2], annot$MLD_pred[1]),
                  xmin = c(annot$date[2], annot$date[1]), xmax = c(annot$date[2], annot$date[2]))

gMLD_anom<-gBase %+% (region_smooth %>% filter(var == 'anom')) + 
  # geom_hline(yintercept = 0, linetype=5, alpha=0.5) +
  geom_segment(data = liner[1,], aes(x = xmin, xend = xmax, y = ymin, yend = ymax), linetype=5, col='grey') +
  geom_segment(data = liner[2,], aes(x = xmin, xend = xmax, y = ymin, yend = ymax), linetype=5, col='grey') +
  geom_line(data = preds %>% filter(var == 'anom'), 
            aes(col=region.col, group=island), alpha=0.5) +
  geom_line(col='black', linewidth=1) + 
  geom_text(data = labels, aes(label = lab), col='black', size=2.5, parse=TRUE) +
  labs(x = '', y = 'Mixed layer anomaly, m') +
  theme(axis.title = element_text(size = 10))

gSX <- gBase + 
  geom_ribbon(data = preds %>% filter(var == 'obs'), 
              aes(group=island, ymin = MLD_lower, ymax = MLD_upper), 
              fill='grey', alpha=0.5) +
  geom_text(data = preds %>% filter(var == 'obs' & date == '2026-02-01'),
            aes(label = island), size=1.5, hjust = -.1) +
  geom_line(data = preds %>% filter(var == 'obs'), 
            aes(group=island, col=region.col)) +
  facet_wrap(~region, nrow=5) +
  scale_x_date(date_breaks = '5 years', date_labels = paste0('%Y')) +
  labs(x = '', y = 'Mixed layer depth, m') +
  theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), 'cm')) +
  coord_cartesian(clip='off')

delta_anom<-read.csv(file = 'results/MLD_anom_change.csv') %>% 
  left_join(island %>% distinct(island, region.col)) %>% 
  mutate(sig = ifelse(change_lower > 0, 'red', 'black')) %>% 
  filter(island %in% ime_df$island)

labber<-data.frame(island = delta_anom$island[which.max(delta_anom$change)],
                   change = c(-1, 1), label = c('Shallower', 'Deeper'))

gMLDdelta<-ggplot(delta_anom, 
                  aes(fct_reorder(island, change), change)) +
  geom_hline(yintercept = 0, linetype=5, colour= 'grey') +
  geom_pointrange(aes(ymin = change_lower, ymax = change_upper, col=sig), size=0.25) +
  geom_point(aes(y = Inf, col=region.col), pch=15, size =2) +
  geom_text(data = labber, aes(label = label), hjust=c(1,0), col='black', size=2.5) +
  coord_flip() +
  scale_colour_identity() +
  labs(x = '', y = expression(Delta * " Mixed layer anomaly, m")) +
  scale_x_discrete(position = 'top') +
  theme(legend.position = NULL, 
        axis.text.y = element_text(size =7),
        axis.title = element_text(size =9, lineheight = 0, vjust = 0.5),
        axis.ticks.y = element_blank())

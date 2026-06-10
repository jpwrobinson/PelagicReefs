
## examine climato differences in MODIS and GlobColour

ime_df<-read.csv('data/GlobColour/IME_climato_GlobColour_MODIS.csv') %>% 
  filter(!is.na(region.num)) 

island.vec<-ime_df %>% distinct(island) %>% pull(island)

ime_modis1<-read.csv(file = 'island_ime_month_dat.csv') %>% 
  left_join(region_df, by = 'island') %>% 
  left_join(island_cols) %>% 
  filter(!is.na(region.num)) %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>%
  mutate(cChl = NA, is_primaryIME = NA, data = 'MODIS_orginal', month=month_num, Chl_increase_nearby = Chl_increase_nearby/100) %>% 
  select(names(ime_df))

ime_data_diff<-ime_df %>% select(Chl_increase_nearby, month, island, data) %>% 
  pivot_wider(names_from = 'data', values_from = 'Chl_increase_nearby') %>% 
  mutate(diff = MODIS - GlobColour)

ime_data_diff %>% filter(!is.na(MODIS) & !is.na(GlobColour)) %>% 
  group_by(island) %>% summarise(cor = cor(GlobColour, MODIS), diff=mean(diff))

# MODIS is systematically larger than GlobColour
# MODIS detects more IMEs than GlobColour


pdf(file = 'fig/ime_db/ime_globcol_vs_modis.pdf', height=7, width=12)

ggplot(ime_df, aes(month, Chl_increase_nearby, col=data)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~island)

ggplot(rbind(ime_df, ime_modis1) %>% filter(data != 'GlobColour'), aes(month, Chl_increase_nearby, col=data)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~island)

ggplot(ime_df, aes(month, Chl_max, col=data)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~island)

ggplot(ime_data_diff, aes(GlobColour, MODIS)) + geom_point() + 
  geom_abline(intercept=0, slope=1, alpha=0.5) +
  facet_wrap(~island)

dev.off()

# NA cases tend to refer to rows where cChl was NA. This indicates there is no IME for XYZ???
# how many Chl_max are NA?
ime_df %>% filter(is.na(Chl_max)) # 3 months in 1998 for Swains
ime_df %>% filter(is.na(Chl_increase_nearby)) # 5,020 month~island combos [higher in new algo version]
ime_df %>% filter(has_IME ==1 & is_primaryIME == 0) # 1,120 month~island combos

ime_df %>% filter(has_IME ==1 & is_primaryIME == 0)  %>% group_by(island) %>% summarise(n = length(is_primaryIME)) %>% 
  arrange(-n)

1120 / dim(ime_df)[1] * 100 # 10% have an IME from a different island

# how are Chl_increase and IME strength correlated?
for(i in 1:length(island.vec)){
  with(ime_df %>% filter(!is.na(Chl_increase_nearby) & island %in% island.vec[i]), 
       print(paste(island.vec[i], round(cor(Chl_increase_nearby, strength_IME), 2)))) # r = 0.86
}

ggplot(ime_df, aes(Chl_increase_nearby, strength_IME)) + geom_point() +
  facet_wrap(~island)

# plot IMe area over time. any spikes indicate when IME may be sat in a Chl_max zone [e.g. equatorial boundary current]
ggplot(ime_df, aes(date, area_IME, group=island)) + geom_line() + facet_wrap(~island) +
  scale_y_continuous(labels = comma)


## plot Chl_max anomaly - how variable is Chlmax?
label_df <- ime_df %>%
  group_by(island) %>%
  summarise(
    date = min(date),
    chl_max_anom = 0.25
  )

pdf(file = 'fig/ime_db/ime_ChlMax_anomaly.pdf', height=7, width=12)
ggplot(ime_df %>% mutate(dir=ifelse(chl_max_anom>0, 'pos', 'neg')), 
       aes(date, chl_max_anom, col=region.col, #alpha=dir, 
           group=island)) + 
  geom_hline(yintercept = 0, col = 'grey', alpha=0.5) +
  geom_line() + 
  geom_text(data = label_df, aes(date, chl_max_anom, label = island), inherit.aes = FALSE, hjust = 0, size = 3) +
  scale_colour_identity() + 
  # scale_alpha_manual(values = c('neg' = 0.5, 'pos' = 1)) +
  facet_wrap(~island) +
  labs(y = 'Chl_max anomaly, (Chl_max - mean(Chl_max))', x= '')+
  scale_y_continuous(limits=c(-.15, 0.28), expand=c(0,0)) +
  scale_x_date(sec.axis=dup_axis()) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.minor = element_blank()
  )
dev.off()


# MEI from NOAA PSL: multivariate ENSO index, Combined SST, SLP, winds, OLR
# library(rsoi)
# mei <- download_mei()
# 

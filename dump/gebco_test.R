source('00_plot_theme.R')

gebco<-read.csv('data/gebco/richardson_GEBCO_compare.csv')

depth<-read.csv('data/richardson_2023/Depth_study_fish_data.csv') %>% 
  mutate(DATE_ = as.Date(DATE_, "%d/%m/%Y"))

gebco<-gebco %>% 
  mutate(resid = Richardson_Slope - GEBCO_slope,
         predict_steep = ifelse(Richardson_Slope > 10, 'Steep', 'Shallow')) %>% 
  left_join(depth %>% distinct(SITE, ISLAND, REGION)) %>% 
  ungroup()

write.csv(gebco, file = 'data/gebco/richardson_GEBCO_compare_with_island.csv', row.names=FALSE)

pdf(file = 'fig/gebco_vs_richardson_explore.pdf', height=5, width=8)

ggplot(gebco, aes(Richardson_Slope, GEBCO_slope, col=REGION)) +
  geom_point()

ggplot(gebco, aes(Richardson_Slope, GEBCO_slope, col=REGION)) +
  geom_abline(intercept=0, slope=1) +
  geom_point() +
  facet_wrap(~REGION) +
  theme(legend.position = 'none') 

labber<-data.frame(REGION = 'MHI', resid = c(-20,20), y = c(40), 
                   label = c('GEBCO\nsteeper', 'GEBCO\nshallower'))

ggplot(gebco, aes(resid, fill=REGION)) + 
  geom_histogram() + 
  geom_text(data = labber, aes(y=y,label = label), size=2.5) +
  facet_wrap(~REGION) +
  theme(legend.position = 'none') +
  labs(y = 'Number of sites', x = 'Richardson - GEBCO steepness', subtitle = 'Difference in GEBCO and Richardson')

dev.off()

# ggplot(gebco %>% filter(resid > 10 | resid < -10), 
#        aes(fct_reorder(SITE, resid), resid, fill=REGION)) +
#   geom_col() +
#   coord_flip()

# summary stats
N<-dim(gebco)[1]
gebco %>% filter(Richardson_Slope > 10 & GEBCO_slope > 10) %>% dim/N*100 # 46% steep agreement
gebco %>% filter(Richardson_Slope > 10 & GEBCO_slope < 10) %>% dim/N*100 # 4% steep disagreement
gebco %>% filter(Richardson_Slope < 10 & GEBCO_slope < 10) %>% dim/N*100 # 32% shallow agreement
gebco %>% filter(Richardson_Slope < 10 & GEBCO_slope > 10) %>% dim/N*100 # 16% shallow disagreement

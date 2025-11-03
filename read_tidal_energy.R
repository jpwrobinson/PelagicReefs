
# tidal conversion = stronger means more internal wave action + mixing, increases planktivores
# Sent by Gareth Nov 2025.
tc_all<-read.csv('data/tidal/TEDestimates_CREPislands_ForJames.csv') %>% 
  left_join(island %>% mutate(ISLAND = island_code, island = island) %>% select(ISLAND, island, region)) %>% 
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

tc<-tc_all %>% 
  group_by(ISLAND, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), 
            ted_median = median(TED_MEAN),
            ted_sum = sum(TED_SUM), 
            ted_sum_median = median(TED_SUM),
            ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))

tc_C<-tc_all %>% 
  group_by(island_group, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), 
            ted_median = median(TED_MEAN), 
            ted_sum = sum(TED_SUM), 
            ted_sum_median = median(TED_SUM),
            ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))



## 360 version
tc_all2<-read.csv('data/tidal/TEDestimates_CREPislands_ForJames_360.csv') %>% 
  left_join(island %>% mutate(ISLAND = island_code, island = island) %>% select(ISLAND, island, region)) %>% 
  mutate(island_group = ifelse(island %in% c('Maui', 'Lanai', 'Molokai', 'Lanai', 'Kahoolawe'), 'Maui_C', island),
         island_group = ifelse(island %in% c('Saipan', 'Tinian', 'Aguijan'), 'Saipan_C', island_group),
         island_group = ifelse(island %in% c('Ofu & Olosega', 'Tau'), 'Tau_C', island_group))

tc2<-tc_all %>% 
  group_by(ISLAND, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM), ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))

tc_C2<-tc_all %>% 
  group_by(island_group, region) %>% 
  summarise(ted_mean = mean(TED_MEAN), ted_sum = sum(TED_SUM), ted_sd = sd(TED_SUM), n_grids = n_distinct(GRID_ID))


## Q: What makes ted mean different from ted sum? Are grid cells equally sized?
## Q: How do we get site-level values? Why do we need site-level values?

## 360 and grid version have identical island-level values

pp<-list(
  ggplot(tc_all, aes(fct_reorder(ISLAND, TED_SUM), TED_SUM, col=region)) +
    ggdist::stat_halfeye() +
    labs(x = '', y = 'Tidal energy (sum)', col='') + 
    theme(legend.position = c(0.5, 0.7)),
  
  ggplot(tc_all, aes(fct_reorder(ISLAND, TED_MEAN), TED_MEAN, col=region)) +
    ggdist::stat_halfeye() +
    labs(x = '', y = 'Tidal energy (mean)', col='') + 
    theme(legend.position = c(0.5, 0.7)),

  ggplot(tc_all, aes(TED_SUM, TED_MEAN, col=region)) + 
    geom_point() + 
    facet_wrap(island ~ ., scales='free') +
    labs(x = 'Tidal energy (sum)', y='Tidal energy (mean)') + 
    theme(legend.position = 'none'),

  ggplot(tc, aes(fct_reorder(ISLAND, ted_mean), ted_mean, fill=region)) + 
    geom_col() +
    labs(x = '', y = 'Tidal energy (mean)', fill='') + 
    scale_y_continuous(expand=c(0,0)) +
    theme(legend.position = c(0.5, 0.7)),
  
  ggplot(tc, aes(fct_reorder(ISLAND, ted_sum_median), ted_sum_median, fill=region)) + 
    geom_col() +
    labs(x = '', y = 'Tidal energy (median of sum)', fill='') + 
    scale_y_continuous(expand=c(0,0)) +
    theme(legend.position = c(0.5, 0.7)),

  ggplot(tc, aes(fct_reorder(ISLAND, ted_sum), ted_sum, fill=region)) + 
    geom_col() +
    labs(x = '', y = 'Tidal energy (sum)', fill='') + 
    scale_y_continuous(expand=c(0,0)) +
    theme(legend.position = c(0.5, 0.7)),
  
  ggplot(tc, aes(ted_sum_median, ted_mean, label=ISLAND, col=region)) +
    geom_point() + geom_text_repel() +
    labs(x = 'Tidal energy (median of sum)', y = 'Tidal energy (mean)', col='') + 
    theme(legend.position = 'none')
)

pdf(file = 'fig/crep_island_TC.pdf', height=7, width=15)
for (p in pp) print(p)
dev.off()

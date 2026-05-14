
ime_df<-read.csv(file = 'data/GlobColour/GlobColour_IME_output.csv') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(year(date)),
         time = as.numeric(date),
         time_s = scale(time)[,1],
         year_s = scale(year)[,1],
         island = factor(island), region = factor(region))


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

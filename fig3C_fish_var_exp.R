
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

blanker<-data.frame(fg.col  = '#01579F', group = 'blanker', proportion = NA)

# Get variance explain by each covariate, sum by group

# Planktivore
coef_draws <- as_draws_df(m2_plank) %>%
  select(starts_with("b_"))

# For each draw, compute group contribution as sum of |coefficient|
# weighted by predictor variance in the data
g1 <- coef_draws %>%
  mutate(
    geomorphic = abs(b_geomorphic_typeIsland) + abs(b_reef_area_km2) + abs(b_island_area_km2),
    habitat = abs(b_site_bathy_400m) + abs(b_hard_coral) + abs(b_depth_m),
    seasonal = abs(b_avg_monthly_mm) + abs(b_mld_amp),
    total = geomorphic + habitat + seasonal,
    prop_geomorphic    = geomorphic    / total,
    prop_habitat          = habitat          / total,
    prop_seasonal = seasonal / total
  ) %>%
  select(starts_with("prop_")) %>% mutate(fg = 'Planktivore', fg.col='#01579F')


# Herbivore
coef_draws <- as_draws_df(m2_herb) %>%
  select(starts_with("b_"))

# For each draw, compute group contribution as sum of |coefficient|
# weighted by predictor variance in the data
g2 <- coef_draws %>%
  mutate(
    geomorphic = abs(b_geomorphic_typeIsland) + abs(b_reef_area_km2) + abs(b_island_area_km2),
    habitat = abs(b_site_bathy_400m) + abs(b_hard_coral) + abs(b_depth_m),
    seasonal = abs(b_avg_monthly_mm) + abs(b_mld_amp),
    total = geomorphic + habitat + seasonal,
    prop_geomorphic    = geomorphic    / total,
    prop_habitat          = habitat          / total,
    prop_seasonal = seasonal / total
  ) %>%
  select(starts_with("prop_")) %>% mutate(fg = 'Herbivore', fg.col='#FF8C00')

labs<-data.frame(fg = c('Planktivore', 'Herbivore'), fg.col=c('#01579F', '#FF8C00'), group = 'seasonal', proportion = c(0.6, 0.7))

# bind and pivot
vars<-rbind(g1, g2) %>% 
  select(starts_with("prop_"), fg.col) %>%
  pivot_longer(-fg.col,names_to = "group",values_to = "proportion", names_prefix = "prop_")

vars<-rbind(vars, blanker)

gC<-ggplot(vars, aes(x = proportion, y = group, fill = fg.col, col=fg.col)) +
  annotate('rect', xmin = -Inf, xmax=Inf, ymin = 2.5, ymax = Inf, fill='grey', alpha=0.1) +
  annotate('rect', xmin = -Inf, xmax=Inf, ymin =-Inf, ymax = 1.5, fill='grey', alpha=0.1) +
  stat_slabinterval(.width = c(0.5, 0.95), point_interval = median_qi,                   
                    slab_alpha = 0.5, interval_alpha = 1, point_alpha = 1,
                    position = position_dodge(0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(data = labs, aes(label = fg),  position = position_dodge(0.5), size=3) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_discrete(limits = c('seasonal', 'habitat', 'blanker', 'geomorphic'),
                   labels = c('Within-island\nseasonality', 'Site-level\nhabitat', '', 'Island-level\ngeomorphology')) +
  labs(x = "Proportion of explained variance",
       y = NULL) +
  scale_fill_identity() + scale_colour_identity() +
  theme(legend.position = "none", 
        axis.text.y = element_text(hjust=0.5),
        axis.ticks.y = element_blank())


load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

# Get variance explain by each covariate, sum by group

# Planktivore
coef_draws <- as_draws_df(m2_plank) |>
  select(starts_with("b_"))

# For each draw, compute group contribution as sum of |coefficient|
# weighted by predictor variance in the data
g1 <- coef_draws |>
  mutate(
    geomorphic = abs(b_geomorphic_typeIsland) + 
      abs(b_reef_area_km2) + 
      abs(b_island_area_km2),
    site = abs(b_site_bathy_400m) + 
      abs(b_hard_coral) + 
      abs(b_depth_m),
    oceanographic = abs(b_avg_monthly_mm) + 
      abs(b_mld_amp),
    total = geomorphic + site + oceanographic,
    prop_geomorphic    = geomorphic    / total,
    prop_site          = site          / total,
    prop_oceanographic = oceanographic / total
  ) |>
  select(starts_with("prop_")) %>% mutate(fg = 'Planktivore')


# Herbivore
coef_draws <- as_draws_df(m2_herb) |>
  select(starts_with("b_"))

# For each draw, compute group contribution as sum of |coefficient|
# weighted by predictor variance in the data
g2 <- coef_draws |>
  mutate(
    geomorphic = abs(b_geomorphic_typeIsland) + 
      abs(b_reef_area_km2) + 
      abs(b_island_area_km2),
    site = abs(b_site_bathy_400m) + 
      abs(b_hard_coral) + 
      abs(b_depth_m),
    oceanographic = abs(b_avg_monthly_mm) + 
      abs(b_mld_amp),
    total = geomorphic + site + oceanographic,
    prop_geomorphic    = geomorphic    / total,
    prop_site          = site          / total,
    prop_oceanographic = oceanographic / total
  ) |>
  select(starts_with("prop_")) %>% mutate(fg = 'Herbivore')



rbind(g1, g2) %>% 
  select(starts_with("prop_"), fg) |>
  pivot_longer(-fg,
               names_to  = "group",
               values_to = "proportion",
               names_prefix = "prop_") |>
  mutate(group = str_to_title(group)) |>
  ggplot(aes(x = proportion, y = group, fill = fg, col=fg)) +
  stat_halfeye(.width = c(0.8, 0.95), point_interval = median_qi) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Proportion of explained variance",
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

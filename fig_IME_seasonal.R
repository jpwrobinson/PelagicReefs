
th<-theme(axis.text.y = element_text(size = 6), axis.ticks.y = element_blank())

# # generate effects for seasonal & island
# Build nd from observed monthly means per island
nd <- dat_scaled_month |>
  select(-month_num) %>% 
  left_join(data.frame('month' = month.abb, 'month_num' = 1:12)) %>% 
  group_by(island, month_num) |>
  summarise(
    mld_anom = mean(mld_anom, na.rm = TRUE),
    mld      = mean(mld, na.rm = TRUE),
    avg_monthly_mm_anom = mean(avg_monthly_mm_anom, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  # remove all non-seasonal effects
  mutate(
    bathymetric_slope   = 0,
    geomorphic_type     = "Atoll",
    reef_area_km2       = 0,
    land_area_km2       = 0,
    mld_mean            = 0,
    mean_chlorophyll    = 0,
    ted_mean            = 0
  ) %>%
  left_join(
    dat_month |>
      group_by(island,REGION,lat, month_num) |>
      summarise(mld_obs = mean(mld, na.rm = TRUE),
                precip_obs = mean(avg_monthly_mm, na.rm = TRUE),.groups = "drop"),
    by = c("island", "month_num")
  ) %>% left_join(island_cols)

# Predict
pred_precip <- nd %>% mutate(mld_anom = 0) %>% 
  add_epred_draws(m_chl_inc, resp = "Chlincreasenearby", allow_new_levels = FALSE)
  
pred_mld <- nd %>%  mutate(avg_monthly_mm_anom = 0) %>% 
  add_epred_draws(m_chl_inc, resp = "Chlincreasenearby", allow_new_levels = FALSE)

pred_combo <- nd %>%  add_epred_draws(m_chl_inc, resp = "Chlincreasenearby", allow_new_levels = FALSE)
  

island_order <- nd |>
  distinct(island, lat, region.num) |>
  arrange(region.num, -lat) |>
  pull(island)

# Plot
gMLD<-pred_mld |>
  group_by(region.num, island, lat, month_num) |>
  summarise(median_epred = median(.epred)/100, .groups = "drop") |>
  ggplot(aes(x = month_num, y = island, fill = median_epred)) +
  geom_tile() +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand=c(0,0)) +
  scale_y_discrete(limits=rev(island_order)) +
  scale_fill_gradientn(
    colors = chl_grad_cols,
    labels = label_percent(), 
    limits=c(0, .25),
    name = 'IME strength') +
  labs(x = NULL, y = NULL, subtitle = 'Mixed layer depth') + th

gPrecip<-pred_precip |>
  mutate(.epred = ifelse(abs(avg_monthly_mm_anom) < 0.00001, NA, .epred)) %>% 
  group_by(region.num, island, lat, month_num) |>
  summarise(median_epred = median(.epred)/100, .groups = "drop") |>
  ggplot(aes(x = month_num, y = island, fill = median_epred)) +
  geom_tile() +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand=c(0,0)) +
  scale_y_discrete(limits=rev(island_order)) +
  scale_fill_gradientn(
    colors = chl_grad_cols,
    labels = label_percent(),
    limits=c(0, .25),
    na.value = alpha('grey90',0.5),
    name = 'IME strength') +
  labs(x = NULL, y = NULL, subtitle = 'Precipitation')  + th

gCombo<-pred_combo |>
  group_by(region.num, island, lat, month_num) |>
  summarise(median_epred = median(.epred)/100, .groups = "drop") |>
  ggplot(aes(x = month_num, y = island, fill = median_epred)) +
  geom_tile() +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand=c(0,0)) +
  scale_y_discrete(limits=rev(island_order)) +
  scale_fill_gradientn(
    colors = chl_grad_cols,
    limits=c(0, .28),
    labels = label_percent(), 
    name = 'IME strength') +
  labs(x = NULL, y = NULL, subtitle = 'Mixed layer + precipitation')  + th


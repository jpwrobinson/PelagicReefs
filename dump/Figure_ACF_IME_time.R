
## checking ACF by island
load('results/mod_ime_time_binom.rds')

resids <- focal |>
  add_residual_draws(m_detect, ndraws = 50) |>
  median_qi(.residual) |>
  group_by(island) |>
  arrange(island, time_s)

# compute ACF per island into a dataframe
acf_df <- resids |>
  group_split() |>
  map_dfr(~{
    a <- acf(.x$.residual, plot = FALSE, lag.max = 36)
    tibble(island = .x$island[1],
           lag    = a$lag[,,1],
           acf    = a$acf[,,1])
  })

ggplot(acf_df, aes(lag, acf)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_hline(yintercept = c(-1.96, 1.96) / sqrt(nrow(focal)/n_distinct(focal$island)),
             linetype = "dashed", colour = "steelblue") +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~island) +
  labs(x = "Lag (months)", y = "ACF", title = "Residual autocorrelation by island")

# These are not consistent. Some islands no AC and some clear repeating patterns in AC.
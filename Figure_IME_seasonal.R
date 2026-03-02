
library(gratia)


islands <- unique(ime_df$island)

# store results
amp_list <- list()
peak_list <- list()

# loop through islands to estimate seasonal preds and derived quantities (amplitude and timing)
for (i in islands) {
  
  newdat <- expand.grid(
    month  = seq(1, 12, length = 100),
    time_s = unique(ime_df$time_s),
    island = i
  ) %>% left_join(ime_df %>% distinct(time_s, date))
  
  # get design matrix
  Xp <- predict(m3, newdata = newdat, type = "lpmatrix")
  cn <- colnames(Xp)
  keep <- grepl("month", cn)
  Xp_month <- Xp[, keep, drop = FALSE]
  
  # simulate coefficient draws for
  set.seed(1)
  B <- 500
  
  beta_hat <- coef(m3)
  Vb <- vcov(m3)
  
  beta_sim <- MASS::mvrnorm(B, beta_hat, Vb)
  beta_sim_month <- beta_sim[, keep, drop = FALSE]
  
  # estimate pred curves
  pred_mat <- Xp_month %*% t(beta_sim_month)
  
  draws_curve <- cbind(
    newdat,
    pred_curve = as.vector(pred_mat),
    .draw = rep(1:B, each = nrow(newdat))
  )
  
  ## get seasonal amplitude over time
  amp_ci <- draws_curve %>%
    group_by(.draw, time_s, date) %>%
    summarise(
      amplitude = max(pred_curve) - min(pred_curve),
      .groups = "drop"
    ) %>%
    group_by(time_s, date) %>%
    summarise(
      est = mean(amplitude),
      lo = quantile(amplitude, 0.025),
      hi = quantile(amplitude, 0.975),
      island = i,
      .groups = "drop"
    )
  
  ## get seasonal peak timing over time
  peak_ci <- draws_curve %>%
    group_by(.draw, time_s, date) %>%
    summarise(
      peak_month = round(month[which.max(pred_curve)]),
      .groups = "drop"
    ) %>%
    group_by(time_s, date) %>%
    summarise(
      est = mean(peak_month),
      lo = quantile(peak_month, 0.025),
      hi = quantile(peak_month, 0.975),
      island = i,
      .groups = "drop"
    )
  
  amp_list[[i]] <- amp_ci
  peak_list[[i]] <- peak_ci
}

amp_df <- bind_rows(amp_list)
peak_df <- bind_rows(peak_list)

# save(m3, ime_df, amp_df, peak_df, file = 'data/GlobColour/ime_seasonal_preds.Rdata')

ggplot(amp_df, aes(x = date, y = est, color = island, fill = island)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = "Seasonal amplitude",
    title = "Seasonal amplitude through time"
  ) + facet_wrap(~island)

ggplot(peak_df, aes(x = date, y = est, color = island, fill = island)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = 1:12, limits = c(1,12)) +
  labs(
    x = "Time",
    y = "Month of peak",
    title = "Seasonal peak timing through time"
  ) + facet_wrap(~island)

library(brms)
library(kernelshap)
library(shapviz)
library(tidyverse)

# ── custom predict function for brms ─────────────────────────────────────────
# returns posterior mean of epred (fixed effects only, marginalised over islands)
brms_pred_fun <- function(model, newdata) {
  colMeans(
    posterior_epred(model, newdata = newdata, 
                    re_formula = NA, allow_new_levels  = TRUE,
                    ndraws=500)
  )
}

# ── covariate groups ──────────────────────────────────────────────────────────
groups <- list(
  Geomorphic    = c("island_area_km2", "reef_area_km2", "site_bathy_400m", "geomorphic_type"),
  Oceanographic = c("mld_mean", "avg_monthly_mm", "ted_mean"),
  Fish_habitat  = c("hard_coral", "depth_m"),
  Human         = c("population_status")
)

all_features <- unlist(groups, use.names = FALSE)

# ── helper: run kernelshap + aggregate to group R2 ───────────────────────────
group_shap_r2 <- function(fit, data, groups, bg_n = 100, response_var) {
  
  all_features <- unlist(groups, use.names=FALSE)
  
  bg <- data |> slice_sample(n = min(bg_n, nrow(data)))
  X  <- data |> select(all_of(all_features)) %>% 
    slice_sample(n = 300)    # speed up
    
  
  ks <- kernelshap(
    object        = fit,
    X             = X,
    bg_X          = bg |> select(all_of(all_features)),
    pred_fun      = brms_pred_fun,
    feature_names = all_features,
    parallel = TRUE
  )
  
  shap_mat <- as.data.frame(ks$S)
  
  # ── total variance in posterior mean predictions ──────────────────────────
  pred_means <- brms_pred_fun(fit, data)
  pred_var   <- var(pred_means)
  
  # ── group-level importance ────────────────────────────────────────────────
  map_dfr(names(groups), function(grp) {
    vars <- groups[[grp]]
    
    if (length(vars) == 1) {
      obs_imp <- abs(shap_mat[[vars]])
    } else {
      obs_imp <- rowSums(abs(shap_mat[, vars]))
    }
    
    mean_abs_shap <- mean(obs_imp)
    
    tibble(
      group         = grp,
      response      = response_var,
      mean_abs_shap = mean_abs_shap,
      rel_R2        = NA_real_,   # filled below after summing all groups
      abs_R2 = var(rowSums(shap_mat[, vars, drop = FALSE])) / pred_var
    )
  }) |>
    mutate(rel_R2 = mean_abs_shap / sum(mean_abs_shap))
}

# ── run for planktivore and herbivore ─────────────────────────────────────────
vp_plank <- group_shap_r2(m2_plank, plank_scaled, groups, 
                          response_var = "Planktivore")

# vp_herb  <- group_shap_r2(m2_herb,  herb_scaled,  groups, 
#                           response_var = "Herbivore")

vp_all <- bind_rows(vp_plank, vp_herb)

print(vp_all)

# ── plot ──────────────────────────────────────────────────────────────────────
vp_all |>
  mutate(group = fct_reorder(group, rel_R2)) |>
  ggplot(aes(x = rel_R2, y = group, fill = response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Planktivore" = "#2196F3",
                               "Herbivore"   = "#4CAF50")) +
  labs(x     = "Relative importance (proportion of total SHAP)",
       y     = NULL,
       fill  = NULL,
       title = "Variance partitioning via Kernel SHAP") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
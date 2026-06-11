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
ime_groups <- list(
  Geomorphic    = c("land_area_km2", "reef_area_km2", "bathymetric_slope", "geomorphic_type"),
  Oceanographic = c("mld_mean",'mld_anom', "avg_monthly_mm_anom", "ted_mean", 'mean_chlorophyll')
)

fish_groups <- list(
  Geomorphic    = c("island_area_km2", "reef_area_km2", "site_bathy_400m", "geomorphic_type"),
  Oceanographic = c("mld_mean", "avg_monthly_mm", "ted_mean"),
  Fish_habitat  = c("hard_coral", "depth_m"),
  Human         = c("population_status")
)

# ── helper: run kernelshap + aggregate to group R2 ───────────────────────────
group_shap_r2 <- function(fit, data, groups, bg_n = 100, n_explain = 300, response_var) {
  
  all_features <- unlist(groups, use.names=FALSE)
  
  bg <- data |> slice_sample(n = min(bg_n, nrow(data)))
  X  <- data |> select(all_of(all_features)) %>% 
    slice_sample(n = min(n_explain, nrow(data))) # for faster runs
  
  ks <- kernelshap(
    object        = fit,
    X             = X,
    bg_X          = bg |> select(all_of(all_features)),
    pred_fun      = brms_pred_fun,
    feature_names = all_features
  )
  
  shap_mat <- as.data.frame(ks$S)
  
  shap_mat |>
    summarise(across(everything(), var)) |>
    pivot_longer(everything(), names_to = "feature", values_to = "shap_var") |>
    arrange(desc(shap_var))
  
  # ── group-level importance ────────────────────────────────────────────────
  group_importance<-map_dfr(names(groups), function(grp) {
    vars <- groups[[grp]]
    
    if (length(vars) == 1) {
      obs_imp <- abs(shap_mat[[vars]])
    } else {
      obs_imp <- rowSums(abs(shap_mat[, vars]))
    }
    
    # average magnitude of effect across observations.
    mean_abs_shap <- mean(obs_imp)
    
    # total SHAP variance for abs_R2
    # heterogeneity of effect across observations
    total_shap_var <- sum(sapply(names(shap_mat), function(v) var(shap_mat[[v]])))
    
    # careful with abs_R2: positive covariance in groups (e.g. big islands = big reefs) can inflate shap values
    tibble(
      group         = grp,
      response      = response_var,
      mean_abs_shap = mean_abs_shap,
      rel_R2        = NA_real_,   # filled below after summing all groups
      abs_R2 = sum(sapply(vars, function(v) var(shap_mat[[v]]))) / total_shap_var
    )
  }) |>
    mutate(rel_R2 = mean_abs_shap / sum(mean_abs_shap))
  
  # ── variable-level importance ────────────────────────────────────────────────
  var_importance <- shap_mat |>
    summarise(across(everything(), ~mean(abs(.)))) |>
    pivot_longer(everything(), 
                 names_to  = "feature", 
                 values_to = "mean_abs_shap") |>
    mutate(response = response_var) |>
    arrange(desc(mean_abs_shap))
  
  return(list(group_importance, var_importance))
}

# ── run for IME ─────────────────────────────────────────
load(file = 'results/mod_ime.rds')
vp_ime <- group_shap_r2(m_chl_inc, mod_dat, 
                          ime_groups, n_explain = dim(mod_dat)[1], bg_n = 100,
                          response_var = "IME strength")

# ── run for planktivore and herbivore ─────────────────────────────────────────
load('results/mod_planktivore_metabolic.rds')
vp_plank <- group_shap_r2(m2_plank, plank_scaled, 
                          fish_groups, n_explain = 300, bg_n = 100,
                          response_var = "Planktivore")

load('results/mod_herbivore_metabolic.rds')
vp_herb  <- group_shap_r2(m2_herb,  herb_scaled,
                          fish_groups, n_explain = 300, bg_n = 100,
                          response_var = "Herbivore")


vp_all <- bind_rows(vp_plank[[1]], vp_herb[[1]])
vp_by_var <- bind_rows(vp_plank[[2]], vp_herb[[2]]) %>% 
  mutate(group = case_when(
    feature %in% fish_groups$Geomorphic    ~ "Geomorphic",
    feature %in% fish_groups$Oceanographic ~ "Oceanographic",
    feature %in% fish_groups$Fish_habitat  ~ "Fish_habitat",
    feature %in% fish_groups$Human         ~ "Human"))

print(vp_all)

write.csv(vp_all, file='results/fish_SHAP_all.csv', row.names = FALSE)
write.csv(vp_by_var, file='results/fish_SHAP_byvar.csv', row.names = FALSE)

# ── plot ──────────────────────────────────────────────────────────────────────

# rel_R2 = within-model partioning that ignores unexplained variance
vp_all |>
  mutate(group = fct_reorder(group, rel_R2)) |>
  ggplot(aes(x = rel_R2, y = group, fill = response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_x_continuous(expand=c(0,0), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Planktivore" = "#2196F3",
                               "Herbivore"   = "#4CAF50")) +
  labs(x     = "Variable importance (proportion of total SHAP)",
       y     = NULL, fill  = NULL) 

vp_all |>
  mutate(group = fct_reorder(group, rel_R2)) |>
  ggplot(aes(x = rel_R2, y = response, fill = group)) +
  geom_col(position='stack') +
  scale_x_continuous(expand=c(0,0), labels = scales::percent_format(accuracy = 1)) +
  # scale_fill_manual(values = c("Planktivore" = "#2196F3",
  #                              "Herbivore"   = "#4CAF50")) +
  labs(x     = "Variable importance (proportion of total SHAP)",
       y     = NULL, fill  = NULL) 

# mean_SHAP = cross-model comparisons
vp_by_var |>
  # mutate(group = fct_reorder(group, mean_abs_shap)) |>
  ggplot(aes(x = mean_abs_shap, y = group, fill = response)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_continuous(expand=c(0,0.01)) +
  scale_y_discrete(limits=c('Geomorphic', 'Oceanographic', 'Fish_habitat', 'Human')) +
  # scale_fill_manual(values = c("Planktivore" = "#2196F3",
  #                              "Herbivore"   = "#4CAF50")) +
  labs(x     = "Mean SHAP values",
       y     = NULL, fill  = NULL)+
  facet_wrap(~response, nrow=1) +
  coord_flip() +
  theme(legend.position='none')

vp_by_var |>
  group_by(response) %>% 
  mutate(feature = fct_reorder(feature, mean_abs_shap)) |>
  ggplot(aes(x = mean_abs_shap, y = feature, fill = response)) +
  geom_col() +
  geom_text(aes(label = feature), hjust = -0.05, size=3) +
  scale_x_continuous(expand=c(0,0)) +
  # scale_y_discrete(limits=c('Geomorphic', 'Oceanographic', 'Fish_habitat', 'Human')) +
  scale_fill_manual(values = c("Planktivore" = "#2196F3",
                               "Herbivore"   = "#4CAF50")) +
  labs(x     = "Mean SHAP values",
       y     = NULL, fill  = NULL)+
  facet_wrap(~response, nrow=1) +
  # coord_flip() +
  theme(legend.position='none',
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

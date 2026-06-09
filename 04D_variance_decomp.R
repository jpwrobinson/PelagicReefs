library(brms)
library(tidyverse)
library(matrixStats)

load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')

source('0_loads/00_crep_metabolic.R')



# ── helper: variance across columns (one value per posterior draw) ──────────
row_var <- function(mat) matrixStats::rowVars(mat)

variance_partition_brms <- function(fit, data, groups) {
  
  # drop Johnston NA
  data<-data %>% filter(!is.na(ted_mean)) %>% droplevels()
  data$island<-as.factor(data$island)
  
  # ── full predictions including random effects ─────────────────────────────
  pred_full    <- posterior_epred(m2_plank, newdata = data)          # fixed + random
  pred_fixonly <- posterior_epred(fit, newdata = data,
                                  re_formula = NA)              # fixed only
  pred_obs     <- posterior_predict(fit, newdata = data)        # includes sigma
  
  var_total    <- row_var(pred_obs)      # denominator: fixed + random + residual
  var_full     <- row_var(pred_full)     # fixed + random signal
  var_fixonly  <- row_var(pred_fixonly)  # fixed signal only
  
  # ── random effect contribution ────────────────────────────────────────────
  var_random   <- var_full - var_fixonly
  
  # ── fixed effect group contributions (from fixed-only predictions) ────────
  group_var <- map(groups, function(vars) {
    newdata_reduced <- data |>
      mutate(across(all_of(vars), \(x) mean(x, na.rm = TRUE)))
    pred_reduced <- posterior_epred(fit, newdata = newdata_reduced,
                                    re_formula = NA)   # fixed only, group removed
    var_fixonly - row_var(pred_reduced)
  })
  
  # ── residual ──────────────────────────────────────────────────────────────
  var_resid <- var_total - var_full   # everything not in fixed + random signal
  
  # ── compile ───────────────────────────────────────────────────────────────
  all_components <- c(
    group_var,
    list(Island_RE = var_random,
         Residual  = var_resid)
  )
  
  map_dfr(names(all_components), function(nm) {
    v <- all_components[[nm]] / var_total
    tibble(
      component = nm,
      mean      = mean(v),
      lower     = quantile(v, 0.025),
      upper     = quantile(v, 0.975)
    )
  })
}

groups <- list(
  Geomorphic    = c("island_area_km2", "reef_area_km2", "site_bathy_400m", "geomorphic_type"),
  Oceanographic = c("mld_mean", "avg_monthly_mm", "ted_mean"),
  Fish_habitat  = c("hard_coral", "depth"),
  Human         = c("population_status")
)

vp_planktivore <- variance_partition_brms(m2_plank, plank_scaled, groups)
vp_herbivore   <- variance_partition_brms(m2_herb, herb_scaled, groups)

# Combine for plotting
vp_all <- bind_rows(
  vp_planktivore |> mutate(response = "Planktivore"),
  vp_herbivore   |> mutate(response = "Herbivore")
)

print(vp_all)
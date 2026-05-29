mod_post<-function(mod, dat_raw, var, n = 100){
  
  vars<-names(mod$data)
  nd<-as.data.frame(setNames(as.list(rep(0, length(vars))), vars)) %>% 
    select(-as.name(var))
  
  nd$geomorphic_type<-'Atoll'
  
  condo<-conditional_effects(mod, as.name(var), prob=0.95, resolution = n,
                             conditions = nd, re_formula=NA)[[1]] %>% 
    mutate(raw = seq_range(dat_raw[[var]], n=n)) %>% 
    mutate(lower95 = lower__, upper95 = upper__) %>% 
    select({{var}}, raw, estimate__, lower95, upper95)
  
  c2<-conditional_effects(mod, as.name(var), prob=0.5)[[1]] %>% 
    select(lower__:upper__)
  
  condo$lower50<-c2$lower__
  condo$upper50<-c2$upper__
  
  return(condo)
}

marginal_post<-function(mod, dat_raw, var, var_raw, n = 100){
  
  pred_grid <- tibble(
    var = seq_range(mod$data[[var]][,1], n=n),
    var_raw = seq_range(dat_raw[[var_raw]], n=n),
    mld_anom_s = 0,
    mld_mean_s = 0,
    month      = 6,
    time_s     = 0,
    island     = focal$island[2]   # single island, effect is shared across islands
  ) %>% select(-as.name(var))
  
  colnames(pred_grid)[1]<-c(as.name(var)) # need full name for epred_draws
  pred_grid$var<-pred_grid[[var]] # need generic for marginalise operation
  
  epred <- pred_grid |>
    add_epred_draws(mod, re_formula = NA, ndraws = 500)
  
  # marginalise over islands (average the posterior draws across islands)
  epred_marginal <- epred |>
    group_by(var, var_raw, .draw) |>
    summarise(.epred = mean(.epred), .groups = "drop") |>
    group_by(var, var_raw) |>
    median_qi(.epred, .width = c(0.5, 0.95))

  colnames(epred_marginal)[1:2]<-c(as.name(var), as.name(var_raw))
  
  return(epred_marginal)
  
}

marginal_post_island<-function(mod, dat_raw, var, var_raw, n = 100){
  
  pred_grid <- expand.grid(
    var = seq_range(mod$data[[var]], n=n),
    var_raw = seq_range(dat_raw[[var_raw]], n=n),
    mld_anom_s = 0,
    mld_mean_s = 0,
    month      = 6,
    time_s     = 0,
    island     = focal$island   # single island, effect is shared across islands
  ) %>% select(-as.name(var))
  
  colnames(pred_grid)[1]<-c(as.name(var)) # need full name for epred_draws
  pred_grid$var<-pred_grid[[var]] # need generic for marginalise operation
  
  epred <- pred_grid |>
    add_epred_draws(mod, re_formula = NA, ndraws = 500)
  
  # marginalise over islands (average the posterior draws across islands)
  epred_marginal <- epred |>
    group_by(var, var_raw, .draw, island) |>
    summarise(.epred = mean(.epred), .groups = "drop") |>
    group_by(var, var_raw, island) |>
    median_qi(.epred, .width = c(0.5, 0.95))
  
  colnames(epred_marginal)[1:2]<-c(as.name(var), as.name(var_raw))
  
  return(epred_marginal)
  
}
  
  
mod_post_island<-function(mod, dat_raw, var, n = 100){
  
  # extract island-level covariates
  nd<-mod$data %>% 
    select(-avg_monthly_mm, -as.name(var)) %>% 
    select(geomorphic_type:island, ted_mean) %>% distinct() %>% 
    mutate(ted_mean = ifelse(is.na(ted_mean), 0, ted_mean))

  # add the mean monthly mm for each island [because IME dataset is monthly and so is this covariate - so need to avg over months]
  avg_monthly_mm<-mod$data %>% group_by(island) %>% summarise(avg_monthly_mm = mean(avg_monthly_mm))
  nd<-nd %>% left_join(avg_monthly_mm)
  
  # nd$population_status<-'U'
  
  N<-length(unique(nd$island))

  # estimate effect of var on y, with random effects
  condo<-conditional_effects(mod, as.name(var), prob=0.95, resolution = n,
                             conditions = nd, re_formula=NULL)[[1]] %>% 
    mutate(raw = rep(seq_range(dat_raw[[var]], n=n), times = N)) %>% 
    mutate(lower95 = lower__, upper95 = upper__) %>% 
    select({{var}}, raw, estimate__, lower95, upper95, island)
  
  c2<-conditional_effects(mod, as.name(var), prob=0.5)[[1]] %>% 
    select(lower__:upper__)
  
  condo$lower50<-c2$lower__
  condo$upper50<-c2$upper__
  
  # clip each island to its observed MLD range
  if(var != 'month_num'){
  obs_range<-dat_raw %>% group_by(island) %>% summarise(min = min(!!sym(var)), max = max(!!sym(var)))
  
  condo <- condo %>%
    left_join(obs_range, by = "island") %>%
    filter(raw >= min, raw <= max) %>%
    select(-min, -max)
  }
  
  return(condo)
}
  



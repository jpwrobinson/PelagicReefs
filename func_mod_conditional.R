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
  



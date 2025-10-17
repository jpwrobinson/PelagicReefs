mod_post<-function(mod, dat_raw, var){
  
  vars<-names(m2_linear_month$data)
  nd<-as.data.frame(setNames(as.list(rep(0, length(vars))), vars)) %>% 
    select(-as.name(var), -island, -REGION, -island.REGION)
  
  nd$geomorphic_type<-'Atoll'
  nd$population_status<-'U'
  
  condo<-conditional_effects(mod, as.name(var), prob=0.95,
                             conditions = nd, re_formula=NA)[[1]] %>% 
    mutate(raw = seq_range(dat_raw[[var]], n=100)) %>% 
    mutate(lower95 = lower__, upper95 = upper__) %>% 
    select({{var}}, raw, estimate__, lower95, upper95)
  
  c2<-conditional_effects(mod, as.name(var), prob=0.5)[[1]] %>% 
    select(lower__:upper__)
  
  condo$lower50<-c2$lower__
  condo$upper50<-c2$upper__
  
  return(condo)
}

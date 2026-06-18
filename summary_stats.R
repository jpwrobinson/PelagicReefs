source('0_loads/00_plot_theme.R')
source('0_loads/00_ime_dataframe.R')

## Fig 1 - IME dataset
focs<-c('Necker', "Sarigan", 'Guam', 'Jarvis', 'Kauai', 'Agrihan', 'Lisianski')

dat_month %>% filter(island %in% focs) %>% group_by(island) %>%  reframe(range(Chl_increase_nearby, na.rm=T))
dat_month %>% filter(island %in% focs) %>% group_by(island) %>%  summarise(month[which.max(Chl_increase_nearby)])

ime_dat %>% reframe(range(median_chl_percent, na.rm=T))
ime_dat %>% group_by(REGION) %>% filter(!is.na(avg_monthly_mm)) %>% reframe(range(avg_monthly_mm))
ime_dat %>% filter(is.na(avg_monthly_mm)) %>% distinct(island)


# Fig 2 - posterior slopes

load(file = 'results/mod_ime.rds')
summary(m_chl_inc)

source('Figure2.R')
plot_data %>% group_by(lab) %>% slice_max(estimate__)
plot_data %>% group_by(lab) %>% slice_min(estimate__)

mld_pred %>% slice_max(estimate__)
mld_pred %>% slice_min(estimate__)

## Fig 3 - IME ~ MLD + TIME
nyears<-diff(range(year(region_smooth$date)))

region_smooth %>% group_by(region) %>% 
  summarise(mld_change = diff(range(region_fit))) %>% 
  mutate(mld_per_year = mld_change / nyears,
         mld_per_decade = mld_per_year * 10)


# Fig 4 - posterior slopes - fish
load('results/mod_planktivore_metabolic.rds')
load('results/mod_herbivore_metabolic.rds')
summary(m2_plank)
summary(m2_herb)

# metabolic rates
plank_scaled %>% 
  group_by(region.col, region, island) %>% 
  summarise(planktivore_metab = median(planktivore_metab),
            herbivore_metab = median(herbivore_metab)) %>% ungroup() %>% 
  reframe(range(planktivore_metab), range(herbivore_metab))

# plankton flux effect per m MLD
# eff<-effects %>% filter(.variable == 'mld_mean') %>% 
#   group_by(fg) %>% 
#   summarise(med = median(.value))
# 
# eff$med[eff$fg=='Planktivore']/sd(depth$mld_mean) # -0.13 per m MLD

load(file = 'results/mod_ime_time_binom.rds')
load(file = 'results/mod_ime_time_hurdle.rds')
bayes_R2(m_detectFull, re.form=NA)
bayes_R2(m_hurdleFull, re.form=NA)

## IME and MLD time series
delta_anom<-read.csv(file = 'results/MLD_anom_change.csv') %>% 
  left_join(island %>% distinct(island, region.col)) %>% 
  mutate(sig = ifelse(change_lower > 0, 'red', 'black')) %>% 
  filter(island %in% ime_df$island)

with(delta_anom, table(sig)) # 20 / 34 islands (59%)
delta_anom %>% filter(sig == 'red') %>% summarise(mean(change)) # 5.696 m 

## NCRMP vs. IME datasets
# temporal windows
plank_scaled %>% group_by(island) %>% reframe(n = n_distinct(year),
                                                range = max(year) - min(year)) %>% 
  ungroup() %>% summarise(median(n), median(range))

island %>% distinct(island, mld_amp) %>% dplyr::slice_min(mld_amp, n = 5)

# Island vs Island complex
ime_is<-mod_dat %>% distinct(island) %>% pull(island)
fish_is<-plank_scaled %>% distinct(island) %>% pull(island)

fish_is[fish_is %in% ime_is] # 29 fish islands in IME

fish_is[!fish_is %in% ime_is] # Islands not in IME because complex:
# "Ofu & Olosega" "Lanai"         "Molokai"       "Tinian"        "Aguijan"

ime_is[!ime_is %in% fish_is] # Islands not in fish because no bathymetry data:
# "Wake"     "Midway"   "Laysan"   "Johnston" "Necker"  

unique(c(fish_is, ime_is))

# LOAD 
source('0_loads/00_crep_metabolic.R')

# basic model fitting planktivore metabolic by island and biophysical covariates

priors <- c(
  prior(normal(0, 0.5), class = "Intercept"),   # shrink intercept toward 0 on log-scale
  prior(normal(0, 0.5), class = "b"),           # shrink slopes modestly
  prior(exponential(1), class = "sd")           # penalize large sd; mean ~1, P(s>~3) small
)

# check vif. mean chl-a is correlated with reef area and MLD amp
car::vif(glm(log(planktivore_metab) ~ 
               land_area_km2 + avg_monthly_mm +
               reef_area_km2 + 
               site_bathy_400m + 
               population_status +
               # mean_chlorophyll +
               ted_mean + 
                mld_mean, data=plank_scaled %>% filter(planktivore_metab>0)))


# 1. Planktivore
# model N = 4294 [2009-2024].   n=3589 with hard coral
m2_plank<-brm(planktivore_metab ~ 
                    geomorphic_type + reef_area_km2 + island_area_km2 + 
                    site_bathy_400m + 
                    hard_coral + 
                    depth_m +
                    avg_monthly_mm + 
                    mld_mean + 
                    population_status +
                    ted_mean +
                    # precip_amp_mm +
                    # mean_chlorophyll +
                    (1 | year ) + # also tested year slopes but not supported by loo
                    (1 | island),
                  # family = lognormal(link = 'identity'),
                    family = hurdle_lognormal(),
        data = plank_scaled,
        prior = priors,
        # backend = "cmdstanr",
        chains = 3, iter = 2000, warmup = 500, cores = 4)

save(m2_plank, plank_scaled, file = 'results/mod_planktivore_metabolic.rds')

load('results/mod_planktivore_metabolic.rds')
checker<-m2_plank
summary(checker)
pp_check(checker, resp = 'planktivore_metab')
conditional_effects(checker)
ranef(checker)
bayes_R2(checker, re.form=NA) 
# metabolic = 51.1% [ran + fix]
# metabolic = 44.6% [fix]
# biomass = 14.7% [now deleted]

### OUTPUTS

# For linear model, extract posterior draws
effects <- checker %>%
  gather_draws(`b_.*`, regex=TRUE) %>%  
  # filter(.variable != 'b_Intercept') %>% 
  mutate(.variable = str_replace_all(.variable, 'b_', ''),
         var_fac = factor(.variable, 
                          levels = rev(c('Intercept', 'geomorphic_typeIsland','reef_area_km2','island_area_km2',
                                         'avg_monthly_mm', 'population_statusU',
                                         'site_bathy_400m', 'hard_coral', 'depth_m',
                                         # 'chl_a_mg_m3_mean'
                                        'ted_mean', 'mld_mean')))) %>% 
  filter(!is.na(var_fac)) %>% 
  group_by(var_fac) %>% mutate(medi = abs(median(.value)))

# Plot effect sizes
pdf(file = 'fig/ime_crep/crep_model_planktivore_effects.pdf', height=3, width=6)
ggplot(effects, aes(x = .value, y = var_fac)) +
  stat_halfeye(.width = c(0.5, 0.95)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Effect size", y = "") +
  xlim(c(-1.2, 1.2))
dev.off()

## Change in planktivore flux along MLD

conditional_effects(checker, c('mld_mean')) %>% 
  plot(rug=TRUE)

# Extract posterior samples
nd<-plank_scaled %>%  
  data_grid(geomorphic_type = 'Atoll',
            population_status = 'U',
            depth_m = 0,
            site_bathy_400m = 0,
            hard_coral = 0,
            avg_monthly_mm = 0,
            reef_area_km2 = 0,
            island_area_km2 = 0,
            mld_mean = seq_range(mld_mean, n = 1000))  %>% 
  mutate(mld_mean_raw = seq_range(plank$mld_mean, n = 1000)) %>%
  add_epred_draws(m2_plank, ndraws = 1000, re_formula = NA) 

med<-nd %>% mutate(mld_mean = round(mld_mean_raw, 1)) %>% 
  group_by(mld_mean) %>% summarise(med = median(.epred),
                                  upper = quantile(.epred, 0.975))

ann<-island %>% select(region.col, island, mld_mean) %>% mutate(mld_mean = round(mld_mean,1)) %>% 
  left_join(med)

pdf(file = 'fig/ime_crep/mld_planktivore.pdf', height=4, width=7)
ggplot(nd, aes(x = mld_mean_raw, y = median)) +
  geom_point(data = ann, aes(x = mld_mean, y = upper, col=region.col)) +
  geom_line(data = ann, aes(x = mld_mean, y = 18 + 0.4 * (as.numeric(as.factor(region.col)) - 1), col=region.col, group=region.col), 
            position = position_dodge(width=0.5), linewidth=1.5) +
  geom_text(data = ann, size=2, angle=90, hjust=0,
            aes(x = mld_mean, y = upper+0.5, col=region.col, label=island)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5, show.legend=F, fill = fg_cols[2]) +
  scale_colour_identity() +
  guides(color='none') +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  labs(x = 'Mixed layer mean, m', y = 'Planktivore metabolic flux')
dev.off()





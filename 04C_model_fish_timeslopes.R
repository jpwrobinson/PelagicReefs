

load('results/mod_planktivore_metabolic.rds')
checker<-m2_plank

m2_slopes <- update(m2_plank, formula. = ~ . - (1|year) - (1|island) + 
                         (1 + year || island), cores=4)

m2_ints <- update(m2_plank, formula. = ~ . - (1|year) - (1|island) + 0 +
                    factor(island):factor(year), cores=4) # remove intercept to get all island-year combos

loo(m2_plank, m2_slopes, m2_ints) # m2_ints supported

# for fixed version
ids<-plank_scaled %>% mutate(island = str_replace_all(island, '\\ ', ''), 
                             id = paste(year, island)) %>% 
  distinct(id) %>% pull(id)

island_year_fixef<-fixef(m2_ints) %>%
  as_tibble(rownames = "term") %>%
  filter(str_detect(term, "island.*year")) %>%
  separate(term, c("island", "year"), sep = ":") %>%
  mutate(
    island = str_extract(island, "[A-Za-z0-9]+$"),
    island = str_replace(island, 'factorisland', ''),
    year = as.numeric(str_extract(year, "\\d+")),
    id = paste(year, island),
    sig = ifelse(Q2.5 > 0 & Q97.5 > 0, 'blue', 'black'),
    sig = ifelse(Q2.5 < 0 & Q97.5 < 0, 'red', sig)
  ) %>% 
  filter(id %in% ids)

g1<-ggplot(island_year_fixef, aes(year, Estimate, col=sig)) + 
  geom_hline(yintercept=0) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5)) + 
  labs(x = '', y = 'Posterior median', subtitle = 'Planktivore varying year-island') +
  facet_wrap(~island) +
  scale_colour_identity()


load('results/mod_herbivore_metabolic.rds')
checker<-m2_herb

m2_slopes <- update(m2_herb, formula. = ~ . - (1|year) - (1|island) + 
                      (1 + year || island), cores=4)

m2_ints <- update(m2_herb, formula. = ~ . - (1|year) - (1|island) + 0 +
                    factor(island):factor(year), cores=4)

loo(m2_herb, m2_slopes, m2_ints)

island_year_fixef<-fixef(m2_ints) %>%
  as_tibble(rownames = "term") %>%
  filter(str_detect(term, "island.*year")) %>%
  separate(term, c("island", "year"), sep = ":") %>%
  mutate(
    island = str_extract(island, "[A-Za-z0-9]+$"),
    island = str_replace(island, 'factorisland', ''),
    year = as.numeric(str_extract(year, "\\d+")),
    id = paste(year, island),
    sig = ifelse(Q2.5 > 0 & Q97.5 > 0, 'blue', 'black'),
    sig = ifelse(Q2.5 < 0 & Q97.5 < 0, 'red', sig)
  ) %>% 
  filter(id %in% ids)

g2<-g1 + island_year_fixef + labs(subtitle = 'Herbivore varying year-island')



pdf(file = 'fig/FigureSX_planktivore_time.pdf', height=7, width=12)
g1
dev.off()

pdf(file = 'fig/FigureSX_herbivore_time.pdf', height=7, width=12)
g2
dev.off()
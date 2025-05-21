library(tidyverse)
library(terra)
library(ggthemes)
library(sf)
library(ggrepel)

# Load the world map as an sf object
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_shifted <- st_transform(world, crs = "+proj=robin +lon_0=150")

theme_set(theme_classic())

# Is IME prevalence and strength correlated with the mean climatology NPP? [probably]
# Does the IME prevalence and strength predict fish biomass on uninhabited reefs?

# https://zenodo.org/records/6416130
ime<-rast('data/ime_layers_messie_2022/IME_database.nc')
islands<-read.csv('data/ime_layers_messie_2022/island_database.csv', skip = 6) %>% 
  janitor::clean_names()
names(islands)

chl_ime<-t(as.matrix(ime[[1]], wide=TRUE))
chl_ime<-as.data.frame(chl_ime)
colnames(chl_ime)<-month.abb[1:12]

# add meta data and pivot
chl_ime<-chl_ime %>% 
  mutate(island = islands$island_name,
         lon = islands$longitude,
         lat = islands$latitude,
         type = ifelse(islands$island_1_or_reef_0 == 1, 'Island', 'Reef'),
         island_area_km2 = islands$island_area_obtained_either_from_the_nunn_database_or_from_the_island_contour_km2,
         reef_area_km2 = islands$island_reef_area_calculated_from_gebco_500_m_resolution_as_the_area_of_pixels_above_30_m_depth_including_emerged_lands_km2) %>% 
  pivot_longer(Jan:Dec, names_to = 'month', values_to = 'chl_ime')

# chl_ime = Chl averaged within IME mask
# Chl_REF = Chl averaged within REF mask
# Chl_max = nearby max Chl (within 1 pixel of island mask) (Chl mg/m-3 ??)
# Chl_increase_nearby = (Chl_max - Chl_REF) / Chl_REF [% increase in Chl due to IME [3% average in dataset]]
# has_IME	=	1 if IME, 0 if not, NaN if undetermined (too many gaps near the island, does not happen on a climatology)
# keep_IME = NaN if no IME or merged IME, 0 if too many gaps within IME region (does not happen on a climatology), 1 otherwise
# mask_IME = cell containing the IME mask for each island (same size as Chl.chl)
# cChl = IME Chl contour

for(i in 2:18){
  var<-t(as.matrix(ime[[i]], wide=TRUE))
  var<-as.data.frame(var) %>% 
    pivot_longer(V1:V12, names_to = 'month', values_to = 'var')
  chl_ime[,7+i]<-var$var
  colnames(chl_ime)[7+i]<-names(ime[[i]])
}

# add IME seasonality and tons carbon estimates
chl_ime<-chl_ime %>% 
         mutate(
         has_IME = ifelse(has_IME ==1 & !is.na(keep_IME), 1, NA),
         # add logical flag for IME that has >10% Chl increase nearby
         has_IME_above10pct = ifelse(has_IME ==1 & Chl_increase_nearby>=0.1, 1, NA),
         # change to Chl_increase_nearby to percent
         Chl_increase_nearby = Chl_increase_nearby * 100,
         # Compute total chlorophyll increase (tons Chl/m (area_IME is in km²))
         total_chl_increase_tC_per_m = ifelse(has_IME ==1,(chl_ime - Chl_REF) * area_IME / 1e3, NA),
         total_chl_ime_tC_per_m = ifelse(has_IME ==1, chl_ime * area_IME / 1e3, NA)) %>% 
  select(-c(N_PHYSAT, Pielou_REF, Shannon_IME, Pielou_IME, braycurtis_IMEvsREF))

# Prevalence of IME 
# note does not match with Table 1 in Messie et al. 2022 because these are on IMEs, not islands
round(colSums(!is.na(chl_ime)) / dim(chl_ime)[1] * 100, 2) # 59.85% = prevalence of IME by month~island
round(colSums(chl_ime >= 0.1, na.rm=TRUE) / dim(chl_ime)[1] * 100, 2) # 21.01% = prevalence of IME > 10% by month-island


# IME avg values
chl_ime %>% group_by(island) %>% 
  summarise(across(c(chl_ime, Chl_REF, Chl_increase_nearby, Chl_max), ~ mean(., na.rm=TRUE))) %>% 
  ungroup() %>% 
  summarise(across(c(chl_ime, Chl_REF, Chl_increase_nearby, Chl_max), ~ mean(., na.rm=TRUE)))

## What is intra-annual / seasonal variation in IME?
## Note that average IME values are for months with IME detected (keep_IME & has_IME)
## Average chl-a values estimated using all months
seas<-chl_ime %>% 
  group_by(island, lon, lat, type, island_area_km2, reef_area_km2) %>% 
  mutate(chl_island = mean(Chl_max, na.rm=TRUE),
         cv_chl = sd(Chl_max, na.rm=TRUE)/mean(Chl_max, na.rm=TRUE) * 100,
         chl_ime = mean(Chl_max[which(keep_IME == 1)]),
         chl_no_ime = mean(Chl_max[which(is.na(keep_IME))])) %>% 
  filter(keep_IME == 1) %>% 
  group_by(island, lon, lat, type, island_area_km2, reef_area_km2, chl_island, cv_chl, chl_ime, chl_no_ime) %>% 
  summarise(cv_ime = sd(Chl_increase_nearby, na.rm=TRUE)/mean(Chl_increase_nearby, na.rm=TRUE) * 100, 
            mean_ime_percent = mean(Chl_increase_nearby, na.rm=TRUE), # mean IME relative to REF, %
            max_ime_percent = max(Chl_increase_nearby, na.rm=TRUE), # max IME relative to REF, %
            total_ime_chl_tCm = sum(total_chl_ime_tC_per_m, na.rm=TRUE), # total annual chl-a produced in IME 
            total_increase_chl_tCm = sum(total_chl_increase_tC_per_m, na.rm=TRUE), # total annual chl-a increase in IME 
            months_ime = n_distinct(month) # number of months with IME
            ) %>% 
  mutate(lat_neg = ifelse(lat > 0, lat*-1, lat),
         ime_diff = (chl_ime - chl_no_ime) / chl_no_ime * 100)

write.csv(chl_ime, file = 'island_ime_month_dat.csv', row.names=FALSE)
write.csv(seas, file = 'island_ime_dat.csv', row.names=FALSE)

seas %>% ungroup() %>% 
  reframe(across(c(mean_ime_percent, total_ime_chl_tCm, months_ime), ~ range(., na.rm=TRUE)))

seas %>% group_by(months_ime) %>% 
  summarise(across(c(mean_ime_percent, total_ime_chl_tCm), ~ mean(., na.rm=TRUE)),
            n = n_distinct(island))

seas %>% ungroup() %>% slice(which.min(mean_ime_percent))
seas %>% ungroup() %>% slice(which.max(mean_ime_percent)) %>% data.frame
seas %>% filter(months_ime < 6) %>% dim / 613 * 100 # 27% of islands with <6 months IME
seas %>% filter(months_ime == 12) %>% dim / 613 * 100 # 17% of islands with 12 months IME
seas %>% filter(ime_diff > 35 | ime_diff <= -35)

hist(seas$months_ime)  

pdf(file = 'fig/ime_db/ime_db_variables.pdf', height=5, width=7)

# Convert to sf object (WGS 84, EPSG:4326)
seas_sf <- st_as_sf(seas, coords = c("lon", "lat"), crs = 4326)
seas_sf <- st_transform(seas_sf, crs = "+proj=robin +lon_0=150")

ggplot() + 
  geom_sf(data = world_shifted, fill = "grey", color = "grey") +
  geom_sf(data = seas_sf,
          aes(col=months_ime), alpha=0.5, size=0.8) +
  scale_color_distiller(palette='Spectral') +
  theme_map() +
  theme(legend.position = 'bottom') +
  coord_sf(xlim = c(-3000000, 12000000), ylim = c(-3500000, 4000000)) +
  labs(subtitle = 'Seasonality in IME duration', colour = 'N. months')

ggplot() + 
  geom_sf(data = world_shifted, fill = "grey", color = "grey") +
  geom_sf(data = seas_sf %>% filter(!is.na(ime_diff)),
          aes(col=ime_diff), alpha=0.5, size=0.8) +
  scale_color_distiller(palette='Spectral') +
  theme_map() +
  theme(legend.position = 'bottom') +
  coord_sf(xlim = c(-3000000, 12000000), ylim = c(-3500000, 4000000)) +
  labs(subtitle = 'Non-IME periods can have higher chl-a than IME', colour = 'IME/non-IME, %')


ggplot(seas, 
       aes(months_ime, mean_ime_percent/100)) + 
  geom_point(alpha=0.5, aes(size = total_ime_chl_tCm)) +
  geom_text_repel(data = seas %>% filter(mean_ime_percent>2), 
                  aes(label = island), size=2) +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_y_continuous(labels=percent) +
  labs(x = 'Number of months IME present', y = 'IME: increase in chl-a relative to REF',
       size = 'IME chl\nTgC yr-1')


ggplot(seas %>% filter(chl_island < 1), 
       aes(months_ime, chl_island)) + 
  geom_point(alpha=0.5) + 
  geom_text_repel(data = seas %>% filter(chl_island < 1), 
                  aes(label = island), size=2) +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  labs(x = 'Number of months IME present', y = 'Island mean chl-a nearby',
       subtitle = 'IME duration does not influence average chl-a nearby islands and reefs')

ggplot(seas %>% filter(chl_island < 1),
       aes(chl_island, mean_ime_percent/100)) + 
  geom_point() +
  geom_text_repel(data = seas %>% filter(chl_island < 1),
                  aes(label = island), size=2) +
  scale_y_continuous(labels=percent) +
  labs(x = 'Climatology: mean maximum chl-a, mg/m3', 
       y = 'IME: increase in chl-a relative to REF',
       subtitle = 'IME is unrelated to climatological average chl-a',
       size = 'IME chl\nTgC yr-1')

ggplot(seas %>% filter(chl_island < 0.4), aes(chl_island, chl_ime)) + 
  geom_point() +
  geom_text_repel(aes(label = island), size=2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = 'Climatology: mean maximum chl-a, mg/m3', 
       subtitle = 'IME chl-a strongly correlated with average chl-a',
       y = 'IME: maximum chl-a, mg/m3')

ggplot(seas %>% filter(chl_ime < 1),
       aes(months_ime, lat_neg)) + 
  geom_point(aes(size = total_ime_chl_tCm))  +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  labs(x = 'Number of months IME present', y = 'Distance from equator',
       size =  'IME chl\nTgC yr-1', subtitle = 'Temporal IME variability occurs across Pacific Ocean')

dev.off()




# Qs for Messie:
# Why script estimates summary stats by IME , but supp data is by island?
# Have variables been interpreted correctly?

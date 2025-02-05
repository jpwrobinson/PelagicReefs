library(tidyverse)
library(terra)
library(ggthemes)
# Load the world map as an sf object
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

theme_set(theme_classic())

# Is IME prevalence and strength correlated with the mean climatology NPP? [probably]
# Does the IME prevalence and strength predict fish biomass on uninhabited reefs?

# https://zenodo.org/records/6416130
ime<-rast('data/ime_layers_messie_2022/IME_database.nc')
islands<-read.csv('data/ime_layers_messie_2022/island_database.csv', skip = 6)
dim(ime)
plot(ime[[2]])
names(ime[1])

chl_ime<-t(as.matrix(ime[[1]], wide=TRUE))
chl_ime<-as.data.frame(chl_ime)
colnames(chl_ime)<-month.abb[1:12]

# add meta data and pivot
chl_ime<-chl_ime %>% 
  mutate(island = islands$Island.name,
         lon = islands$Longitude,
         lat = islands$Latitude) %>% 
  pivot_longer(Jan:Dec, names_to = 'month', values_to = 'chl_ime')

# chl_ime = Chl averaged within IME mask
# Chl_REF = Chl averaged within REF mask
# Chl_max = nearby max Chl (within 1 pixel of island mask) (Chl mg/m-3 ??)
# Chl_increase_nearby = (Chl_max - Chl_REF) / Chl_REF [% increase in Chl due to IME [3% average in dataset]]
# has_IME	=	1 if IME, 0 if not, NaN if undetermined (too many gaps near the island, does not happen on a climatology)
# keep_IME = NaN if no IME or merged IME, 0 if too many gaps within IME region (does not happen on a climatology), 1 otherwise
# mask_IME = cell containing the IME mask for each island (same size as Chl.chl)

for(i in 2:18){
  var<-t(as.matrix(ime[[i]], wide=TRUE))
  var<-as.data.frame(var) %>% 
    pivot_longer(V1:V12, names_to = 'month', values_to = 'var')
  chl_ime[,4+i]<-var$var
  colnames(chl_ime)[4+i]<-names(ime[[i]])
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
         total_chl_increase_tC_per_m = ifelse(has_IME ==1,
                (chl_ime - Chl_REF) * area_IME / 1e3, NA)) %>% 
  group_by(island) %>% 
  mutate(n_month_ime = n_distinct(month[!is.na(Chl_increase_nearby)])) %>% 
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

chl_ime %>% group_by(island) %>% 
  summarise(across(c(total_chl_increase_tC_per_m), ~ sum(., na.rm=TRUE))) %>% 
  ungroup() %>% 
  summarise(across(c(total_chl_increase_tC_per_m), ~ mean(., na.rm=TRUE)))


## What is intra-annual / seasonal variation in IME?
seas<-chl_ime %>% 
  filter(has_IME == 1) %>% 
  group_by(island, lon, lat) %>% 
  summarise(cv_ime = sd(Chl_increase_nearby, na.rm=TRUE)/mean(Chl_increase_nearby, na.rm=TRUE) * 100, 
            mean_ime_percent = mean(Chl_increase_nearby, na.rm=TRUE),
            max_ime_percent = max(Chl_increase_nearby, na.rm=TRUE),
            max_chl = mean(Chl_max, na.rm=TRUE),
            total_ime_chl_tCm = sum(total_chl_increase_tC_per_m, na.rm=TRUE),
            months_ime = n_distinct(month[!is.na(has_IME)])
            )

write.csv(chl_ime, file = 'island_ime_month_dat.csv', row.names=FALSE)
write.csv(seas, file = 'island_ime_dat.csv', row.names=FALSE)

seas %>% ungroup() %>% 
  reframe(across(c(mean_ime_percent, total_ime_chl_tCm, months_ime), ~ range(., na.rm=TRUE)))

seas %>% group_by(months_ime) %>% 
  summarise(across(c(mean_ime_percent, total_ime_chl_tCm), ~ mean(., na.rm=TRUE)),
            n = n_distinct(island))

seas %>% ungroup() %>% slice(which.min(mean_ime_percent))
seas %>% ungroup() %>% slice(which.max(mean_ime_percent)) %>% data.frame
seas %>% filter(months_ime < 6) %>% dim / 613 * 100
seas %>% filter(months_ime == 12) %>% dim / 613 * 100


hist(seas$months_ime)  

ggplot(seas %>% filter(mean_ime_percent > 10), 
       aes(cv_ime, mean_ime_percent, size = total_ime_chl_tCm)) + geom_point()

ggplot() + 
  # geom_sf(data = world, fill = "lightblue", color = "black") +
  geom_point(data = seas %>% filter(mean_ime_percent > 1), 
             aes(lon, lat, col=max_chl), alpha=0.5) +
  scale_color_distiller(palette='Spectral') +
  theme_minimal()


# Qs for Messie:
# Why script estimates summary stats by IME , but supp data is by island?
# Have variables been interpreted correctly?

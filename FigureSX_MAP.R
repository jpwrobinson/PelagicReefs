library(tidyterra)
library(sf)
library(terra)

source='MODIS-1'
source('0_loads/00_crep_metabolic.R')
load(file = 'results/mod_ime.rds')


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(crs = "+proj=longlat +lon_0=180 +datum=WGS84")

island_ime<-mod_dat %>% distinct(island, REGION, region.col) %>% 
  mutate(data = 'IME', region = REGION) %>% 
  left_join(island %>% mutate(lon = longitude, lat = latitude) %>% 
              select(island, island_code, lon, lat))

island_crep<-depth %>% distinct(island, region) %>% 
  left_join(island %>% 
              mutate(lon = longitude, lat = latitude) %>% 
              select(island, island_code, lat, lon, region.col)) %>% 
  mutate(data = 'Fish NCRMP')

islands_sf <- rbind(island_crep, island_ime %>% select(names(island_crep))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

islands_lab <- rbind(island_crep, island_ime %>% select(names(island_crep))) %>%   
  mutate(lon = ifelse(lon < 0, lon+180, lon-180)) %>%
  distinct(island, island_code, lon, lat) 


gMap<-ggplot() +
  geom_sf(data = world, fill = "grey80", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = islands_sf, size = 2, aes(fill=region.col), pch=21, col='black') +
  geom_text_repel(
    data = islands_lab, aes(lon, lat, label = island_code),
    size = 2.5, colour = "black", box.padding = 0.3, max.overlaps = Inf
  ) +
  coord_sf(
    xlim = c(-42, 30),   # ~130°E to 70°W in 0-360 space
    ylim = c(-20, 30),    # tropical Pacific
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = seq(140, 240, by = 20)
  ) +
  theme_minimal() + guides(col=FALSE) +
  theme(
    panel.background = element_rect(fill = "#d6eaf8"),  # ocean colour
    panel.grid = element_line(colour = "white", linewidth = 0.3),
    panel.border = element_rect(colour = "black", linewidth = 0.8)
  ) +
  labs(x = NULL, y = NULL) 

pdf(file = 'fig/FigureSX_map.pdf', height=4, width=7)
gMap
dev.off()
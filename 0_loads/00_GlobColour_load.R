
library(sf)
library(terra)
library(tidyverse)
library(ncdf4)
source('0_loads/00_islands.R')

# open the file
nc <- nc_open('data/GlobColour/IME_database_GlobColour_monthly_NOAAislands.nc')

# list all dimensions
nc$dim

var_names <- names(nc$var)

# print long names for each variable
for (v in var_names) {
  longname <- ncatt_get(nc, v, "long_name")$value
  cat(v, ":", longname, "\n")
}

# island_number : Island number (Messié et al., 2022 database) 
# island_name : Island name (Messié et al., 2022 database) 
# island_latitude : Island position (latitude) 
# island_longitude : Island position (longitude) 
# is_emerged : Island type (0 = submerged reef, 1 = emerged island) 
# cChl : Chl contour delineating the IME region 
# Chl_max : Chl 99th percentile next to the island 
# Chl_IME : Chl averaged within IME region 
# Chl_REF : Chl averaged within REF region 
# Chl_increase_nearby : Proportion of Chl increase next to the island (strength_IME) relative to Chl_REF 
# strength_IME : IME strength, defined as Chl increase next to the island (Chl_max - Chl_REF) 
# area_IME : IME area 
# has_IME : 1 if belonging to an IME, 0 otherwise, NaN if too many clouds next to the island (no seasonal instance) 
# keep_IME : NaN if no IME of if belonging to a shared IME (lead by another island), 0 if more than 60% gaps in chl within the IME region (no seasonal instance), 1 otherwise 

# extract time and convert from base (days since 1970)

# 1️⃣ extract dimensions
islands <- ncvar_get(nc, "island_name")             # ~34 islands
lon <- ncvar_get(nc, "island_longitude")             # ~34 islands
lat <- ncvar_get(nc, "island_latitude")             # ~34 islands
time <- ncvar_get(nc, "time")           # raw numeric
nc$dim$time$units
origin <- as.Date('1970-01-01')
dates <- origin + time


# 2️⃣ list all variables you want (all 9 layers)
var_names <- names(nc$var)[-c(1:5)]

# 3️⃣ loop over variables and build long table
df_list <- lapply(var_names, function(var){
  
  # extract variable: dimensions = island x time
  df <- ncvar_get(nc, var) %>% as.data.frame()
  
  # convert to data.frame
  colnames(df) <- as.character(dates)        # column names = actual dates
  df$island <- islands                        # add island column
  df$lon <- lon                        # add island column
  df$lat <- lat                        # add island column
  
  # pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = -c(island,lon,lat),
      names_to = "date",
      values_to = "value"
    ) %>%
    mutate(
      date = as.Date(date),
      variable = var
    ) %>%
    select(island, date, variable, value)
  
  return(df_long)
})

# 4️⃣ combine all variables into one tidy data.frame
ime_df <- bind_rows(df_list) %>% 
  mutate(value = as.numeric(value),
         month = month(date), year = year(date)) %>% 
  pivot_wider( id_cols = c(island, date, month, year), names_from = variable, values_from=value) %>% 
  group_by(island) %>% 
  mutate(chl_max_mean = mean(Chl_max, na.rm=TRUE)) %>% ungroup() %>% 
  group_by(island, month) %>% 
  mutate(chl_max_monthly_mean = mean(Chl_max, na.rm=TRUE)) %>% ungroup() %>% 
  mutate(
    chl_max_anom = (Chl_max - chl_max_mean) / chl_max_mean, # anomaly for that island
    chl_max_month_anom = (Chl_max - chl_max_monthly_mean) / chl_max_monthly_mean, ## anomaly for that island-month
    island_messie = island,
    island = trimws(str_replace_all(island, 'Atoll', '')),
    island = trimws(str_replace_all(island, 'Island', '')),
    island = trimws(str_replace_all(island, 'Reef', '')),
    island = trimws(str_replace_all(island, '\\ and', '\\ &')),
    island = case_match(island, 
                        'Hawai’i' ~ 'Hawaii',
                        'French Frigate Shoals' ~ 'French Frigate',
                        'Kaua’i' ~ 'Kauai',
                        'Ni’ihau' ~ 'Niihau',
                        'O’ahu' ~ 'Oahu',
                        'Swains  (Olohega)' ~ 'Swains',
                        'Ta’u' ~ 'Tau', .default = island)) %>% 
  left_join(region_df, by = 'island') %>% 
  left_join(island_cols)

write.csv(ime_df, file = 'data/GlobColour/GlobColour_IME_output.csv', row.names=FALSE)

island.vec<-ime_df %>% distinct(island) %>% pull(island)

pdf(file = 'fig/ime_db/ime_globcol_timeseries.pdf', height=7, width=12)

for(i in 1:length(island.vec)){
  
    g1<-ggplot(ime_df %>% filter(island == island.vec[i]), aes(date, Chl_max)) + 
      geom_line(col='grey') + geom_point() +
    labs(x = '', subtitle = island.vec[i])
    
    g2<-ggplot(ime_df %>% filter(island == island.vec[i]), aes(date, Chl_increase_nearby)) + 
      geom_line(col='grey') + geom_point() +
      scale_y_continuous(labels=label_percent()) +
      labs(x = '')
    
    g3<-ggplot(ime_df %>% filter(island == island.vec[i]) %>% 
                mutate(dir=ifelse(chl_max_anom>0, 'pos', 'neg')), aes(date, chl_max_anom)) + 
      geom_hline(yintercept = 0, col = 'grey', alpha=0.5) +
      geom_line(col='grey') + geom_point(aes(colour = dir)) +
      scale_color_manual(values= c('neg' = "#2C7BB6", 'pos' = "#FDAE61")) +
      labs(x = '', y = 'Chl_max anomaly') + guides(colour='none')
    
    print(plot_grid(g1, g2, g3, nrow=3))
    rm(g1, g2, g3)
}
dev.off()

# NA cases tend to refer to rows where cChl was NA. This indicates there is no IME for XYZ???
# how many Chl_max are NA?
ime_df %>% filter(is.na(Chl_max)) # 3 months in 1998 for Swains
ime_df %>% filter(is.na(Chl_increase_nearby)) # 3,426 month~island combos

label_df <- ime_df %>%
  group_by(island) %>%
  summarise(
    date = min(date),
    chl_max_anom = 0.25
  )

pdf(file = 'fig/ime_db/ime_ChlMax_anomaly.pdf', height=7, width=12)
ggplot(ime_df %>% mutate(dir=ifelse(chl_max_anom>0, 'pos', 'neg')), 
       aes(date, chl_max_anom, col=region.col, #alpha=dir, 
           group=island)) + 
  geom_hline(yintercept = 0, col = 'grey', alpha=0.5) +
  geom_line() + 
  geom_text(data = label_df, aes(date, chl_max_anom, label = island), inherit.aes = FALSE, hjust = 0, size = 3) +
  scale_colour_identity() + 
  # scale_alpha_manual(values = c('neg' = 0.5, 'pos' = 1)) +
  facet_wrap(~island) +
  labs(y = 'Chl_max anomaly, (Chl_max - mean(Chl_max))', x= '')+
  scale_y_continuous(limits=c(-.15, 0.28), expand=c(0,0)) +
  scale_x_date(sec.axis=dup_axis()) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.minor = element_blank()
  )
dev.off()


# look at changes in seasonality
imey<-ime_df %>% group_by(island, region, region.col, year) %>% 
  summarise(cv = sd(Chl_max) / mean(Chl_max),
            amp = max(Chl_max) - min(Chl_max))

ggplot(imey, aes(year, cv, col=region.col, group=island)) + geom_line() + facet_wrap(~region) +
  scale_colour_identity()

ggplot(imey, aes(year, amp, col=region.col, group=island)) + geom_line() + facet_wrap(~region) +
  scale_colour_identity()

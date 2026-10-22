
library(sf)
library(terra)
library(tidyverse)
library(ncdf4)
library(mgcv)
library(gratia)
theme_set(theme_bw())

nc <- nc_open('data/GlobColour/IME_database_GlobColour_monthly_wMLD.nc')

islands<-read.csv('data/ime_layers_messie_2022/island_database.csv', skip = 6) %>% 
  janitor::clean_names()

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
# island_type : Island type according to Nunn et al. (2016) 
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
# has_IME : 1 if generating an IME, 0 otherwise, NaN if too many clouds next to the island (no seasonal instance) 
# is_primaryIME : 1 if the IME is primary, 0 if sub-IME 
# keep_IME : NaN if no IME of if generating a shared IME (lead by another island), 0 if more than 60% gaps in chl within the IME region (no seasonal instance), 1 otherwise 
# is_badArea :  
#   deltaSST :  
#   curr : Current 
# precip :  
#   tau :  
#   swh :  
#   mld : Mixed layer depth

# 1️⃣ extract dimensions
island_names <- islands$island_name # ~664 islands loaded from NatGeosci dataset
lon <- ncvar_get(nc, "island_longitude")
lat <- ncvar_get(nc, "island_latitude")
time <- ncvar_get(nc, "time")           # raw numeric

var_names <- names(nc$var)[-c(1:6)]

# 3️⃣ loop over variables and build long table
df_list_gc <- lapply(var_names, function(var){
  
  # extract variable: dimensions = island x time
  df <- ncvar_get(nc, var) %>% as.data.frame()
  
  # convert to data.frame
  colnames(df) <- as.character(time)        # column names = days since Jan 1st, 1970
  df$island <- island_names                        # add island column
  df$lon <- lon                        # add island column
  df$lat <- lat                        # add island column
  
  # pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = -c(island,lon,lat),
      names_to = "time",
      values_to = "value"
    ) %>%
    mutate(
      # month_name = month.abb[month],
      variable = var
    ) %>%
    select(island, time, variable, value)
  
  return(df_long)
})

# 4️⃣ combine all variables into one tidy data.frame
ime_df_gc <- bind_rows(df_list_gc) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider( id_cols = c(island, time), names_from = variable, values_from=value) %>% 
  mutate(time = as.numeric(time), 
         date = as.Date(time, origin = "1970-01-01"),
         year = as.numeric(year(date)),
         month = as.numeric(month(date)))

# write.csv(ime_df_gc, file = 'data/GlobColour/IME_climato_GlobColour_MODIS_allPacific.csv', row.names=FALSE)

## Test models
focal<-ime_df_gc %>% 
  mutate(time_s = scale(time)[,1],
         mld_s = scale(mld),
         tau_s = scale(tau),
         swh_s = scale(swh),
         precip_s = scale(precip),
         curr_s = scale(curr),
         deltaSST_s = scale(deltaSST),
         island = factor(island)) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))

focal2<-ime_df_gc %>% 
  filter(!is.na(strength_IME) & strength_IME != 0) %>% 
  mutate(strength_IMElog = log(strength_IME),
         time_s = scale(time)[,1],
         mld_s = scale(mld),
         tau_s = scale(tau),
         swh_s = scale(swh),
         precip_s = scale(precip),
         curr_s = scale(curr),
         deltaSST_s = scale(deltaSST),
         island = factor(island)) %>% 
  arrange(island, time_s) %>%
  mutate(new_series = c(TRUE, diff(as.numeric(island)) != 0))

## 1. Fit binomial version = on/off IME
# n = 225002 [758 dropped for NA]

m_detect <- mgcv::bam(
  has_IME ~ 
    s(month, bs = "cc", k = 6) +
    s(time_s, k = 15) +
    s(mld_s, k=5) + 
    s(swh_s, k=5) +
    s(tau_s, k=5) +
    s(precip_s, k=5) +
    s(curr_s, k=5) +
    # s(deltaSST_s, k=5) + 
    s(island, bs = 're'), # random intercept
    # s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability..too hard computationally
  family = binomial,
  data = focal,
  method   = "fREML",
  discrete = TRUE,
  select   = FALSE  
)
save(m_detect, file = 'results/ime_messie_pacific.rds')

# get resid for rho [can't use with binomial]
# r  <- resid(m_detect, type = "response")
# acf(r)  # or itsadug::start_value_rho(m0, plot = TRUE)
# rhoD <- itsadug::start_value_rho(m_detect)
# itsadug::acf_resid(m_detect)

summary(m_detect)
draw(m_detect)
acf(resid(m_detect))

m_strength <- mgcv::bam(
  strength_IMElog ~ 
    # s(month, bs = "cc", k = 6) +
    # s(time_s, k = 15) +
    # s(mld_s, k=5) +
    s(swh_s, k=5) + 
    s(swh_s, island, bs='fs', k=5, m=1),
    # s(tau_s, k=5) + 
    # s(precip_s, k=5) + 
    # s(curr_s, k=5) + 
    # s(deltaSST_s, k=5) +
  # s(time_s, by = island, bs = "cr", k = 10)),   # island-level probability..too hard computationally
  data = focal2,
  # AR.start = focal2$new_series,
  # rho = rhoS, 
  method   = "fREML",
  discrete = TRUE
)

# get resid for rho
r  <- resid(m_strength, type = "response")
acf(r)  # or itsadug::start_value_rho(m0, plot = TRUE)
rhoS <- itsadug::start_value_rho(m_strength)
itsadug::acf_resid(m_strength)

summary(m_strength)
hist(resid(m_strength))
draw(m_strength)
acf(resid(m_strength))


## viz the island smooths for swh

rng <- focal2 |> group_by(island) |>
  summarise(lo = quantile(swh_s, .02), hi = quantile(swh_s, .98), n = n())

grid <- rng |> rowwise() |>
  reframe(island = island, swh_s = seq(lo, hi, length.out = 40))

grid$fit    <- exp(predict(m_strength, grid))                    # global + island
grid$global <- exp(predict(m_strength, grid, exclude = "s(swh_s,island)"))
grid$dev    <- grid$fit - grid$global                           # island deviation


# 1. spaghetti of island curves around the global mean
ggplot(grid, aes(swh_s, fit, group = island)) +
  geom_line(alpha = .15) +
  geom_line(aes(y = global), colour = "red", linewidth = 1,
            data = distinct(grid, swh_s, .keep_all = TRUE) |> arrange(swh_s))

# 2. deviations only (shape differences, level removed)
grid |> group_by(island) |> mutate(dev_c = dev - mean(dev)) |>
  ggplot(aes(swh_s, dev_c, group = island)) + geom_line(alpha = .15)

# 3. a few extreme and typical islands, faceted, with CIs
pr <- predict(m_strength, grid, se.fit = TRUE)

slope_i <- grid |> group_by(island) |>
  summarise(effect = fit[which.max(swh_s)] - fit[which.min(swh_s)])

ggplot(slope_i, aes(effect)) + geom_histogram()

slope_i %>% filter(effect > 0.01 | effect < -0.01)
42/658*100

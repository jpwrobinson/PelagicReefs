
library(tidyverse)
library(magrittr)
library(patchwork)


# Climatological covariates from Gove et al. 2013
oce <-readxl::read_excel('data/crep_oceanographic/Gove2013_pone.0061974.s005.xlsx', sheet=2) %>% 
  clean_names() %>% 
  mutate(island = recode(island, 'French Frigate Shoals' = 'French Frigate'))

# Reading processed CREP data 
crep_full <- read.csv('data/noaa-crep/crep_full_merged.csv')

# Calculating routine metabolic rates as per Barneche et al 2014 Ecol Lett
## 
# Tc = 20oC + 273.15 in K
# normmet is lnb0Tc, which is the log of the size-corrected metabolic rate at Tc = 20oC
# Er is the activation energy in eV
# k is Boltzmann constant in eV / K
# Barneche et al. (2014) find that Ea is not different from 0,
# thus the term Ea * ((1 / (k*T)) - (1 / (k * Tc))) = 0 and is not considered

routmet <- function (mass, temp, Tc = 293.15, alpha = 0.76, normmet = -5.714, Er = 0.589, 
                     k = 8.62e-05, Ei = 2.035, Topt = 306.31) {
  
  BoltzRel   <- (Er * ((1/(k*Tc)) - (1/(k*temp))))
  InnactTerm <- log(1 + (Er/(Ei - Er)) * exp(Ei * ((1 / (k * Topt)) - (1 / (k * temp)))))
  
  Bi <- normmet + (alpha * log (mass)) + BoltzRel - InnactTerm
  return(exp(Bi))
  
}


## Sea surface temperature for CREP-full
un_oce <- sort(unique(oce$island))
un_crep <- sort(unique(crep_full$ISLAND))

no_match <- un_crep[which(is.na(match(un_crep,un_oce)))] 
## no temp data for Kahoolawe. It is just next to Maui, so using Maui's data

sst_K_crep <- unlist(oce[match(crep_full$ISLAND,oce$island),'sst_mean']) + 273.15
sst_K_crep[crep_full$ISLAND == no_match] <- unique(sst_K_crep[crep_full$ISLAND == 'Maui'])
count_area_m2 <- (pi*(7.5^2))

## Converting to energy (kJ day^-1) and area units
## 1) assumes that each g C respired results in 3.66 g CO2 produced (co2_mol_mass / c_mol_mass)
## 2) assumes that one mol of C02 produced consumes 0.85 mol of O2 (resp_quot)
## 3) assumes that one mol of O2 corresponds to 22.4 L/mol
##    OBS: the effect of temperature on volume is disregarded because, although the
##         assumed value refers to STP, the energetic density below is already adjusted to
##         biologically relevant temperatures; so is the resp_quot above
## 4) assumes a caloric equivalent of oxygen of ~20.1 kJ per L of O2
energy_conv <- function(c_metab, c_mol_mass = 12.01, co2_mol_mass = 44.01,
                        resp_quot = 0.85, gas_vol_dens = 22.4, temp_stp = 273.15, oxy_cal_eq = 20.1){
 
  co2_mols <- (c_metab * (co2_mol_mass / c_mol_mass)/co2_mol_mass)  
  o2_vol <- co2_mols * (1/resp_quot) * gas_vol_dens
  energy_dens <- o2_vol * oxy_cal_eq
  return(energy_dens)

}

## Inputting all values back into crep_full
crep_full %<>% 
  mutate(
    ind_metab_gC_day = routmet(
      mass = body_mass_g,
      temp = sst_K_crep),
    metab_gC_day = ind_metab_gC_day * MEAN_COUNT,
    metab_kJ_day = energy_conv(metab_gC_day),
    metab_kJ_m2_day = metab_kJ_day / count_area_m2)
 

## Exploratory aggregation
crep_full |>
  distinct(OBS_YEAR, ISLAND,SECTOR,REEF_ZONE,SITEVISITID) |>
  group_by(ISLAND,SECTOR,REEF_ZONE) |>
  count() |>
  as.data.frame()

## A processed data frame to play

# ISLAND MEAN METABOLIC RATES
proc <- crep_full |>
  group_by(OBS_YEAR, REGION,ISLAND,SECTOR,REEF_ZONE,SITEVISITID,TROPHIC_BROAD) |>
  summarise(metab_kJ_m2_day = sum(metab_kJ_m2_day), .groups = 'drop') |>
  pivot_wider(names_from  = TROPHIC_BROAD, values_from = metab_kJ_m2_day) |>
  mutate(across(PISCIVORE:SECONDARY,\(x)ifelse(is.na(x),0,x))) |>
  rowwise() |>
  mutate(PLANK_rel = PLANKTIVORE / sum(c_across(PISCIVORE:SECONDARY))) |>
  group_by(REGION,ISLAND,OBS_YEAR) |>
  summarise(across(PISCIVORE:PLANK_rel, mean),.groups = 'drop')

# SITE SUM METABOLIC RATES
proc_reg <- crep_full |>
  group_by(OBS_YEAR, REGION,ISLAND,SECTOR,REEF_ZONE,SITEVISITID,TROPHIC_BROAD) |>
  summarise(metab_kJ_m2_day = sum(metab_kJ_m2_day), .groups = 'drop') |>
  pivot_wider(names_from  = TROPHIC_BROAD, values_from = metab_kJ_m2_day) |>
  mutate(
    across(PISCIVORE:SECONDARY,\(x)ifelse(is.na(x),0,x)),
    REGION = factor(REGION,c('MARIAN','SAMOA','PRIAs','NWHI','MHI'))) |>
  rowwise() |>
  mutate(PLANK_rel = PLANKTIVORE / sum(c_across(PISCIVORE:SECONDARY))) |>
  ungroup() |>
  arrange(REGION) |>
  mutate(ISLAND = factor(ISLAND,unique(ISLAND))) %>% 
  left_join(island_cols)

write.csv(proc, file = 'data/metabolic/crep_island_metabolic_rates.csv', row.names=FALSE)
write.csv(proc_reg, file = 'data/metabolic/crep_site_metabolic_rates.csv', row.names=FALSE)
  
## Temporal patterns in planktivore community metabolic rates
abs_comm_met <- 
  ggplot(data = proc) +
  aes(x = OBS_YEAR, y = PLANKTIVORE, group = ISLAND, colour = ISLAND) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_continuous(
    breaks = c(2009,2013,2017,2021,2024)
  ) +
  facet_wrap(~REGION, nrow= 1) +
  labs(
    x = 'Year',
    y = expression(kJ~m^-2~d^-1),
    title = "Planktivore metabolic rates"
  ) +
  theme_minimal() +
  theme(legend.position='none')
  
## Temporal patterns in planktivore relative community metabolic rates
rel_comm_met <- 
  ggplot(data = proc) +
  aes(x = OBS_YEAR, y = PLANK_rel, group = ISLAND, colour = ISLAND) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(2009,2013,2017,2021,2024)
  ) +
  facet_wrap(~REGION, nrow= 1) +
  labs(
    x = 'Year',
    y = 'Proportion',
    title = "Planktivore relative metabolic rates"
  ) +
  theme_minimal() +
  theme(legend.position='none')
  
## Combining plots
(temp_pk_met <- abs_comm_met / rel_comm_met)


## What's the overall relative importance of planktivores
## for commuity metabolic rates?
geom_mean <- function(x) exp(mean(log(x)))
gm_pkrel <- format(geom_mean(proc$PLANK_rel), digits = 2)
gm_pkrel_lab <- paste("italic(G)~'='~",gm_pkrel, "~'%'")

pk_rel_hist <-
  ggplot(data = proc) +
  aes(x = PLANK_rel) +
  geom_histogram(
    fill = "#A7C3D9",
    col = NA) +
  labs(
    x = 'Planktivore contribution to community metabolic rate',
    y = 'Value count') +
  geom_vline(
    xintercept = geom_mean(proc$PLANK_rel),
    colour = "#0D2447",
    linetype = 'dashed') +
  theme_minimal() +
  annotate(
    'text', 
    label = gm_pkrel_lab, 
    x = 0.4, y = 27,
    hjust = 0, parse = TRUE)

pk_rel_hist


## Also no time, a look at absolute planktivore metabolic rates across islands
isl_pk_met <- ggplot(proc_reg %>% filter(PLANKTIVORE>0)) +
  aes(x = ISLAND, y=PLANKTIVORE, colour = region.col) +
  geom_jitter(
    width = 0.15,
    alpha = 0.2
  ) +
  scale_colour_identity() + 
  scale_y_log10(
    minor_breaks = NULL,
    labels=scales::trans_format("log10", scales::math_format(10^.x)),
    sec.axis = dup_axis(labels=scales::trans_format("log10", scales::math_format(10^.x)))
  ) +
  coord_flip() +
  labs(x = '', y=expression(Planktivore~metabolic~rates~(kJ~m^-2~d^-1))) +
  theme(
    legend.position= 'none')

pdf(file = 'fig/planktivore_metabolic_island.pdf', height=9, width=5)
print(isl_pk_met)
dev.off()

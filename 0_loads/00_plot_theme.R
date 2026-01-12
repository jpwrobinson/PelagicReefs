library(tidyverse)
library(janitor)
library(modelr)
library(performance)
library(brms)
theme_set(theme_classic())
library(GGally)
library(ggrepel)
library(cowplot)
library(emmeans)
library(mgcv)
library(marginaleffects)
library(gratia)
library(patchwork)
source('pairs2.R')
# model packages
library(brms)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)



island_cols<-data.frame(region = c("Northwestern Hawaiian", "Hawaii", "Mariana", "Equatorial", "Samoa"),
                        REGION = c("NWHI", "MHI", "MARIAN", "PRIAs", "SAMOA"),
                        region.col = c('#349BEB', '#F05826', '#01A74F', '#FEB913', '#FCF20E'))

fg_cols<-c('Herbivore' = '#F79C00', 'Planktivore' = '#01579F')

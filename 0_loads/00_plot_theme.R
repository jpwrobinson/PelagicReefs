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
                        region.col = c('#349BEB', '#F05826', '#01A74F', '#FEB913', '#FCF20E'),
                        region.num = c(1,2,3,4,5))

fg_cols<-c('Herbivore' = '#FF8C00', 'Planktivore' = '#01579F')


chl_grad_cols <- c(
  '#332A82','#3838A2','#306ED9','#55B1AE','#B7BD71','#EDBA5F','#FECC56','#FEE053'
)

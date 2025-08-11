library(tidyverse)
library(modelr)
library(performance)
library(brms)
theme_set(theme_classic())
library(GGally)
library(ggrepel)
library(cowplot)
source('pairs2.R')

island_cols<-data.frame(region = c("Northwestern Hawaiian", "Hawaii", "Mariana", "Equatorial", "Samoa"),
                        REGION = c("NWHI", "MHI", "MARIAN", "PRIAs", "SAMOA"),
                        region.col = c('#349BEB', '#F05826', '#01A74F', '#FEB913', '#FCF20E'))


# model packages
library(brms)
library(scales)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)


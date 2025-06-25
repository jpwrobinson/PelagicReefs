theme_set(theme_classic())
library(GGally)
library(ggrepel)
library(cowplot)

island_cols<-data.frame(region = c("Northwestern Hawaiian", "Hawaii", "Mariana", "Equatorial", "Samoa"),
                        REGION = c("NWHI", "MHI", "MARIAN", "PRIAs", "SAMOA"),
                        region.col = c('#349BEB', '#F05826', '#01A74F', '#FEB913', '#FCF20E'))



### script enm - dismo ###

## variables selection - correlation ##

# mauricio vancine
# 22-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(caret)
library(corrr)
library(GGally)
library(raster)
library(rgdal)
library(tidyverse)
library(wesanderson)

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var/bioclim_v20")
dir()

# import variables e get data ---------------------------------------------
# list variables
ti <- dir(pattern = "_ne_brazil_res05.tif")
ti

# import rasters
var <- raster::stack(ti)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)

# plot
plot(var$bio01)

# extract values
var_da <- var %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  na.omit
var_da

# verify
head(var_da)
dim(var_da)

# correlation -------------------------------------------------------------
# directory
setwd("..")
dir.create("02_correlation") 
setwd("02_correlation")

# correlation
cor <- corrr::correlate(var_da, method = "spearman")
cor

# lower
cor_lower <- corrr::shave(cor)
cor_lower

# visualizate
cor_vis <- corrr::fashion(cor_lower)
cor_vis

# export
readr::write_csv(cor_vis, "correlation.csv")

# plot
plot_cor <- corrr::rplot(cor_lower, shape = 20, print_cor = TRUE, 
                         colors = wesanderson::wes_palette(name = "Zissou1", 10, type = "continuous"))
plot_cor

fluxes_cor <- corrr::network_plot(cor, min_cor = .7, 
                                  colors = wesanderson::wes_palette(name = "Zissou1", 10, type = "continuous"))
fluxes_cor

# export
ggsave(plot_cor, filename = "correlation.tiff", wi = 20, he = 15, 
       units = "cm", dpi = 300, comp = "lzw")
ggsave(fluxes_cor, filename = "correlation_fluxes.tiff", wi = 20, he = 15, 
       units = "cm", dpi = 300, comp = "lzw")

# select variables --------------------------------------------------------
# verify
corrr::as_matrix(cor) %>% 
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)

# correlated variables
fi_06 <- corrr::as_matrix(cor) %>% 
  caret::findCorrelation(cutoff = .6)
fi_06

# select
var_da_cor06 <- var_da %>% 
  dplyr::select(-fi_06)
var_da_cor06

# new correlation
cor06 <- corrr::correlate(var_da_cor06, method = "spearman")
cor06

# graphic
plot_cor06_des <- GGally::ggpairs(
  var_da_cor06 %>% dplyr::sample_n(1e3), 
  lower = list(continuous = wrap(ggally_points, pch = 21, color = "black", fill = "blue", size = 2, alpha = .7)),
  diag = list(continuous = wrap(ggally_barDiag, color = "gray10", bins = 10)),
  upper = list(continuous = wrap(ggally_cor, color = "black", size = 5, method = "spearman"))) +
  theme_bw() +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(size = 8, colour = "black"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13), 
        panel.grid.major = element_line(colour = "white"))

# export
ggsave(plot_cor06_des, filename = "correlation_07.tiff", wi = 20, he = 15, 
       units = "cm", dpi = 300, comp = "lzw")

# end ---------------------------------------------------------------------
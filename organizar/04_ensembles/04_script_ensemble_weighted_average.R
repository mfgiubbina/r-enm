### script ensemble - dismo - weighted average ###

# mauricio vancine
# 27-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(beepr)
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)
library(vegan)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/03_enms"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# list files
csv <- dir(pattern = ".csv$", recursive = TRUE)
csv

# import models
eva <- purrr::map_dfr(csv, readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# auc
auc_limit <- .75

# algorithms
alg <- c("bio", "dom", "mah", "glm", "gam", "mar", "ann", "cta", "rfr", "svm", "max")
alg

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  auc >= auc_limit, 
                  algorithm %in% alg)
  
  # import enms
  enm <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::mutate(file = paste0(i, "/", file)) %>% 
    dplyr::pull() %>% 
    raster::stack()

  # auc
  auc <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # standardization
  enm_st <- enm %>% 
    values %>% 
    vegan::decostand("range", na.rm = TRUE)

  # weighted average ensemble
  ens <- enm[[1]]
  ens[] <- apply(enm_st, 1, function(x){sum(x * auc) / sum(auc)})
  
  # export ensemble
  raster::writeRaster(ens, paste0(i, "/ensemble_weighted_average_", i, ".tif"), 
                      format = "GTiff", options = c("COMPRESS=DEFLATE"), overwrite = TRUE)
  
  # notification sound
  beepr::beep(3)
  
} 

# end ---------------------------------------------------------------------
# -------------------------------------------------------------------------
# ensemble - weighted average
# mauricio vancine - mauricio.vancine@gmail.com
# 17-07-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)
library(vegan)

# directory
path <- "/home/mude/data/gitlab/course-sdm"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("05_sdm_multiplo")

# list files
csv <- dir(pattern = "eval_", recursive = TRUE)
csv

# import models
eva <- purrr::map_dfr(csv, readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# auc
auc_limit <- .75

# algorithms
alg <- eva$algorithm %>% unique
alg

# ensemble
for(i in eva$species %>% unique){}
  
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
    dplyr::mutate(file = paste0(i, "/00_replicas/", file)) %>% 
    dplyr::pull() %>% 
    raster::stack()

  # auc
  auc <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # standardization
  print("Standardization can take a looong time...")
  enm_st <- enm %>% 
    values %>% 
    vegan::decostand("range", na.rm = TRUE)
  print("All right, finish!")

  # weighted average ensemble
  ens <- enm[[1]]
  ens[] <- apply(enm_st, 1, function(x){sum(x * auc) / sum(auc)})
  
  # directory
  setwd(path)
  dir.create("06_consenso_media_ponderada")
  setwd("06_consenso_media_ponderada")
  
  # export
  raster::writeRaster(x = ens, 
                      filename = paste0("consenso_media_ponderada_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      overwrite = TRUE)
  
} 

# end ---------------------------------------------------------------------
# -------------------------------------------------------------------------
# ensemble - weighted average
# mauricio vancine - mauricio.vancine@gmail.com
# 15-11-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)
library(tidyverse)
library(vegan)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 6)

# directory
path <- "/home/mude/data/gitlab/r-sdm"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("02_output")

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
  
# gcms
gcm <- c("ac", "cc", "hd", "ip", "mg", "mr")
gcm

# cenarios
ce <- c("present", "45bi50", "45bi70", "85bi50", "85bi70")
ce

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  auc >= auc_limit, 
                  algorithm %in% alg) %>% 
    dplyr::mutate(file_tif = stringr::str_replace(stringr::str_split(file, "[.]", simplify = TRUE)[, 1], "enm_", "sdm_"))
  
  # auc
  auc_i <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # list files
  sdm_i_f <- eva_i %>% 
    dplyr::select(file_tif) %>% 
    dplyr::pull()
  
  # directory
  setwd(path)
  paste0("02_output/", i, "/00_replicas/") %>% 
    setwd()
  
  # import
  sdm_i_r <- grep(paste(sdm_i_f, collapse = "|"), dir(pattern = ".tif$"), value = TRUE) %>% 
    raster::stack()
  
  # -------------------------------------------------------------------------
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  sdm_i_st <- tibble::tibble(cenarios = rep(ce, each = ncell(sdm_i_r)))
  
  for(j in gcm){
    
    # information
    print(paste("Standardization to GCM", j))
    
    sdm_i_r_gcm <- sdm_i_r[[c(grep("present", names(sdm_i_r)), grep(j, names(sdm_i_r)))]]
    
    sdm_i_gcm_alg_st <- NULL
    sdm_i_gcm_st <- tibble::tibble(cenarios = rep(ce, each = ncell(sdm_i_r)))
    
    for(k in alg){
      
      # information
      print(paste("Standardization to algoritm", k))
      
      sdm_i_r_gcm_alg <- sdm_i_r_gcm[[grep(k, names(sdm_i_r_gcm))]]
      sdm_i_r_gcm_alg_ce_val <- NULL
      
      for(c in ce){
        
        sdm_i_r_gcm_alg_ce <- sdm_i_r_gcm_alg[[grep(c, names(sdm_i_r_gcm_alg))]] %>% 
          raster::values() %>% 
          tibble::as_tibble() %>% 
          dplyr::bind_cols(cenarios = rep(c, nrow(.)), .)
        
        colnames(sdm_i_r_gcm_alg_ce) <- c("cenarios", paste0(j, "_", k, "_r0", 1:nlayers(sdm_i_r_gcm_alg[[grep(c, names(sdm_i_r_gcm_alg))]])))
        
        sdm_i_r_gcm_alg_ce_val <- rbind(sdm_i_r_gcm_alg_ce_val, sdm_i_r_gcm_alg_ce)
        
      }
      
      sdm_i_gcm_alg_st <- vegan::decostand(sdm_i_r_gcm_alg_ce_val[, -1], "range", na.rm = TRUE)
      sdm_i_gcm_st <- cbind(sdm_i_gcm_st, sdm_i_gcm_alg_st)
      
    }
    
    sdm_i_st <- cbind(sdm_i_st, sdm_i_gcm_st[, -1])
    
  }
  
  # -------------------------------------------------------------------------
  # weighted average ensemble -----------------------------------------------
  # present
  sdm_i_st_pres <- sdm_i_st %>% 
    dplyr::filter(cenarios == "present") %>% 
    dplyr::select(2:18)
  
  ens <- sdm_i_r[[1]]
  ens[] <- apply(sdm_i_st_pres, 1, function(x){sum(x * auc_i) / sum(auc_i)})
  
  plot(ens)
  
  # directory
  setwd(path)
  dir.create("04_ensemble_weighted_average")
  setwd("04_ensemble_weighted_average")
  
  # export
  raster::writeRaster(x = ens, 
                      filename = paste0("ensemble_weighted_average_", i, "_present"), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  # future
  for(c in ce[-1]){
    
    sdm_i_st_ce <- sdm_i_st %>%
      dplyr::filter(cenarios == c) %>% 
      dplyr::select(-1)
    
    ens <- sdm_i_r[[1]]
    ens[] <- apply(sdm_i_st_ce, 1, function(x){sum(x * rep(auc_i, times = 6)) / sum(rep(auc_i, times = 6))})
    
    # export
    raster::writeRaster(x = ens, 
                        filename = paste0("ensemble_weighted_average_", i, "_", c), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        progress = "text",
                        overwrite = TRUE)
    
  }
  
  print("All right, finish!")
  
} 

# end ---------------------------------------------------------------------
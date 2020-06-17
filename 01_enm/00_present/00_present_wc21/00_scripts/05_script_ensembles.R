#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-06-16
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
# raster::beginCluster(n = parallel::detectCores() - 1)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc21"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluation")

# import evaluations
eva <- dir(pattern = "00_evaluation_", recursive = TRUE) %>% 
  purrr::map_dfr(., col_types = cols(), readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# tss
tss_limit <- .5

# directories
setwd(path); dir.create("05_ensembles")

# ensemble
for(i in eva$species %>% unique){
  
  # ensemble ----------------------------------------------------------------
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  tss_spec_sens >= tss_limit)
  
  # tss
  tss_i <- eva_i %>% 
    dplyr::select(tss_spec_sens) %>% 
    dplyr::mutate(tss = (tss_spec_sens) ^ 2) %>% 
    dplyr::pull()
  
  # list files
  enm_i_f <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::pull()
  
  # directory
  setwd(path); setwd(paste0("03_enm/", i))
  
  # import
  enm_i_r <- dir(pattern = ".tif$") %>% 
    stringr::str_subset(paste(enm_i_f, collapse = "|")) %>% 
    raster::stack()
  
  # infos
  met <- stringr::str_split_fixed(names(enm_i_r), "_", 9)[, 4] %>% 
    unique
  
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  enm_i_st <- NULL
  
  for(j in met){
    
    # information
    print(j)
    
    # method selection
    enm_i_r_met_val <- enm_i_r[[grep(j, names(enm_i_r))]] %>% 
      raster::values()
    
    # standardization
    enm_i_met_st <- vegan::decostand(enm_i_r_met_val, "range", na.rm = TRUE)
    enm_i_st <- cbind(enm_i_st, enm_i_met_st)
    
  }
  
  # weighted average ensemble -----------------------------------------------
  # information
  print(paste("Weighted average ensemble to ", i))
  
  # directory
  setwd(path); setwd("05_ensembles"); dir.create(i); setwd(i)
  
  # weighted average ensemble
  ens <- enm_i_r[[1]]
  ens[] <- apply(enm_i_st, 1, function(x){sum(x * tss_i) / sum(tss_i)})
  plot(ens)
  
  # export
  raster::writeRaster(x = ens, 
                      filename = paste0("ensemble_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  } 

# end ---------------------------------------------------------------------
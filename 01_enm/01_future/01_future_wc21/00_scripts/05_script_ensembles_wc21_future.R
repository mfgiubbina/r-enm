#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-06-20
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
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc21"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluation")

# import evaluations
eva <- dir(pattern = "00_eval_", recursive = TRUE) %>% 
  purrr::map_dfr(., col_types = cols(), readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# tss
tss_limit <- .5

# directories
setwd(path); dir.create("05_ensembles")

# ensemble
for(i in eva$species %>% unique){
  
  # ensemble ---------------------------------------------------------------
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
  setwd(path); setwd(paste0("03_enms/", i))
  
  # import
  enm_i_r <- dir(pattern = ".tif$") %>% 
    stringr::str_subset(paste(enm_i_f, collapse = "|")) %>% 
    raster::stack()
  names(enm_i_r) <- stringr::str_replace(names(enm_i_r), "_present", "__present")
  
  # info
  gcm <- stringr::str_split_fixed(names(enm_i_r), "_", 8)[, 6] %>% 
    stringi::stri_remove_empty() %>% 
    unique
  
  met <- stringr::str_split_fixed(names(enm_i_r), "_", 9)[, 4] %>% 
    unique
  
  sce <- stringr::str_split_fixed(names(enm_i_r), "_", 7)[, 7] %>%
    unique %>% sort
  
  names(enm_i_r) <- stringr::str_replace(names(enm_i_r), "__present", "_present")
  
  # tss
  tss_i_gcm <- rep(tss_i, length(gcm))
  
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  enm_i_st <- list()
  
  for(g in gcm){
    
    # information
    print(paste("Standardization to GCM", g))
    
    enm_i_r_gcm <- enm_i_r[[c(grep("present", names(enm_i_r)), 
                              grep(paste0("_", g, "_"), names(enm_i_r)))]]
    
    enm_i_gcm_st <- list()
    
    for(m in met){
      
      # information
      print(paste("Standardization to algoritm", m))
      
      enm_i_r_gcm_met <- enm_i_r_gcm[[grep(m, names(enm_i_r_gcm))]]
      enm_i_r_gcm_met_sce_val <- NULL
      
      for(c in sce){
        
        enm_i_r_gcm_met_sce_t <- enm_i_r_gcm_met[[grep(c, names(enm_i_r_gcm_met))]]
        
        enm_i_r_gcm_met_sce <- enm_i_r_gcm_met_sce_t %>% 
          raster::values() %>% 
          tibble::as_tibble() %>% 
          dplyr::rename_all(~(paste0("enm_", i, "_", m, "_", 
                                     stringr::str_split(names(enm_i_r_gcm_met_sce_t), "_", simplify = TRUE)[, 5], 
                                     "_", g)))
        
        colnames(enm_i_r_gcm_met_sce)
        
        enm_i_r_gcm_met_sce <- dplyr::bind_cols(scenarios = rep(c, nrow(enm_i_r_gcm_met_sce)), enm_i_r_gcm_met_sce)
        
        enm_i_r_gcm_met_sce_val <- rbind(enm_i_r_gcm_met_sce_val, enm_i_r_gcm_met_sce)
        
      }
      
      enm_i_gcm_met_st <- vegan::decostand(enm_i_r_gcm_met_sce_val[, -1], "range", na.rm = TRUE)
      enm_i_gcm_st <- c(enm_i_gcm_st, enm_i_gcm_met_st)
      enm_i_gcm_st <- as.data.frame(enm_i_gcm_st)
      
    }
    
    enm_i_st <- c(enm_i_st, enm_i_gcm_st)
    enm_i_st <- as.data.frame(enm_i_st)
    
  }
  
  enm_i_st$scenarios <- enm_i_r_gcm_met_sce_val$scenarios
  enm_i_st
  
  # weighted average ensemble -----------------------------------------------
  # directory
  setwd(path); setwd("05_ensembles"); dir.create(i); setwd(i)
  
  # ensemble
  for(c in sce){
    
    # information
    print(paste("Weighted average ensemble to", i, "and", c))
    
    # selection
    enm_i_st_sce <- enm_i_st %>%
      dplyr::filter(scenarios == c) %>% 
      dplyr::select(-scenarios)
    
    # weighted average
    ens <- enm_i_r[[1]]
    ens[] <- apply(enm_i_st_sce, 1, function(x){sum(x * tss_i_gcm) / sum(tss_i_gcm)})
    plot(ens)
    
    # export
    raster::writeRaster(x = ens, 
                        filename = paste0("ens_", i, "_", c), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        progress = "text",
                        overwrite = TRUE)
    
  }
  
  print("All right, finish!")
  
}

# end ---------------------------------------------------------------------
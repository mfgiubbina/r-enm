#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-04-29
#' ---

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
raster::beginCluster(n = 2)

# directory
path <- "/home/mude/data/github/00_github_organizar/r-sdm/00_pragmatico/00_present"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluation")

# import evaluations
eva <- dir(pattern = "00_evaluation_", recursive = TRUE) %>% 
  purrr::map_dfr(., readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# auc
auc_limit <- .75

# directories
setwd(path); dir.create("05_ensembles")

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  auc >= auc_limit)
  
  # auc
  auc_i <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # list algorithms
  alg <- eva_i$algorithm
  alg
  
  # list files
  sdm_i_f <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::pull()
  
  # directory
  setwd(path); setwd(paste0("03_sdm/", i))
  
  # import
  sdm_i_r <- grep(paste(sdm_i_f, collapse = "|"), dir(pattern = ".tif$"), value = TRUE) %>% 
    raster::stack()
  
  
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  sdm_i_st <- NULL
  
  for(j in alg %>% unique){
    
    # algoritm selection
    sdm_i_r_alg_val <- sdm_i_r[[grep(j, names(sdm_i_r))]] %>% 
      raster::values()
    
    # standardization
    sdm_i_alg_st <- vegan::decostand(sdm_i_r_alg_val, "range", na.rm = TRUE)
    sdm_i_st <- cbind(sdm_i_st, sdm_i_alg_st)
    
  }
  
  # weighted average ensemble -----------------------------------------------
  # information
  print(paste("Weighted average ensemble to ", i))
  
  # weighted average ensemble
  ens <- sdm_i_r[[1]]
  ens[] <- apply(sdm_i_st, 1, function(x){sum(x * auc_i) / sum(auc_i)})
  plot(ens, col = viridis::viridis(10))
  
  # directory
  setwd(path); setwd("05_ensembles")
  
  # export
  raster::writeRaster(x = ens, 
                      filename = paste0("ensemble_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  
  # uncertainties - hierarchical anova --------------------------------------
  # information
  print(paste("Uncertainties to", i))
  
  # suitability
  sui <- sdm_i_st
  
  # hierarchical anova
  sui_ms <- NULL
  
  for(p in 1:nrow(sui)){
    
    sui_p <- as.numeric(sui[p, ])

    if(any(is.na(sui_p))){
      
      sui_ms <- rbind(sui_ms, rep(NA, 2))
      
    } else{
      
        lm_model <- lm(sui_p ~ alg)
        anova_model <- anova(lm_model)
        sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
    }
    
  }
  
  # column names
  nrow(sui_ms)
  colnames(sui_ms) <- c("algorithm", "residuals")
  
  # proportion of variance explained by each component
  sui_ms_sum <- apply(sui_ms, 1, sum)
  sui_ms_prop <- sui_ms/sui_ms_sum
  
  # mapping uncertainties
  unc_r <- sdm_i_r[[1]]
  unc <- raster::stack()
  
  for(r in 1:ncol(sui_ms_prop)){

      unc_r[] <- sui_ms_prop[, r]
      unc <- raster::stack(unc, unc_r)
  }
  
  # names
  names(unc) <- c("algorithm", "residuals")
  plot(unc, col = viridis::viridis(10))
  
  # table
  table_resume <- sui_ms_prop %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na() %>%
    tidyr::pivot_longer(cols = c("algorithm", "residuals"), names_to = "source", values_to = "values") %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(median = median(values) %>% round(3),
                     min = min(values) %>% round(3),
                     max = max(values) %>% round(3))

  readr::write_csv(table_resume, paste0("total_sum_squares_anova_", i, ".csv"))
  
  # export
  raster::writeRaster(x = unc, 
                      filename = paste0("uncertainties_", names(unc), "_", i), 
                      bylayer = TRUE,
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  print("All right, finish!")
  
} 

# end ---------------------------------------------------------------------
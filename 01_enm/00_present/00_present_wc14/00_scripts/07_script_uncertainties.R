#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-05-18
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)
library(wesanderson)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
# raster::beginCluster(n = parallel::detectCores() - 1)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc21"
setwd(path)
dir()

# weighted average ensemble  ----------------------------------------------
# directories
setwd("05_ensembles")

# species list
sp <- dir()
sp

# uncertainties -----------------------------------------------------------
for(i in sp){

  # information
  print(paste("Uncertainties to", i))
  
  # directory
  setwd(i)
  
  # suitability
  sui <- dir() %>% 
    raster::raster()
  
  # factors
  met_fac <- stringr::str_split_fixed(colnames(enm_i_st), "_", 9)[, 4]
  
  # hierarchical anova
  sui_ms <- NULL
  
  for(p in 1:nrow(sui)){
    
    print(paste0(round(p/nrow(sui)*100, 2), "%"))
    sui_p <- as.numeric(sui[p, ])
    
    if(any(is.na(sui_p))){
      
      sui_ms <- rbind(sui_ms, rep(NA, 2))
      
    } else{
      
      lm_model <- lm(sui_p ~ met_fac)
      anova_model <- anova(lm_model)
      sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
      
    }
    
  }
  
  # column names
  colnames(sui_ms) <- c("methods", "residuals")
  
  # proportion of variance explained by each component
  sui_ms_sum <- apply(sui_ms, 1, sum)
  sui_ms_prop <- (sui_ms/sui_ms_sum)*100
  
  # mapping uncertainties
  unc_r <- enm_i_r[[1]]
  unc <- raster::stack()
  
  for(r in 1:ncol(sui_ms_prop)){
    
    unc_r[] <- sui_ms_prop[, r]
    unc <- raster::stack(unc, unc_r)
  }
  
  # names
  names(unc) <- c("methods", "residuals")
  plot(unc)
  
  # directory
  setwd(path); setwd("07_uncertainties"); dir.create(i); setwd(i)
  
  # table
  table_resume <- sui_ms_prop %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na() %>%
    tidyr::pivot_longer(cols = c("methods", "residuals"), names_to = "source", values_to = "values") %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(median = median(values) %>% round(3),
                     min = min(values) %>% round(3),
                     max = max(values) %>% round(3),
                     mean = mean(values) %>% round(3),
                     sd = sd(values) %>% round(3))
  
  readr::write_csv(table_resume, paste0("uncertainties_mean_squares_anova_", i, ".csv"))
  
  # export
  raster::writeRaster(x = unc, 
                      filename = paste0("uncertainties_", names(unc), "_", i), 
                      bylayer = TRUE,
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
} 

# end ---------------------------------------------------------------------
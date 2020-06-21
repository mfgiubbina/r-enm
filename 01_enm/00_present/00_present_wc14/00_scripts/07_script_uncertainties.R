#' ---
#' title: ensemble - uncertainties
#' author: mauricio vancine
#' date: 2020-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)
library(progress)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = parallel::detectCores() - 1)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc14"
setwd(path)
dir()

# evaluations -------------------------------------------------------------
# directory
setwd("04_evaluations")

# import evaluations
eva <- dir(pattern = "00_table_eval", recursive = TRUE) %>% 
  purrr::map_dfr(., col_types = cols(), readr::read_csv)
eva

# uncertainties -----------------------------------------------------------
# tss
tss_limit <- .5

# directory
setwd(path); dir.create("07_uncertainties")

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Uncertainties to", i))
  
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
  
  # inf
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
  
  
  # uncertainties -----------------------------------------------------------
  # information
  print(paste("Uncertainties to", i))
  
  # suitability
  sui <- enm_i_st
  
  # factors
  met_fac <- stringr::str_split_fixed(colnames(enm_i_st), "_", 9)[, 4]
  
  # anova
  sui_ms <- NULL
  
  pb <- progress_bar$new(format = "anova: [:bar] (:percent)",
                         total = nrow(sui))
  
  for(p in 1:nrow(sui)){
    
    pb$tick()
    
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
  
  readr::write_csv(table_resume, paste0("00_unc_table_", i, ".csv"))
  
  # export
  raster::writeRaster(x = unc, 
                      filename = paste0("unc_", names(unc), "_", i), 
                      bylayer = TRUE,
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
} 

# end ---------------------------------------------------------------------
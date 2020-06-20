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
# raster::beginCluster(n = parallel::detectCores() - 1)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc14"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluations")

# import evaluations
eva <- dir(pattern = "00_table_eval_", recursive = TRUE) %>% 
  purrr::map_dfr(., readr::read_csv)
eva

# uncertainties -----------------------------------------------------------
# tss
tss_limit <- .5

# directories
setwd(path); dir.create("07_uncertainties")

# ensemble
for(i in eva$species %>% unique){
  
  # standardization ---------------------------------------------------------------
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
  names(enm_i_r) <- stringr::str_replace(names(enm_i_r), "_present", "__present")
  
  # info
  gcm <- stringr::str_split_fixed(names(enm_i_r), "_", 8)[, 6] %>% 
    stringi::stri_remove_empty() %>% 
    unique
  
  met <- stringr::str_split_fixed(names(enm_i_r), "_", 9)[, 4] %>% 
    unique
  
  cen <- stringr::str_split_fixed(names(enm_i_r), "_", 7)[, 7] %>%
    unique %>% sort
  
  names(enm_i_r) <- stringr::str_replace(names(enm_i_r), "__present", "_present")
  
  # tss
  tss_i_gcm <- rep(tss_i, length(gcm))
  
  # standardization
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
      enm_i_r_gcm_met_cen_val <- NULL
      
      for(c in cen){
        
        enm_i_r_gcm_met_cen_t <- enm_i_r_gcm_met[[grep(c, names(enm_i_r_gcm_met))]]
        
        enm_i_r_gcm_met_cen <- enm_i_r_gcm_met_cen_t %>% 
          raster::values() %>% 
          tibble::as_tibble() %>% 
          dplyr::rename_all(~(paste0("enm_", i, "_", m, "_", 
                                     stringr::str_split(names(enm_i_r_gcm_met_cen_t), "_", simplify = TRUE)[, 5], 
                                     "_", g)))
        
        colnames(enm_i_r_gcm_met_cen)
        
        enm_i_r_gcm_met_cen <- dplyr::bind_cols(cenarios = rep(c, nrow(enm_i_r_gcm_met_cen)), enm_i_r_gcm_met_cen)
        
        enm_i_r_gcm_met_cen_val <- rbind(enm_i_r_gcm_met_cen_val, enm_i_r_gcm_met_cen)
        
      }
      
      enm_i_gcm_met_st <- vegan::decostand(enm_i_r_gcm_met_cen_val[, -1], "range", na.rm = TRUE)
      enm_i_gcm_st <- c(enm_i_gcm_st, enm_i_gcm_met_st)
      enm_i_gcm_st <- as.data.frame(enm_i_gcm_st)
      
    }
    
    enm_i_st <- c(enm_i_st, enm_i_gcm_st)
    enm_i_st <- as.data.frame(enm_i_st)
    
  }
  
  enm_i_st$cenarios <- enm_i_r_gcm_met_cen_val$cenarios
  enm_i_st
  
  # uncertainties -----------------------------------------------------------
  
  # table
  table_resume <- NULL
  
  # ensemble
  for(c in cen){
    
    # information
    print(paste("Uncertainties to", i, "and", c))
    
    # selection
    enm_i_st_cen <- enm_i_st %>%
      dplyr::filter(cenarios == c) %>% 
      dplyr::select(-cenarios)
    
    # suitability
    sui <- enm_i_st_cen
    
    # factors
    met_fac <- stringr::str_split_fixed(names(enm_i_st_cen), "_", 9)[, 4] %>% as.factor()
    gcm_fac <- stringr::str_split_fixed(names(enm_i_st_cen), "_", 9)[, 6] %>% as.factor()
    
    # anova
    sui_ms <- NULL
    
    pb <- progress_bar$new(format = "anova: [:bar] (:percent)",
                           total = nrow(sui))
    
    for(p in 1:nrow(sui)){
      
      pb$tick()
      
      sui_p <- as.numeric(sui[p, ])
      
      if(any(is.na(sui_p))){
        
        sui_ms <- rbind(sui_ms, rep(NA, 4))
        
      } else{
        
        lm_model <- lm(sui_p ~ met_fac * gcm_fac)
        anova_model <- anova(lm_model)
        sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
        
      }
      
    }
    
    # column names
    colnames(sui_ms) <- c("methods", "gcms", "methods_gcms", "residuals")
    
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
    names(unc) <- c("methods", "gcms", "methods_gcms", "residuals")
    plot(unc)
    
    # directory
    setwd(path); setwd("07_uncertainties"); dir.create(i); setwd(i)
    
    # export
    raster::writeRaster(x = unc, 
                        filename = paste0("unc_", names(unc), "_", i, "_", c), 
                        bylayer = TRUE,
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        progress = "text",
                        overwrite = TRUE)
    
    # table
    table_resume_temp <- sui_ms_prop %>% 
      tibble::as_tibble() %>% 
      tidyr::drop_na() %>%
      tidyr::pivot_longer(cols = c("methods", "gcms", "methodsgcms", "residuals"), 
                          names_to = "source", values_to = "values") %>% 
      dplyr::group_by(source) %>% 
      dplyr::summarise(median = median(values) %>% round(3),
                       min = min(values) %>% round(3),
                       max = max(values) %>% round(3),
                       mean = max(values) %>% round(3),
                       max = max(values) %>% round(3)) %>% 
      dplyr::mutate(scenario = c, .after = source)
    table_resume <- dplyr::bind_rows(table_resume, table_resume_temp)
    
  }
  
  readr::write_csv(table_resume, paste0("00_table_unc_", i, ".csv"))
  
  print("All right, finish!")
  
}

# end ---------------------------------------------------------------------
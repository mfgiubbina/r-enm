#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2019-01-20
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
path <- "/home/mude/data/gitlab/r-sdm/00_pragmatico/01_future"
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
auc_limit <- .7

# gcms
gcm <- c("ac", "cc", "mr")
gcm

# cenarios
ce <- c("pres", "45_2050", "45_2070", "85_2050", "85_2070")
ce

# directories
setwd(path); dir.create("05_ensembles")

# ensemble
for(i in eva$species %>% unique){}
  
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
    dplyr::pull() %>% 
    rep(., times = length(gcm))
  
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
  
  sdm_i_st <- tibble::tibble(cenarios = rep(ce, each = ncell(sdm_i_r)))
  
  for(g in gcm){
    
    # information
    print(paste("Standardization to GCM", g))
    
    sdm_i_r_gcm <- sdm_i_r[[c(grep("pres", names(sdm_i_r)), grep(g, names(sdm_i_r)))]]
    
    sdm_i_gcm_alg_st <- NULL
    sdm_i_gcm_st <- tibble::tibble(cenarios = rep(ce, each = ncell(sdm_i_r)))
    
    for(a in alg %>% unique){
      
      # information
      print(paste("Standardization to algoritm", a))
      
      sdm_i_r_gcm_alg <- sdm_i_r_gcm[[grep(a, names(sdm_i_r_gcm))]]
      sdm_i_r_gcm_alg_ce_val <- NULL
      
      for(c in ce){
        
      sdm_i_r_gcm_alg_ce_t <- sdm_i_r_gcm_alg[[grep(c, names(sdm_i_r_gcm_alg))]]
      
        sdm_i_r_gcm_alg_ce <- sdm_i_r_gcm_alg_ce_t %>% 
          raster::values() %>% 
          tibble::as_tibble() %>% 
          dplyr::bind_cols(cenarios = rep(c, nrow(.)), .)
        
        colnames(sdm_i_r_gcm_alg_ce) <- c("cenarios", paste0(eva_i[eva_i$algorithm == a, "file"] %>% dplyr::pull(), "_", g))
        
        sdm_i_r_gcm_alg_ce_val <- rbind(sdm_i_r_gcm_alg_ce_val, sdm_i_r_gcm_alg_ce)
        
      }
      
      sdm_i_gcm_alg_st <- vegan::decostand(sdm_i_r_gcm_alg_ce_val[, -1], "range", na.rm = TRUE)
      sdm_i_gcm_st <- cbind(sdm_i_gcm_st, sdm_i_gcm_alg_st)
      
    }
    
    sdm_i_st <- cbind(sdm_i_st, sdm_i_gcm_st[, -1])
    
  }
  
  sdm_i_st

  # weighted average ensemble -----------------------------------------------
  # directory
  setwd(path); dir.create("05_ensembles"); setwd("05_ensembles")
  
  # ensemble
  for(c in ce){
    
    sdm_i_st_ce <- sdm_i_st %>%
      dplyr::filter(cenarios == c) %>% 
      dplyr::select(-1)
    
    ens <- sdm_i_r[[1]]
    ens[] <- apply(sdm_i_st_ce, 1, function(x){sum(x * auc_i) / sum(auc_i)})
    
    # export
    raster::writeRaster(x = ens, 
                        filename = paste0("ensemble_weighted_average_", i, "_", c), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        progress = "text",
                        overwrite = TRUE)
    
  }
  
  # uncertainties - hierarchical anova --------------------------------------
  # information
  print(paste("Uncertainties to", i))
  
  # suitability
  sui <- tibble::tibble(id = seq(ncell(sdm_i_r)))
  
  for(i in sdm_i_st$cenarios %>% unique){
    
    sui_t <- sdm_i_st[sdm_i_st$cenarios == i, -1]
    colnames(sui_t) <- paste0(colnames(sui_t), "_", i)
    sui <- cbind(sui, sui_t)
    
  }
  
  sui <- sui[, -1]
  
  s# factors
  time <- paste0(stringr::str_split(colnames(sui), "_", simplify = TRUE)[, 7], "_",
                 stringr::str_split(colnames(sui), "_", simplify = TRUE)[, 8])
  time
  
  method <- stringr::str_split(colnames(sui), "_", simplify = TRUE)[, 4]
  method
  
  gcm <- stringr::str_split(colnames(sui), "_", simplify = TRUE)[, 6]
  gcm

  # hierarchical anova
  sui_ms <- NULL
  
  for(p in 1:nrow(sui)){
    
    sui_p <- as.numeric(sui[p, ])

    if(any(is.na(sui_p))){
      
      sui_ms <- rbind(sui_ms, rep(NA, 4))
      
    } else{
      
          lm_model <- lm(sui_p ~ time + method %in% time + gcm %in% time)
          anova_model <- anova(lm_model)
          sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
    }
    
  }
  
  # column names
  head(sui_ms)
  dim(sui_ms)
  colnames(sui_ms) <- c("time", "method_time", "gcm_time", "residuals")
  
  # proportion of variance explained by each component
  sui_ms_sum <- apply(sui_ms, 1, sum)
  sui_ms_prop <- (sui_ms/sui_ms_sum)*100
  
  # mapping uncertainties
  unc_r <- sdm_i_r[[1]]
  unc <- raster::stack()
  
  for(r in 1:ncol(sui_ms_prop)){

      unc_r[] <- sui_ms_prop[, r]
      unc <- raster::stack(unc, unc_r)
  }
  
  # names
  names(unc) <- c("time", "method_time", "gcm_time", "residuals")
  plot(unc, col = viridis::viridis(10))
  
  # table
  table_resume <- sui_ms_prop %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na() %>%
    tidyr::pivot_longer(cols = c("time", "method_time", "gcm_time", "residuals"), names_to = "source", values_to = "values") %>% 
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
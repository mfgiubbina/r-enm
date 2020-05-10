#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-05-06
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 2)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future"
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
# tss
tss_limit <- .5

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
                  tss_spec_sens >= tss_limit)
  
  # tss
  tss_i <- eva_i %>% 
    dplyr::select(tss_spec_sens) %>% 
    dplyr::mutate(tss = (tss_spec_sens) ^ 2) %>% 
    dplyr::pull() %>% 
    rep(., times = length(gcm))
  
  # list algorithms
  alg <- eva_i$algorithm
  alg
  
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
  
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  enm_i_st <- tibble::tibble(cenarios = rep(ce, each = ncell(enm_i_r)))
  
  for(g in gcm){
    
    # information
    print(paste("Standardization to GCM", g))
    
    enm_i_r_gcm <- enm_i_r[[c(grep("pres", names(enm_i_r)), grep(g, names(enm_i_r)))]]
    
    enm_i_gcm_alg_st <- NULL
    enm_i_gcm_st <- tibble::tibble(cenarios = rep(ce, each = ncell(enm_i_r)))
    
    for(a in alg %>% unique){
      
      # information
      print(paste("Standardization to algoritm", a))
      
      enm_i_r_gcm_alg <- enm_i_r_gcm[[grep(a, names(enm_i_r_gcm))]]
      enm_i_r_gcm_alg_ce_val <- NULL
      
      for(c in ce){
        
        enm_i_r_gcm_alg_ce_t <- enm_i_r_gcm_alg[[grep(c, names(enm_i_r_gcm_alg))]]
        
        enm_i_r_gcm_alg_ce <- enm_i_r_gcm_alg_ce_t %>% 
          raster::values() %>% 
          tibble::as_tibble() %>% 
          dplyr::bind_cols(cenarios = rep(c, nrow(.)), .)
        
        colnames(enm_i_r_gcm_alg_ce) <- c("cenarios", paste0(eva_i[eva_i$algorithm == a, "file"] %>% dplyr::pull(), "_", g))
        
        enm_i_r_gcm_alg_ce_val <- rbind(enm_i_r_gcm_alg_ce_val, enm_i_r_gcm_alg_ce)
        
      }
      
      enm_i_gcm_alg_st <- vegan::decostand(enm_i_r_gcm_alg_ce_val[, -1], "range", na.rm = TRUE)
      enm_i_gcm_st <- cbind(enm_i_gcm_st, enm_i_gcm_alg_st)
      
    }
    
    enm_i_st <- cbind(enm_i_st, enm_i_gcm_st[, -1])
    
  }
  
  enm_i_st
  
  # weighted average ensemble -----------------------------------------------
  # directory
  setwd(path); dir.create("05_ensembles"); setwd("05_ensembles")
  
  # ensemble
  for(c in ce){
    
    enm_i_st_ce <- enm_i_st %>%
      dplyr::filter(cenarios == c) %>% 
      dplyr::select(-1)
    
    ens <- enm_i_r[[1]]
    ens[] <- apply(enm_i_st_ce, 1, function(x){sum(x * tss_i) / sum(tss_i)})
    
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
  sui <- tibble::tibble(id = seq(ncell(enm_i_r)))
  
  for(i in enm_i_st$cenarios %>% unique){
    
    sui_t <- enm_i_st[enm_i_st$cenarios == i, -1]
    colnames(sui_t) <- paste0(colnames(sui_t), "_", i)
    sui <- cbind(sui, sui_t)
    
  }
  
  sui <- sui[, -1]
  
  s# factors
  scenario <- stringr::str_split(colnames(sui), "_", simplify = TRUE)[, 7]
  scenario
  
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
      
      lm_model <- lm(sui_p ~ method + aogcm + scenario +
                       method*aogcm + method*scenario + aogcm*scenario +
                       method*aogcm*scenario)
      anova_model <- anova(lm_model)
      sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
    }
    
  }
  
  # column names
  head(sui_ms)
  dim(sui_ms)
  colnames(sui_ms) <- 
    
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
  names(unc) <- c("method",  "aogcm", "scenario", "method_aogcm", 
                  "method_scenario", "aogcm_scenario", "residuals")
  plot(unc, col = viridis::viridis(10))
  
  # table
  table_resume <- sui_ms_prop %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na() %>%
    tidyr::pivot_longer(cols = c("method",  "aogcm", "scenario", "method_aogcm", 
                                 "method_scenario", "aogcm_scenario", "residuals"), 
                        names_to = "source", values_to = "values") %>% 
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
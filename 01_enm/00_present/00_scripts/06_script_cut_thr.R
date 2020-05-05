#' ---
#' title: binatization - cut threshold ensembles and uncertainties - area
#' authors: mauricio vancine
#' date: 2020-04-29
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)

# directory
path <- "/home/mude/data/github/00_github_organizar/r-sdm/00_pragmatico/00_present"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("02_occurrences/03_clean/00_occ_clean_taxa_date_bias_limit_spatial_2020-04-29.csv")
occ

# binatization and area ---------------------------------------------------
# directory
setwd(path); dir.create("06_ensembles_uncertainties_thrs")

# Binarizate and area
for(i in occ$species %>% unique){
  
  # ensemble
  # information
  print(paste0("Binarizate weighted average to ", i))
  
  # directory
  setwd(path); setwd("05_ensembles")
  
  # presence and pseudo-absence
  setwd(path); setwd(paste0("04_evaluation/", i))
  pa <- purrr::map_dfr(dir(pattern = "03_"), readr::read_csv) %>% 
    dplyr::mutate(species = i)
  
  # import ensembles
  setwd(path); setwd("05_ensembles")
  ens <- dir(pattern = paste0(i, ".tif$")) %>%
    grep("ensemble", ., value = TRUE) %>% 
    raster::raster()
  
  # import uncertainties
  unc <- dir(pattern = paste0(i, ".tif$")) %>% 
    grep("uncertainties", ., value = TRUE) %>% 
    raster::stack()
  
  # extract
  sui <- pa %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    raster::extract(ens, .)
  
  # combine
  pa_sui <- cbind(pa, sui = sui)
  
  # maximum tss and kappa
  max_tss <- ecospat::ecospat.max.tss(Pred = pa_sui$sui, Sp.occ = pa_sui$pa)
  max_kappa <- ecospat::ecospat.max.kappa(Pred = pa_sui$sui, Sp.occ = pa_sui$pa)
  
  # thrs
  thrs <- list(
    lpt = min(pa_sui[pa_sui$pa == 1, "sui"]),
    p10 = quantile(pa_sui[pa_sui$pa == 1, "sui"], .1) %>% as.numeric,
    p20 = quantile(pa_sui[pa_sui$pa == 1, "sui"], .2) %>% as.numeric,
    max_tss = max_tss$max.threshold,
    max_kappa = max_kappa$max.threshold)
  
  # directory
  setwd(path); setwd("06_ensembles_uncertainties_thrs")
  
  # area
  area <- NULL
  table_thr_area <- NULL
  
  # cuts
  for(j in thrs %>% length %>% seq){
    
    # information
    print(j)
    
    # cut
    ens_t <- ens >= thrs[[j]]
    
    # area
    area <- tapply(raster::area(ens_t), raster::values(ens_t), sum)
    area
    
    # table
    table_thr_area <- rbind(table_thr_area, 
                            tibble::tibble(species = i,
                                           threshold = names(thrs)[j],
                                           threshold_val = thrs[[j]] %>% round(3),
                                           area_total_km2 = sum(area) %>% round(3),
                                           presence_km2 = area[2] %>% round(3),
                                           presence_por = round(area[2]/sum(area)*100, 2),
                                           absence_km2 = area[1] %>% round(3),
                                           absence_por = round(area[1]/sum(area)*100, 2)))
    
    # ens
    raster::writeRaster(x = ens_t, 
                        filename = paste0("ensemble_", i, "_", names(thrs)[j]), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)
    
  }
  
  # export area
  readr::write_csv(table_thr_area, paste0("thresholds_areas_", i, ".csv"))
  
}

# end ---------------------------------------------------------------------
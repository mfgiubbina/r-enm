#' ---
#' title: threshold of ensembles
#' authors: mauricio vancine
#' date: 2020-05-09
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("02_occurrences/03_clean/occ_clean_taxa_date_bias_limit_spatial.csv")
occ

# binatization and area ---------------------------------------------------
# directory
setwd(path); dir.create("06_ensembles_thrs")

# binarizate and area
for(i in occ$species %>% unique){
  
  # ensemble
  # information
  print(paste0("Binarizate weighted average to ", i))
  
  # directory
  setwd(path); setwd("05_ensembles_uncertainties")
  
  # presence and pseudo-absence
  setwd(path); setwd(paste0("04_evaluation/", i))
  pa <- purrr::map_dfr(dir(pattern = "pa_|pr_"), readr::read_csv) %>% 
    dplyr::mutate(species = i)
  
  # import ensembles
  setwd(path); setwd(paste0("05_ensembles_uncertainties/", i))
  ens <- dir(pattern = paste0(i, ".tif$")) %>%
    grep("ensemble", ., value = TRUE) %>% 
    raster::raster()
  
  # extract
  sui <- pa %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    raster::extract(ens, .)
  
  # combine
  pa_sui <- cbind(pa, sui = sui)
  
  # maximum tss and kappa
  max_tss <- ecospat::ecospat.max.tss(Pred = pa_sui$sui, Sp.occ = pa_sui$pa)
  
  # thrs
  thrs <- list(
    lpt = round(min(pa_sui[pa_sui$pa == 1, "sui"]), 2),
    p10 = round(quantile(pa_sui[pa_sui$pa == 1, "sui"], .1), 2) %>% as.numeric,
    p20 = round(quantile(pa_sui[pa_sui$pa == 1, "sui"], .2), 2) %>% as.numeric,
    p30 = round(quantile(pa_sui[pa_sui$pa == 1, "sui"], .3), 2) %>% as.numeric,
    max_tss = max_tss$max.threshold)
  
  # tss
  tss <- list(
    lpt = max_tss$table[max_tss$table$threshold == thrs$lpt, 2],
    p10 = max_tss$table[max_tss$table$threshold == thrs$p10, 2],
    p20 = max_tss$table[max_tss$table$threshold == thrs$p20, 2],
    p30 = max_tss$table[max_tss$table$threshold == thrs$p30, 2],
    max_tss = max_tss$max.TSS)
  
  # directory
  setwd(path); setwd("06_ensembles_thrs"); dir.create(i); setwd(i)
  
  # area table
  table_thr_area <- NULL
  
  # cuts
  for(j in thrs %>% length %>% seq){
    
    # information
    print(names(thrs[j]))
    
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
                                           tss = tss[[j]]%>% round(3),
                                           area_total_km2 = sum(area) %>% round(3),
                                           presence_km2 = area[2] %>% round(3),
                                           presence_por = round(area[2]/sum(area)*100, 2),
                                           absence_km2 = area[1] %>% round(3),
                                           absence_por = round(area[1]/sum(area)*100, 2)))
    
    # ens
    raster::writeRaster(x = ens_t, 
                        filename = paste0(names(ens), "_thr_", names(thrs)[j]), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)
    
  }
  
  # export area
  readr::write_csv(table_thr_area, paste0("00_thresholds_areas_", i, ".csv"))
  
}

# end ---------------------------------------------------------------------
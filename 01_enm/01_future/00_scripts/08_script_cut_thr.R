# -------------------------------------------------------------------------
# binatization - cut threshold
# mauricio vancine - mauricio.vancine@gmail.com
# 17-07-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)

# directory
path <- "/home/mude/data/gitlab/course-sdm"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
setwd("02_occ")
occ <- readr::read_csv("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv")
occ

# binatization - cut threshold --------------------------------------------
# directory
setwd(path); dir.create("08_consenso_thr"); setwd("08_consenso_thr")

for(i in occ$species %>% unique){
  
  # ensemble - weighted average
  # information
  print(paste0("Binarizate weighted average to ", i))
  
  # directory
  setwd(path); setwd("06_consenso_media_ponderada")
  
  # import
  ens_w <- dir(pattern = ".tif$") %>% 
    raster::stack()
  
  # extract
  thrs <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    raster::extract(ens_w, .)
  
  # thrs
  li_thrs <- list(
    lpt = min(thrs[thrs > 0]),
    p10 = quantile(thrs[thrs > 0], .1),
    p20 = quantile(thrs[thrs > 0], .2)
  )
  
  # directory
  setwd(path); setwd("08_consenso_thr")
  
  for(j in li_thrs %>% length %>% seq){
    
    # export
    raster::writeRaster(x = ens_w >= li_thrs[[j]], 
                        filename = paste0("consenso_media_ponderada_thr_", names(li_thrs)[j], "_", i), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)
    
  }

}


# end ---------------------------------------------------------------------
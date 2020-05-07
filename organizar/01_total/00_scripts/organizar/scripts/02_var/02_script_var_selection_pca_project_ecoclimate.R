# script description #
# script:  variables - pca projection
# authors: mauricio vancine
# date:    06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(factoextra)
library(FactoMineR)
library(raster)
library(RStoolbox)
library(tidyverse)
library(viridis)

# source
source("/media/mude/data/gitlab/r-enm/scripts/02_var/02_script_function_var_selection_pca_projection.R")

# directory
path <- "/media/mude/data/spatial_data/raster/ecoclimate/02_modern"
setwd(path)
dir()

# import ------------------------------------------------------------------
# list variables
tif <- dir(pattern = ".tif$")
tif

# tif presente
tif_pre <- tif %>% 
  stringr::str_subset("modern")
tif_pre

# tif future
tif_fut <- tif %>% 
  stringr::str_subset("rcp")
tif_fut

# import rasters present
var_pre <- raster::stack(tif_pre)
var_pre

# import rasters future
var_fut <- raster::stack(tif_fut)
var_fut

# aogcms list
aogcm <- stringr::str_split(tif_pre, "_", simplify = TRUE)[, 1] %>% unique
aogcm <- aogcm[-c(3, 7)]
aogcm

# pca ---------------------------------------------------------------------
# directory
dir.create("pca")
setwd("pca")
dir.create("pres")
dir.create("proj")

# for
for(i in aogcm){
  
  # information
  print(i)
  
  # aogcm raster present
  var_pre_aogcm <- var_pre[[grep(pattern = i, names(var_pre), value = TRUE)]]
  
  # aogcm raster future
  var_fut_aogcm <- var_fut[[grep(pattern = i, names(var_fut), value = TRUE)]]
  
  for(j in c("rcp45", "rcp85")){
    
    # aogcm raster future
    var_fut_aogcm_rcp <- var_fut_aogcm[[grep(pattern = j, names(var_fut_aogcm), value = TRUE)]]
    
    # function
    var_pca_proj(raster_pres = var_pre_aogcm,
                 raster_proj = var_fut_aogcm_rcp,
                 cum_sum = .95,
                 graphics = TRUE,
                 graphic_names = i,
                 graphic_var_pres_names = c(paste0("bio0", 1:9), paste0("bio", 10:19)),
                 prefix_pca_raster_pres = names(var_pre_aogcm[[1]]) %>% sub("_bio01", "", .), 
                 prefix_pca_raster_proj = names(var_fut_aogcm_rcp[[1]]) %>% sub("_bio01", "", .), 
                 path_output_pca_pres = "/media/mude/data/spatial_data/raster/ecoclimate/02_modern/pca/pres", 
                 path_output_pca_proj = "/media/mude/data/spatial_data/raster/ecoclimate/02_modern/pca/proj")
    
  }
  
}

# end ---------------------------------------------------------------------
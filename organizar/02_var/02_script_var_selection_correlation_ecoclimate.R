# script description
# objective:  variables selection - correlation
# variables:  ecoclimate
# author:     mauricio vancine
# date:       28-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(caret)
library(corrr)
library(GGally)
library(raster)
library(rgdal)
library(tidyverse)

# source
source("/media/mude/data/gitlab/r-enm/scripts/02_var/02_script_function_var_correlation.R")

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var/ecoclimate")
dir()

# variables ---------------------------------------------------------------
# list files
tif_pres <- dir(patt = "modern", recursive = TRUE) %>% 
  grep(pattern = "brazil", value = TRUE)
tif_pres

tif_fut <- dir(patt = "rcp", recursive = TRUE) %>% 
  grep(pattern = "brazil", value = TRUE) %>% 
  grep(pattern = "mri_rcp85", value = TRUE)
tif_fut

var_fut <- raster::stack(tif_fut)
raster_proj=var_fut

# import rasters
var_pres <- raster::stack(tif_pres)
var_pres

# plot
landscapetools::show_landscape(var_pres[[1]])

# correlation -------------------------------------------------------------
# aogcms
aogcm <- stringr::str_split(names(var_pres), pattern = "_", simplify = TRUE)[, 1] %>% 
  unique
aogcm

# correlation
for(i in aogcm){
  
  # information
  print(i)
  
  # selection
  var_pres_i <- var_pres[[grep(i, names(var_pres), value = TRUE)]]
  
  var_fur_i <- var_fut[[grep(i, names(var_fut), value = TRUE)]]
  
  # function
  var_correlation(raster = var_pres_i,
                  cutoff = .7, 
                  method = "spearman", 
                  names = c(paste0("bio0", 1:9), paste0("bio", 10:19)),
                  path = "/media/mude/data/gitlab/r-enm/data/02_var/ecoclimate",
                  folder = paste0("correlation_", i))
}
  
# end ---------------------------------------------------------------------
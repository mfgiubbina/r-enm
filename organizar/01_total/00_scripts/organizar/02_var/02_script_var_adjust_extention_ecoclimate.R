# script description #
# script: variables - adjust extention ecoclimate
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# source
source("/media/mude/data/gitlab/r-enm/scripts/02_var/02_script_function_var_adjust_extention.R")
var_adjust_extention

# directory
path <- "/media/mude/data/spatial_data/raster/ecoclimate/02_modern"
setwd(path)
dir()

# limit -------------------------------------------------------------------
# import
br <- sf::read_sf("/media/mude/data/gitlab/r-enm/data/02_var/limits/naturalearth_small_brazill.shp")
br

# plot
ggplot() +
  geom_sf(data = br, fill = "gray") +
  theme_bw()

# variables presente ------------------------------------------------------
# list files
tif <- dir(patt = ".tif$", recursive = TRUE)
tif

# import rasters
var <- raster::stack(tif_pres)
var

# plot
plot(var$ccsm_modern_1950_1999_bio01)

# adust extention ---------------------------------------------------------
# function
var_adjust_extention(raster = var, 
                     vector = br, 
                     names = "ne_brazil", 
                     path = path)

# end ---------------------------------------------------------------------
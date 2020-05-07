# script description #
# script: variables - adjust extention
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# raster options
raster::rasterOptions(maxmemory = 1e+10)
raster::rasterOptions(chunksize = 1e+10)

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var")
dir()

# limit -------------------------------------------------------------------
# directory
setwd("limits")

# import
br <- sf::st_read("naturalearth_small_brazill.shp")
br

# plot
ggplot() +
  geom_sf(data = br, fill = "gray") +
  theme_bw()

# climate variables -------------------------------------------------------
# directory
setwd("..")
setwd("bioclim_v20")
dir()

# list files
tif <- dir(pattern = "tif$")
tif

# import
var <- raster::stack(tif)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
plot(var$bio01)

# adust extention ---------------------------------------------------------
# crop = adjust extention
var_crop_br <- raster::crop(x = var, y = br)
var_crop_br

# plot
plot(var_crop_br$bio01)

# adjust to limite --------------------------------------------------------
# mask = adjust to mask
var_mask_br <- raster::mask(x = var_crop_br, mask = br)
var_mask_br

# plot
plot(var_mask_br$bio01)

# export ------------------------------------------------------------------
raster::writeRaster(x = var_mask_br, 
                    filename = paste0("wc20_masknebrazil_res10m_", names(var)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# exclude -----------------------------------------------------------------
# list
li <- dir(pattern = "wc2.0")
li

# exclude
unlink(li)

# end ---------------------------------------------------------------------
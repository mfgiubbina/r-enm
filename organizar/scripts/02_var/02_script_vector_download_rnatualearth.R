# script description #
# script: vector - download natural earth
# package: rnaturalearth
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(rnaturalearth)
library(sf)
library(tidyverse)

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var")
dir()

# limit -------------------------------------------------------------------
# directory
dir.create("limits")
setwd("limits")

# download
br <- rnaturalearth::ne_countries(scale = "small", country = "Brazil", returnclass = "sf")
br

# plot
ggplot() +
  geom_sf(data = br, fill = "gray") +
  theme_bw()

# export limit
sf::st_write(br, "naturalearth_small_brazill.shp")

# naturalearth ------------------------------------------------------------
# https://www.naturalearthdata.com/

#' Natural Earth is a public domain map dataset available at 1:10m, 1:50m, 
#' and 1:110 million scales. Featuring tightly integrated vector and raster 
#' data, with Natural Earth you can make a variety of visually pleasing, 
#' well-crafted maps with cartography or GIS software

# end ---------------------------------------------------------------------
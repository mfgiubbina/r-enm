### script enm - dismo ###

## multiple algorithms ##

# mauricio vancine
# 21-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(SSDM)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyverse)
library(viridis)

# directory
path <- "/home/mude/data/gitlab/r-enm/data"
setwd(path)
dir()

# occurrences -------------------------------------------------------------
occ <- readr::read_csv("/home/mude/data/gitlab/r-enm/data/01_occ/terretrial_animal_occ_spocc_raw_2019-04-23_filter_gnr_spatial_date.csv")
occ

# map
br <- rnaturalearth::countries110 %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(name == "Brazil")
br

# plot
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ, aes(x = longitude, y = latitude), size = 2.5, alpha = .2) +
  coord_sf() +
  theme_bw()

# variables ---------------------------------------------------------------
# list files
tif <- dir(pattern = "tif$", recursive = TRUE) %>% 
  stringr::str_subset(pattern = "pca")
tif

# import rasters
var <- raster::stack(tif) %>% raster::brick()
var

# names
names(var)
names(var) <- stringr::str_replace(names(var), "wc20_ne_brazil_res05_", "")
names(var)
crs(var) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# plot
landscapetools::show_landscape(var$pc01) + 
  geom_point(data = occ, aes(x = longitude, y = latitude, color = species), 
             size = 2.5, alpha = .7)

# enm adjust --------------------------------------------------------------
# ensemble SDM building
ESDM <- SSDM::ensemble_modelling(algorithms = c("GLM", "GAM", "MARS", "GBM", "CTA", "RF", "MAXENT", "ANN", "SVM"), 
                                 Occurrences = occ %>% dplyr::select(species, longitude, latitude), 
                                 Env = var %>% raster::stack(), 
                                 rep = 1,
                                 Xcol = "longitude", 
                                 Ycol = "latitude")


# Results plotting
plot(ESDM)

plot(ESDM@projection)
ESDM@algorithm.evaluation
ESDM@evaluation
ESDM@parameters


# end ---------------------------------------------------------------------

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
path <- "/media/mude/data/gitlab/r-enm/data"

# occurrences -------------------------------------------------------------
# directory
setwd("/media/mude/data/gitlab/r-enm/data/01_occ")
dir()

# occurrences
occ <- readr::read_csv("occ_spocc_filter_taxonomic_terretrial_animal_2019-04-18.csv")
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
# directory
setwd("/media/mude/data/00_trabalho/00_empresas/aquaflora/05_distribuicao_especies/00_treinamento/02_data/01_var/02_selection/04_pca/")
dir()

# list files
ti <- dir(patt = ".tif$")
ti

# import rasters
var <- raster::stack(ti) %>% 
  raster::brick()
var

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_sf(data = br, fill = NA) +
  geom_point(data = occ, aes(x = longitude, y = latitude), color = "red", size = 2.5, alpha = .4) +
  coord_sf() +
  theme_bw()

# enm adjust --------------------------------------------------------------
# ensemble SDM building
ESDM <- SSDM::ensemble_modelling(algorithms = c("GLM", "GAM"), 
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

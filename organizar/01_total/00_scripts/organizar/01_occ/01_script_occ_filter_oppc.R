# script description #
# script: occurrences - oppc
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)

# raster options
raster::rasterOptions()
raster::rasterOptions(maxmemory = 1e+50)
raster::rasterOptions(chunksize = 1e+50)
raster::rasterOptions()

# directory
path <- "/media/mude/data/gitlab/r-enm/data"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ_data <- readr::read_csv("01_occ/filtered/occ_terretrial_animal_integrated_filter_gnr_dist_spatial_date.csv")
occ_data

# var
raster_id <- raster::raster("02_var/bioclim_v20/pca/wc20_masknebrazil_res05g_pc01.tif")
raster_id[!is.na(raster_id)] <- raster::cellFromXY(raster_id, raster::rasterToPoints(raster_id)[, 1:2])
names(raster_id) <- "id"
raster_id

# map
landscapetools::show_landscape(raster_id)

# data filter -------------------------------------------------------------
# oppc
occ_data_oppc <- occ_data %>% 
  dplyr::mutate(oppc = raster::extract(raster_id, dplyr::select(., longitude, latitude))) %>% 
  dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
  dplyr::filter(!is.na(oppc)) %>% 
  dplyr::add_count(species) %>% 
  dplyr::arrange(species)
occ_data_oppc

# verify
table(occ_data$species)
table(occ_data_oppc$species)

# export ------------------------------------------------------------------
readr::write_csv(occ_data_oppc, "01_occ/filtered/occ_terretrial_animal_integrated_filter_gnr_dist_spatial_date_oppc.csv")

# end ---------------------------------------------------------------------
### function download bio-oracle ###

# mauricio vancine
# 14-04-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)
library(sdmpredictors)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data"
setwd(path)
dir.create("bio_oracle")
setwd("bio_oracle")
dir.create("present")
dir.create("future")
dir()

# layer list --------------------------------------------------------------
# present
la_pre <- sdmpredictors::list_layers(version = 2) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(dataset_code, layer_code, name, description, units, cellsize_lonlat, is_surface)
la_pre

readr::write_csv(la_pre, paste0(path, "/bio_oracle/present/bio_oracle_present.csv"))

# future
la_fut <- sdmpredictors::list_layers_future(version = 2) %>% 
  tibble::as_tibble()
la_fut

readr::write_csv(la_fut, paste0(path, "/bio_oracle/present/bio_oracle_future.csv"))

# layers load -------------------------------------------------------------
# load-download present
sdmpredictors::load_layers(layercodes = la_pre$layer_code[1],
                           datadir = paste0(path, "/bio_oracle/present"))

# load-download future
sdmpredictors::load_layers(layercodes = la_fut$layer_code[1],
                           datadir = paste0(path, "/bio_oracle/future"))

# layer statistics --------------------------------------------------------
# descritive
layer_stats(li_pre$layer_code[1:2]) 

# correlation
layers_correlation(li_pre$layer_code[1:2])

# end ---------------------------------------------------------------------
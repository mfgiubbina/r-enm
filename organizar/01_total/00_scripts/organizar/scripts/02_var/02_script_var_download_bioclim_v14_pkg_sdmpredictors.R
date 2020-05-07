### script enm - download bio-oracle v1 ###

# mauricio vancine
# 15-04-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(sdmpredictors)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data"
setwd(path)
dir.create("bioclim_v14_sdmpredictors")
setwd("bioclim_v14_sdmpredictors")
dir.create("present")
dir.create("future")
dir()

# layer list --------------------------------------------------------------
# present
la_pre <- sdmpredictors::list_layers() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(dataset_code == "WorldClim")
la_pre

readr::write_csv(la_pre, paste0(path, "/bioclim_v14_sdmpredictors/present/bioclim_v14_present.csv"))

# future
la_fut <- sdmpredictors::list_layers_future() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(dataset_code == "WorldClim")
la_fut

readr::write_csv(la_fut, paste0(path, "/bioclim_v14_sdmpredictors/future/bioclim_v14_future.csv"))

# layers load -------------------------------------------------------------
# load-download present
sdmpredictors::load_layers(layercodes = la_pre$layer_code,
                           datadir = paste0(path, "/bioclim_v14_sdmpredictors/present"))

# load-download future
sdmpredictors::load_layers(layercodes = la_fut$layer_code,
                           datadir = paste0(path, "/bioclim_v14_sdmpredictors/future"))

# layer statistics --------------------------------------------------------
# descritive
layer_stats(la_pre$layer_code[1:2]) 

# correlation
layers_correlation(la_pre$layer_code[1:2])

# end ---------------------------------------------------------------------
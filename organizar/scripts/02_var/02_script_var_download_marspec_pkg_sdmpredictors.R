### script enm - download marspec ###

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
dir.create("marspec")
setwd("marspec")
dir()

# layer list --------------------------------------------------------------
# present
la <- sdmpredictors::list_layers() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(dataset_code == "MARSPEC")
la

readr::write_csv(la, paste0(path, "/marspec/marspec.csv"))

# layers load -------------------------------------------------------------
# load-download
sdmpredictors::load_layers(layercodes = la$layer_code,
                           datadir = paste0(path, "/marspec"))

# layer statistics --------------------------------------------------------
# descritive
layer_stats(la$layer_code[1:2]) 

# correlation
layers_correlation(la$layer_code[1:2])

# end ---------------------------------------------------------------------
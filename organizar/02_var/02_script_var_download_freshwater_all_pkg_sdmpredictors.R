### script enm - download freshwater ###

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
dir.create("freshwater")
setwd("freshwater")
dir()

# layer list --------------------------------------------------------------
# layers
la <- sdmpredictors::list_layers() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(dataset_code == "Freshwater")
la

# information
readr::write_csv(la, paste0(path, "/freshwater/freshwater.csv"))

# layers load -------------------------------------------------------------
# load-download
sdmpredictors::load_layers(layercodes = la$layer_code,
                           datadir = paste0(path, "/freshwater"))

# end ---------------------------------------------------------------------
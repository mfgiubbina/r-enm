### function download bio-oracle ###

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
dir.create("bio_oracle_v2")
setwd("bio_oracle_v2")
dir.create("present")
dir.create("future")
dir()

# layer list --------------------------------------------------------------
# present
la_pre <- sdmpredictors::list_layers(version = 2) %>% 
  tibble::as_tibble()
la_pre

readr::write_csv(la_pre, paste0(path, "/bio_oracle_v2/present/bio_oracle_present.csv"))

# future
la_fut <- sdmpredictors::list_layers_future(version = 2) %>% 
  tibble::as_tibble()
la_fut$dataset_code %>% table

readr::write_csv(la_fut, paste0(path, "/bio_oracle_v2/present/bio_oracle_future.csv"))

# layers load -------------------------------------------------------------
# load-download present
sdmpredictors::load_layers(layercodes = la_pre$layer_code,
                           datadir = paste0(path, "/bio_oracle_v2/present"))

# load-download future
sdmpredictors::load_layers(layercodes = la_fut$layer_code,
                           datadir = paste0(path, "/bio_oracle_v2/future"))

# layer statistics --------------------------------------------------------
# descritive
layer_stats(la_pre$layer_code[1:2]) 

# correlation
layers_correlation(la_pre$layer_code[1:2])

# end ---------------------------------------------------------------------
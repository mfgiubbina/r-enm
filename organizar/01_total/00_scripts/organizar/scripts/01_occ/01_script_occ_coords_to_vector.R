# script description #
# script: occurences - coordinates to vector
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(sf)
library(readxl)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/spocc"
setwd(path)
dir()

# import ------------------------------------------------------------------
occ <- purrr::map_dfr(dir(pattern = ".csv"), readr::read_csv)
occ
  
# convert to vector -------------------------------------------------------
occ_ve <- sf::st_as_sf(occ, coords = c("longitude", "latitude"), crs = 4326)
occ_ve

plot(occ_ve[2], pch = 20)

# export ------------------------------------------------------------------


# end ---------------------------------------------------------------------
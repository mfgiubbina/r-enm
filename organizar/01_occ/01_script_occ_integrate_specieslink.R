# script description #
# script: occurences - integrate specieslink
# package: spocc
# author:   mauricio vancine
# date:     07-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(readxl)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# specieslink -------------------------------------------------------------
# directory
setwd("specieslink")
dir()

occ_specieslink <- NULL

for(i in dir(patt = ".xlsx")){
  
  # information
  print(i)
  
  # import data
  occ <- readxl::read_xlsx(i)
  
  # adjust data
  occ_data <- occ %>%
    dplyr::mutate(name = scientificname,
                  species = stringr::str_split(i, pattern = "_", simplify = TRUE)[, 3:4] %>% paste0(., collapse = "_"),
                  longitude = longitude %>% as.numeric, 
                  latitude = latitude %>% as.numeric, 
                  year = yearcollected %>% as.numeric, 
                  base = "specieslink") %>%
    dplyr::select(name, species, longitude, latitude, year, base)
  
  # combine data
  occ_specieslink <- dplyr::bind_rows(occ_specieslink, occ_data)
  
}

# view
occ_specieslink

# end ---------------------------------------------------------------------
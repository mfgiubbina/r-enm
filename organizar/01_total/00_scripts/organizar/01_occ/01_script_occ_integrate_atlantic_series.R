# script description #
# script: occurences - integrate atlantics
# package: spocc
# author:   mauricio vancine
# date:     06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# atlantics ---------------------------------------------------------------
# directory
setwd("atlantic_series")
dir()

occ_atlantics <- NULL

for(i in dir(patt = ".csv")){
  
  # information
  print(i)
  
  # import data
  occ <- readr::read_csv(i)
  
  # adjust data
  occ_data <- occ %>%
    dplyr::mutate(name = as.character(name),
                  species = as.character(species),
                  longitude = longitude %>% as.character %>% as.numeric,
                  latitude = latitude %>% as.character %>% as.numeric,
                  year = year %>% as.numeric,
                  base = as.character(base))
  
  # combine data
  occ_atlantics <- dplyr::bind_rows(occ_atlantics, occ_data)
  
}

# view
occ_atlantics

# export
readr::write_csv(occ_atlantics, "00_atlantic_series_integrated.csv")

# end ---------------------------------------------------------------------
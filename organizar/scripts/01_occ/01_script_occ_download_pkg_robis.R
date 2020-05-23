# script description #
# function: occurences - download
# package:  robis
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(robis)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/lista_especies"
setwd(path)
dir()

# species list ------------------------------------------------------------
sp_list <- readr::read_csv("marine_animal_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::pull()
sp_list

# occ download ------------------------------------------------------------
occ <- robis::occurrence(sp_list) %>% 
  tibble::as_tibble()
occ

dplyr::glimpse(occ_data)

# get data ----------------------------------------------------------------
occ_data <- occ %>% 
  dplyr::mutate(name = scientificName,
                species = scientificName %>% stringr::str_replace(" ", "_") %>% stringr::str_to_lower(),
                longitude = as.numeric(decimalLongitude), 
                latitude = as.numeric(decimalLatitude),
                year = as.numeric(date_year),
                base = paste0("obis_", as.character(basisOfRecord))) %>% 
  dplyr::arrange(species) %>% 
  dplyr::select(name, species, longitude, latitude, year, base)
occ_data

# export ------------------------------------------------------------------
readr::write_csv(occ_data, paste0("marine_animal_occ_raw_", lubridate::today(), ".csv"))

# end ---------------------------------------------------------------------
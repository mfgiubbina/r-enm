### script enm - download occurrences - robis ##

# mauricio vancine
# 03-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(robis)
library(tidyverse)

# directory
# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/lista_especies"
setwd(path)
dir()

# species list ------------------------------------------------------------
sp_list <- readr::read_csv("marine_animal_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::pull()
sp_list

# download ----------------------------------------------------------------
# directory
setwd("..")
dir.create("spocc")
setwd("spocc")

# occ
for(i in sp_list){
  
  # information
  print(i)
  
  # download
  occ <- robis::occurrence(scientificname = i) %>% 
    tibble::as_tibble()

  # selection
  occ_data <- occ %>% 
    dplyr::mutate(name = scientificName,
                  species = scientificName %>% stringr::str_replace(" ", "_") %>% stringr::str_to_lower(),
                  longitude = as.numeric(decimalLongitude), 
                  latitude = as.numeric(decimalLatitude),
                  year = as.numeric(date_year),
                  base = paste0("obis_", as.character(basisOfRecord))) %>% 
    dplyr::select(name, species, longitude, latitude, year, base) %>% 
    dplyr::arrange(species)
  
  # export
  readr::write_csv(occ_data, 
                   paste0("occ_robis_", i %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                          "_", lubridate::today(), ".csv"))
  
}

# end ---------------------------------------------------------------------
# script description #
# function: occurences - download
# package:  bien
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(BIEN)
library(lubridate)
library(tidyverse)

# directory
setwd("/media/mude/data/gitlab/r-enm/data/01_occ/lista_especies")
dir()

# import ------------------------------------------------------------------
# species list
sp_list <- readr::read_csv("terretrial_plants_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::pull()
sp_list

# download ----------------------------------------------------------------
# directory
setwd("..")
dir.create("bien")
setwd("bien")

# occ
for(i in sp_list){
  
  # information
  print(i)
  
  # download
  occ <- BIEN::BIEN_occurrence_species(species = i) %>% 
    tibble::as_tibble()
  
  # conditional without data
  if(nrow(occ) == 0){
    
    # get data
    occ_data <- tibble::tibble(name = i,
                    species = i %>% stringr::str_replace(" ", "_") %>% stringr::str_to_lower(),
                    longitude = NA,
                    latitude = NA,
                    year = NA,
                    base = NA)
    
  } else{
    
    # get data
    occ_data <- occ %>% 
      dplyr::mutate(name = scrubbed_species_binomial,
                    species = scrubbed_species_binomial %>% stringr::str_replace(" ", "_") %>% stringr::str_to_lower(),
                    year = lubridate::year(occ$date_collected),
                    base = datasource) %>% 
      dplyr::select(name, species, longitude, latitude, year, base)
  }
  
  # export
  readr::write_csv(occ_data, 
                   paste0("occ_bien_", i %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                          "_", lubridate::today(), ".csv"))
  
}

# end ---------------------------------------------------------------------
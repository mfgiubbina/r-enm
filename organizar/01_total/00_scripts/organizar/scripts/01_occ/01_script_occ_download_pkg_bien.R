# script description #
# function: occurences - download
# package: bien
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

# bien --------------------------------------------------------------------
# download occ
occ_bien <- BIEN::BIEN_occurrence_species(species = sp_list) %>% 
    tibble::as_tibble()
occ_bien

# adjust data
da.pl.bien <- occ.pl.bien %>% 
  dplyr::mutate(species = scrubbed_species_binomial %>% stringr::str_replace(" ", "_") %>% stringr::str_to_lower(),
                year = lubridate::year(occ.pl.bien$date_collected),
                key = as.character(datasource_id)) %>% 
  dplyr::rename(name = scrubbed_species_binomial,  base = datasource) %>% 
  dplyr::select(species, name, longitude, latitude, base, year, key)
da.pl.bien  



# export
readr::write_csv(da.pl.se, "occ_pl.csv")

# end ---------------------------------------------------------------------
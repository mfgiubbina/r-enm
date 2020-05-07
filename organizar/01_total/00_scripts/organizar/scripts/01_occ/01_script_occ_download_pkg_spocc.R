### script occ - pkg spocc - download ###

# mauricio vancine
# 03-05-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(lubridate)
library(mapr)
library(spocc)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# import data -------------------------------------------------------------
# species list
sp_list <- readr::read_csv("terretrial_animal_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::arrange(species) %>% 
  dplyr::pull()
sp_list

# bases for download
db <- c("gbif", "bison", "inat", "ebird", "ecoengine", "vertnet", "idigbio", "obis", "ala")
db

# download ----------------------------------------------------------------
# occ data
occ <- spocc::occ(query = sp_list, 
                     from = bases, 
                     ebirdopts = list(key = "t3bbcnthdbjs"), # make key in https://ebird.org/api/keygen
                     has_coords = TRUE, 
                     limit = 1e6)
occ

# map
mapr::map_leaflet(occ$gbif$data$Boana_faber) %>% 
  mapr::hull()

# get data ----------------------------------------------------------------
# data
occ_data <- spocc::occ2df(occ) %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = lubridate::year(date)) %>% 
  dplyr::select(species, name, longitude, latitude, year, base)
occ_data

# export ------------------------------------------------------------------
readr::write_csv(occ_data, paste0("occ_spocc_raw_terretrial_animal_", lubridate::today(), ".csv"))

# end ---------------------------------------------------------------------
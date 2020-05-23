### script enm - download occurrences - spocc ##

# mauricio vancine
# 18-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(lubridate)
library(marinespeed)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# download ----------------------------------------------------------------
# species list
sp_list <- readr::read_csv("00_species_list/00_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::pull()
sp_list

# bases for download
bases <- c("gbif", 
           "bison", 
           "inat", 
           "ebird", 
           "ecoengine", 
           "vertnet", 
           "idigbio", 
           "obis", 
           "ala")
bases

# download occ
occ <- spocc::occ(query = sp_list, 
                     from = bases, 
                     ebirdopts = list(key = "t3bbcnthdbjs"), # make key in https://ebird.org/api/keygen
                     has_coords = TRUE, 
                     limit = 1e2)
occ

# verify warnings
warnings()

# map
mapr::map_leaflet(occ$gbif$data$Haddadus_binotatus) %>% 
  hull()


# get data ----------------------------------------------------------------
# data
occ_data <- spocc::occ2df(occ) %>% 
  dplyr::mutate(longitude = longitude %>% as.numeric(), 
                latitude = latitude %>% as.numeric(),
                key = as.character(key),
                year = lubridate::year(date))
occ_data

# export
dir.create("01_occ")
readr::write_csv(occ_data, paste0("01_occ/00_occ_raw_", lubridate::today(), ".csv"))

# end ---------------------------------------------------------------------
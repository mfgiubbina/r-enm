# script description #
# function: occurences - download
# package: spocc
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(lubridate)
library(spocc)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/lista_especies"
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
# directory
setwd("..")
dir.create("spocc")
setwd("spocc")

# occ
for(i in sp_list){
  
  # information
  print(i)
  
  # download
  occ <- spocc::occ(query = i, 
                    from = db,
                    has_coords = TRUE, 
                    limit = 1e6)
  
  # conditional without data
  if(occ %>% spocc::occ2df() %>% nrow == 0){
    
    # conditional without year  
  } else if(!"date" %in% c(occ %>% spocc::occ2df() %>% colnames)){
    
    # get data
    occ_data <- spocc::occ2df(occ) %>% 
      dplyr::mutate(longitude = as.numeric(longitude),
                    latitude = as.numeric(latitude),
                    year = NA,
                    base = prov) %>% 
      dplyr::select(name, longitude, latitude, year, base)
    
    # conditional with data and year
  } else{
    
    # get data
    occ_data <- spocc::occ2df(occ) %>% 
      dplyr::mutate(longitude = as.numeric(longitude),
                    latitude = as.numeric(latitude),
                    year = lubridate::year(date),
                    base = prov) %>% 
      dplyr::select(name, longitude, latitude, year, base)
    
  }
  
  # export
  readr::write_csv(occ_data, 
                   paste0("occ_spocc_", i %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                          "_", lubridate::today(), ".csv"))
  
}

# end ---------------------------------------------------------------------
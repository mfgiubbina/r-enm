# script description #
# script: occurences - integrate
# package: spocc
# author:   mauricio vancine
# date:     06-05-2018

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

# specieslink -------------------------------------------------------------
# directory
setwd("..")
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

# spocc -------------------------------------------------------------------
# directory
setwd("..")
setwd("spocc")
dir()

occ_spocc <- purrr::map_dfr(dir(pattern = ".csv"), readr::read_csv)
occ_spocc

# integrate ---------------------------------------------------------------
occ <- dplyr::bind_rows(occ_atlantics, occ_specieslink, occ_spocc) %>% 
  dplyr::arrange(species)
occ

# export ------------------------------------------------------------------
# directory
setwd("..")
dir.create("integrated")
setwd("integrated")

readr::write_csv(occ, "occ_terretrial_animal_integrated.csv")

# end ---------------------------------------------------------------------
### script occ - pkg taxize - global names resolver ###

# mauricio vancine
# 05-05-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(taxize)
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

# global names resolver ---------------------------------------------------
# gnr names
gnr <- taxize::gnr_resolve(sp_list)
gnr

# others names
gnr_tax <- gnr %>% 
  dplyr::mutate(species = submitted_name %>% 
                  stringr::str_to_lower() %>% 
                  stringr::str_replace(" ", "_")) %>% 
  dplyr::select(species, matched_name) %>% 
  dplyr::bind_rows(tibble::tibble(species = sp_list %>% 
                                    stringr::str_to_lower() %>% 
                                    stringr::str_replace(" ", "_") %>% 
                                    rep(times = 4),
                                  matched_name = c(sp_list,
                                                   sp_list %>% stringr::str_to_lower(),
                                                   sp_list %>% stringr::str_to_upper(),
                                                   sp_list %>% stringr::str_to_title()))) %>% 
  dplyr::distinct()

gnr_tax

# export ------------------------------------------------------------------
# gnr names
readr::write_csv(gnr, "terretrial_animal_gnr.csv")

# gnr total names
readr::write_csv(gnr_tax, "terretrial_animal_gnr_tax.csv")

# end ---------------------------------------------------------------------
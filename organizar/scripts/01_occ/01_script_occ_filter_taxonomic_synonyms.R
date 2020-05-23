### script enm - taxonomic filter occurrences ###

# mauricio vancine
# 23-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(lubridate)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ_data <- readr::read_csv("terretrial_animal_occ_spocc_raw_2019-04-23.csv")
occ_data

# confer
occ_data$name %>% table %>% tibble::as_tibble() %>% select(1) %>% pull

# species list
sp_list <- readr::read_csv("terretrial_animal_species_list.csv") %>% 
  dplyr::pull()
sp_list

# preparate data ----------------------------------------------------------
# import taxize data
tax_data <- readr::read_csv("terretrial_animal_gnr.csv") %>% 
  dplyr::select(user_supplied_name, matched_name) %>% 
  dplyr::mutate(species = user_supplied_name %>% tolower %>% sub(" ", "_", .)) %>% 
  dplyr::bind_rows(tibble::tibble(species = sp_list %>% tolower %>% sub(" ", "_", .),
                                  gnr_name_sp = sp_list %>% tolower,
                                  gnr_name_syn = NA))
tax_data

# species
tax_data_filter <- tax_data %>% 
  dplyr::select(species, gnr_name_sp) %>% 
  dplyr::rename(name = gnr_name_sp) %>% 
  dplyr::distinct()
tax_data_filter

# syn
tax_data_filter_syn <- tax_data %>% 
  dplyr::select(species, gnr_name_syn) %>%
  dplyr::rename(name = gnr_name_syn) %>% 
  dplyr::bind_rows(tax_data_filter, .) %>% 
  dplyr::distinct()
tax_data_filter_syn

# taxonomic filter --------------------------------------------------------
# filter by gnr name
occ_data_filter_gnr <- dplyr::inner_join(tax_data_filter, occ_data, by = "name")
occ_data_filter_gnr

# confer
occ_data_filter_gnr$name %>% table

# filter by gnr syn
occ_data_filter_gnr_syn <- dplyr::inner_join(tax_data_filter_syn, occ_data, by = "name")
occ_data_filter_gnr_syn

# confer
occ_data_filter_gnr_syn$name %>% table

# export data -------------------------------------------------------------
# species
readr::write_csv(occ_data_filter_gnr,
                 paste0("01_occ_filter_taxonomic_", lubridate::today(), ".csv"))

# syn
readr::write_csv(occ_data_filter_gnr_syn,
                 paste0("01_occ_filter_taxonomic_syn_", lubridate::today(), ".csv"))

# end ---------------------------------------------------------------------
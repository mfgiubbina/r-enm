# script description #
# function: occurences - taxonomic filter
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(fuzzyjoin)
library(lubridate)
library(stringdist)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/integrated"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ_data <- readr::read_csv("occ_terretrial_animal_integrated.csv")
occ_data

# confer
occ_data$name %>% 
  table %>% 
  tibble::as_tibble()

# preparate data ----------------------------------------------------------
# directory
setwd("/media/mude/data/gitlab/r-enm/data/01_occ/lista_especies")
dir()

# import gnr data
tax_data_filter <- readr::read_csv("terretrial_animal_gnr_tax.csv")
tax_data_filter

# taxonomic filter --------------------------------------------------------
# filter by gnr name - dist
occ_data_filter_gnr_dist <- fuzzyjoin::stringdist_inner_join(x = occ_data[, -2], 
                                                             y = tax_data_filter,
                                                             by = c(name = "matched_name"), 
                                                             max_dist = 2) %>% 
  dplyr::arrange(name) %>% 
  dplyr::select(name, species, everything(), -matched_name) %>% 
  dplyr::distinct()
occ_data_filter_gnr_dist

# filter by gnr name - exact
occ_data_filter_gnr_exact <- dplyr::inner_join(occ_data[, -2], tax_data_filter, c(name = "matched_name")) %>% 
  dplyr::arrange(name) %>% 
  dplyr::select(name, species, everything()) %>% 
  dplyr::distinct()
occ_data_filter_gnr_exact

# confer
occ_data$name %>% table
occ_data_filter_gnr_dist$name %>% table
occ_data_filter_gnr_exact$name %>% table

# export data -------------------------------------------------------------
# directory
setwd("..")
dir.create("filtered")
setwd("filtered")

# dist
readr::write_csv(occ_data_filter_gnr_dist,
                 paste0("occ_terretrial_animal_integrated_filter_gnr_dist.csv"))

# exact
readr::write_csv(occ_data_filter_gnr_exact,
                 paste0("occ_terretrial_animal_integrated_filter_gnr_exact.csv"))

# end ---------------------------------------------------------------------